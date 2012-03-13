(ns newark.compiler
  (:refer-clojure :exclude [compile extend])
  (:use [newark.util :only [make-dictionary
                            extend-dictionary
                            dictionary-get
                            dictionary-set]]))

;; simple back-end
;; translate the expanded ast
;; into purely imperative form

(declare norm norm* norm** norm-list norm-let
         simplify simplify*
         serialize serialize-let serialize-letrec serialize-fn)

(defn tracer-for [x]
  (fn [y] [:SET! x y]))

(defn norm [x]
  (cond
   (seq? x)    (norm-list x)
   (vector? x) (norm* x)
   (symbol? x) (if (namespace x)
                 [:GLOBAL (namespace x) (name x)]
                 [:VAR x])
   :else       [:VAL x]))

(defn norm* [xs]
  (vec (map norm xs)))

(defn norm** [xs]
  (vec (map norm* xs)))

(defn norm-list [[x & [a b c d e :as xs]]]
  (case (keyword x)
    :core/raw         [:RAW a]
    :core/if          [:IF (norm a) (norm b) (norm c)]
    :core/set!        [:SET!  (norm a) (norm b) (norm c)]
    :core/.           [:FIELD (norm a) (norm b)]
    :core/throw       [:THROW (norm a)]
    :core/begin       [:BEGIN (norm* xs)]
    :core/let         [:LET (norm** a) (norm b)]    
    :core/letrec      [:LETREC (norm** a) (norm b)]
    :core/block       [:BLOCK [:LABEL a] (norm b)]
    :core/loop        [:LOOP  [:LABEL a] (norm b)]
    :core/return-from [:RETURN_FROM [:LABEL a] (norm b)]
    
    :core/fn          [:FN
                       (when a (norm a))
                       (when b (norm b))
                       (norm* c)
                       (norm d)]

    :core/try         [:TRY (norm a)]
    :core/catch       [:CATCH (norm a) (norm b)]
    :core/finally     [:FINALLY (norm a)]
    
    :core/new         [:NEW (norm a) (norm* (rest xs))]
                      [:CALL (norm x) (norm* xs)]))

(def ^:dynamic *scope* nil)

(defn make-scope [& {:keys [level env]}]
  {:env        (or env (make-dictionary))
   :block      (atom [])
   :level      (or level 0)
   :num-locals (atom 0)
   :num-labels (atom 0)})

(defn lookup [x & [scope]]
  (dictionary-get
   (:env (or scope *scope*))
   x))

(defn make-local [& [scope]]  
  (let [scope (or scope *scope*)
        id    @(:num-locals scope)]
    (swap! (:num-locals scope) inc)
    [:LOCAL (:level scope) id]))

(defn make-label [& [scope]]  
  (let [scope (or scope *scope*)
        id    @(:num-labels scope)]
    (swap! (:num-labels scope) inc)
    [:LABEL (:level scope) id]))

(defn bind [x y & [scope]]
  (let [scope (or scope *scope*)]
    (dictionary-set (:env scope) x y)
    y))

(defn rename-local [sym & [scope]]  
  (let [scope (or scope *scope*)
        local (make-local scope)]
    (bind sym local scope)))

(defn rename-label [sym & [scope]]  
  (let [scope (or scope *scope*)
        label (make-label scope)]
    (bind sym label scope)))

(defn rename-args [args & [scope]]
  (let [scope (or scope *scope*)]
    (doall
     (for [[i a] (map vector (range) args)]
       (let [a* [:ARG (:level scope) i]]
         (dictionary-set (:env scope) a a*)
         a*)))))

(defn extend-scope [scope]
  (make-scope
   :env   (extend-dictionary (:env scope))
   :level (inc (:level scope))))

(defn finalize-scope [& [scope]]
  (let [scope (or scope *scope*)
        n     @(:num-locals scope)]
    (when (> n 0)
      (let [block  (:block scope)
            locals (for [i (range n)] [:LOCAL (:level scope) n])]
        (reset! block
                (apply conj [:DECLARE locals] @block))))))

(defn extend-env [scope]
  (update-in scope [:env] extend-dictionary))

(defn extend-block [scope]
  (assoc scope :block (atom [])))

(defn push [x & [scope]]
  (let [scope (or scope *scope*)]
    (swap! (:block scope) conj x)))

(defn trace [x t & [scope]]
  (let [scope (or scope *scope*)
        x     (if t (t x) x)]
    (push x scope)))

(defn maybe-trace [x t & [scope]]
  (when t (trace x t scope)))

(defmacro with-scope [scope & body]
  `(binding [*scope* (or ~scope (make-scope))]
     (do ~@body)))

(defmacro with-env [& body]
  `(with-scope (extend-env *scope*) ~@body))

(defmacro with-block [& body]
  `(let [scope# (or *scope* (make-scope))]
     (binding [*scope* (assoc scope# :block (atom []))]
       (do ~@body)
       @(:block *scope*))))

(defn serialize-begin [exprs tracer]
  (if (empty? (rest exprs))
    (serialize (first exprs) tracer)
    (do (serialize (first exprs) nil)
        (recur (rest exprs) tracer))))

(defn serialize-let-bindings [locals exprs]
  (when (seq locals)
    (let [[x & locals] locals
          [y & exprs]  exprs]
      (serialize y (tracer-for x))
      (recur locals exprs))))

(defn serialize-let-body [locals vars body tracer]
  (with-env
    (doall (map bind vars locals))
    (serialize body tracer)))

(defn serialize-let [bindings body tracer]
  (let [vars   (map first bindings)
        exprs  (map second bindings)
        locals (doall (for [_ bindings] (make-local)))]
    (serialize-let-bindings locals exprs)
    (serialize-let-body locals vars body tracer)))

(defn serialize-letrec [bindings body tracer]
  (with-env
    (let [vars   (map first bindings)
          exprs  (map second bindings)
          locals (doall (for [v vars] (bind v (make-local))))]
      (doseq [[x y] (map list locals exprs)]
        (serialize y (tracer-for x)))
      (serialize body tracer))))

(defn serialize-block
  "serializes a term into a fresh block and returns the block"
  [x t]
  (with-block (serialize x t)))

(defn serialize-labeled-block
  "serializes a labeled control-structure (block or loop)
   lazily allocates a local sentinel var to determine whether
   or not a non-local exit has occurred"
  [tag label body tracer]
  (with-env
    (let [scope    *scope*
          label    (rename-label label)
          sentinel (atom nil) 
          getter     (fn []
                       (when (not @sentinel)
                         (reset! sentinel (make-local scope)))
                       @sentinel)
          _        (bind label [getter tracer] scope)
          body     (with-block (serialize body tracer))]
      (push [tag label @sentinel body]))))

(defn serialize-return-from
  [label expr]
  (let [[_ level _ :as label]    (lookup label)
        [sentinel-getter tracer] (lookup label)]
    (if (< level (:level *scope*))
      (let [sentinel (sentinel-getter)]
        [:IF sentinel
         (with-block
           (serialize expr tracer)
           (push [:SET! sentinel [:VAL false]])
           (push [:NON_LOCAL_EXIT]))
         [[:NON_LOCAL_EXIT_ERROR]]])
      (do (serialize expr tracer)
          (push [:BREAK label])))))

(defn serialize-fn
  [this args pargs body tracer]
  (with-scope
    (extend-scope *scope*)
    (let [this   (when this (make-local))
          args   (when args (make-local))
          pargs  (rename-args pargs)
          return (make-local)]
      (when this (push [:SET! this [:THIS]]))
      (when args (push [:SET! args [:ARGUMENTS]]))
      (serialize-block body (tracer-for return))
      (push [:RETURN return])
      (finalize-scope)
      (trace [:FN pargs (:block *scope*)] tracer))))

(defn simplify* [xs]
  (doall (map simplify xs)))

(defn simplify [[tag a b c d :as x]]
  (case tag
    :GLOBAL x
    :VAL    x
    :RAW    x
    :ARG    (lookup x)
    :VAR    (lookup x)
    :SET!   (do (serialize x nil)
                (simplify a))
    (let [local (make-local)]
      (serialize x (tracer-for local))
      local)))

(defn serialize [[tag a b c d :as x] t]
  (case tag
    :GLOBAL (maybe-trace x t)
    :VAL    (maybe-trace x t)
    :RAW    (maybe-trace x t)
    :VAR    (maybe-trace (lookup x) t)

    :IF     (let [a (simplify a)
                  b (serialize-block b t)
                  c (serialize-block c t)]
              (push [:IF a b c]))

    :SET!   (serialize b (tracer-for a))
    
    :BEGIN  (serialize-begin a t)
    :LET    (serialize-let a b t) 
    :LETREC (serialize-letrec a b t)
    
    :BLOCK  (serialize-labeled-block :BLOCK a b t)
    :LOOP   (serialize-labeled-block :LOOP a b t)
    
    :RETURN_FROM (serialize-return-from a b)

    :FN (serialize-fn a b c d t)

    :NEW  (trace [:NEW  (simplify a) (simplify* b)] t)
    :CALL (trace [:CALL (simplify a) (simplify* b)] t)))