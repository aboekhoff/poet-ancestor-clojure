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
         serialize serialize-let serialize-letrec serialize-fn
         simplify-to-atom)

(defn tracer-for [x]
  (fn [y] [:SET! x y]))

(defn norm [x]
  (cond
   (seq? x)     (norm-list x)
   (vector? x)  [:ARRAY (norm* x)]
   (symbol? x)  (if (namespace x)
                  [:GLOBAL (namespace x) (name x)]
                  [:VAR x])
   (keyword? x) [:KEYWORD (.substring (str x) 1)]
   :else        [:VAL x]))

(defn norm* [xs]
  (vec (map norm xs)))

(defn norm** [xs]
  (vec (map norm* xs)))

(defn dot? [x]
  (or (= x '.)
      (= x 'core/.)))

(defn norm-args [pargs]
  (let [[pargs &rest] (split-with (comp not dot?) pargs)
        &rest         (when (seq &rest) (second &rest))]
    [(norm* pargs) (when &rest (norm &rest))]))

(def math
  {'prelude/+             '+
   'prelude/*             '*
   'prelude/-             '-
   (symbol "prelude" "/") '/
   'prelude/mod           '%
   'prelude/div           'div})

(def logic
  '{prelude/< <
    prelude/> >
    prelude/<= <=
    prelude/>= >=
    prelude/=  ===
    prelude/nil? nil?})

(def bits
  {'prelude/bit-and          '&
   'prelude/bit-or           '|
   'prelude/bit-xor          (symbol "^")
   'prelude/bit-not          (symbol "~")
   'prelude/bit-shift-left   '<<
   'prelude/bit-shift-right  '>>
   'prelude/bit-shift-right* '>>})

(def js  
  {'prelude/has-property?    'in
   'prelude/instanceof       'instanceof
   'prelude/typeof           'typeof
   'prelude/delete-property! 'delete})

(defn get-op [x]
  (when (and (symbol? x)
             (= (namespace x) "prelude"))
    (or (get math x)
        (get logic x)
        (get bits x)
        (get js x))))

(defn norm-math [op args zero one]
  (case (count args)
    0 (or zero (throw (RuntimeException. (str "operator: " op " requires at least one argument"))))
    1 one
    [:OPCALL op args]))

(defn cascade [xs]
  (let [[a b] xs]
    (when (and a b)
      (cons (list a b) (cascade (rest xs))))))

(defn norm-logic [op args]
  (case (count args)
    0 (throw (RuntimeException. (str "operator: " op " requires at least one argument")))
    1 [:VAL true]
    2 [:OPCALL (str op) args]
    ;else
    [:OPCALL "&&" (map (fn [[x y]] [:OPCALL op [x y]]) (cascade args))]))

(defn norm-quote [x]
  (cond
   (symbol? x)
   (if (namespace x)
     [:CALL
      [:GLOBAL "prelude" "make-qualified-symbol"]
      [[:VAL (namespace x)] [:VAL (name x)]]]
     [:CALL
      [:GLOBAL "prelude" "make-symbol"]
      [[:VAL (name x)]]])

   (seq? x)
   [:CALL [:GLOBAL "prelude" "list"]
    (vec (map norm-quote x))]

   (vector? x)
   [:ARRAY (vec (map norm-quote x))]

   :else
   (norm x)))

(defn norm-op [op args]
  (let [op (str op)]
    (case op
      "+"   (norm-math op args [:VAL 0] (first args))
      "*"   (norm-math op args [:VAL 1] (first args))
      "-"   (norm-math op args nil [:OPCALL "-" [(first args)]])
      "/"   (norm-math op args nil [:OPCALL "/" [(first args) [:VAL 1]]])
      "%"   [:OPCALL "%" args]
      "div" [:OPCALL "~" [[:OPCALL "~" [(norm-op '/ args)]]]]

      "<"   (norm-logic op args)
      ">"   (norm-logic op args)
      "<="  (norm-logic op args)
      ">="  (norm-logic op args)
      "===" (norm-logic op args)

      "nil?" [:OPCALL "==" [(first args) [:VAL nil]]]

      [:OPCALL op args])))

(defn norm-call [callee args]
  (if-let [op (get-op callee)]
    (norm-op op (norm* args))
    [:CALL (norm callee) (norm* args)]))

(defn norm-unwind-protect [clauses]
  (let [clauses  (into {} (for [[k & x] clauses] [k x]))
        try*     (first (:try clauses))
        catch*   (:catch clauses)
        finally* (first (:finally clauses))]
    [:UNWIND_PROTECT
     (when try* (norm try*))
     (when (first catch*) [(norm (first catch*)) (norm (second catch*))])
     (when finally* (norm finally*))]))

(defn norm-do-properties [l [v x] body]
  [:DO_PROPERTIES [:LABEL l] (norm v) (norm x) (norm body)])

(defn norm-list [[x & [a b c d e :as xs]]]
  (case (keyword x)
    :core/raw         [:RAW a]
    :core/if          [:IF (norm a) (norm b) (norm c)]
    :core/set!        [:SET!  (norm a) (norm b)]
    :core/.           [:FIELD (norm a) (norm b)]
    :core/throw       [:THROW (norm a)]
    :core/begin       [:BEGIN (norm* xs)]
    :core/let         [:LET (norm** a) (norm b)]    
    :core/letrec      [:LETREC (norm** a) (norm b)]
    :core/block       [:BLOCK [:LABEL a] (norm b)]
    :core/loop*       [:LOOP  [:LABEL a] (norm b)]
    :core/return-from [:RETURN_FROM [:LABEL a] (norm b)]
    
    :core/fn          (let [[args &rest] (norm-args a)]
                        [:FN [args nil &rest] (norm b)])

    :core/method      (let [[this & args]      a
                            this               (norm this)
                            [args &rest]       (norm-args args)]
                        [:FN [args this &rest] (norm b)])
    
    :core/new         [:NEW (norm a) (norm* (rest xs))]
    :core/quote       (norm-quote a)

    :core/unwind-protect
    (norm-unwind-protect xs)

    :core/do-properties*
    (norm-do-properties a b c)
    ;; else
    (norm-call x xs)))

(def ^:dynamic *scope* nil)

(def ^:dynamic *root-scope* nil)

(defn make-scope [& {:keys [level env keywords]}]
  {:env        (or env (make-dictionary))
   :block      (atom [])
   :level      (or level 0)
   :keywords   (or keywords (atom {}))
   :num-locals (atom 0)
   :num-labels (atom 0)
   :num-errors (atom 0)})

(defn lookup [x & [scope]]
  (dictionary-get
   (:env (or scope *scope*))
   x))

(defn make-local [& [scope]]  
  (let [scope (or scope *scope*)
        id    @(:num-locals scope)]
    (swap! (:num-locals scope) inc)
    [:LOCAL (:level scope) id]))

(defn make-keyword [kwd]
  (let [scope *root-scope*]
    (or (get @(:keywords scope) kwd)
        (let [kwd* (make-local scope)]
          (swap! (:keywords scope) assoc kwd kwd*)
          kwd*))))

(defn make-label [& [scope]]  
  (let [scope (or scope *scope*)
        id    @(:num-labels scope)]
    (swap! (:num-labels scope) inc)
    [:LABEL (:level scope) id]))

;; used to create unique vars for catch (e) 

(defn make-error [& [scope]]
  (let [scope (or scope *scope*)
        id    @(:num-errors scope)]
    (swap! (:num-errors scope) inc)
    [:ERROR (:level scope) id]))

(defn bind [x y & [scope]]
  (let [scope (or scope *scope*)]
    (dictionary-set (:env scope) x y)
    y))

(defn rename-local [sym & [scope]]  
  (let [scope (or scope *scope*)
        local (make-local scope)]
    (bind sym local scope)
    local))

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
   :env      (extend-dictionary (:env scope))
   :level    (inc (:level scope))
   :keywords (:keywords scope)))

(defn gen-declaration [scope]
  (let [n @(:num-locals scope)]
    (when (> n 0)
      [:DECLARE
       (vec (for [i (range n)] [:LOCAL (:level scope) i]))])))

(defn finalize-scope [& [scope]]
  (let [scope (or scope *scope*)
        block (:block scope)]
    (when-let [decl (gen-declaration scope)]
      (reset! block (apply conj [decl] @block)))))

(defn finalize-toplevel []
  (let [scope *root-scope*
        
        sets   (for [[k v] @(:keywords scope)]
                [:SET! v [:CALL [:GLOBAL "prelude" "make-keyword"]
                          [[:VAL (name k)]]]])
       
        decl   (gen-declaration scope)
        block  (:block scope)
        base   (if decl [decl] [])
        base   (if (seq sets) (apply conj base sets) base)
        base   (apply conj base @block)]
    (reset! (:block scope) base)))

(defn extend-env [scope]
  (assoc scope :env (extend-dictionary (:env scope))))

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
  (cond
   (empty? exprs)        (serialize [:VAL nil] tracer)
   (empty? (rest exprs)) (serialize (first exprs) tracer)
   :else
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

(defn serialize-labeled-block*
  "serializes a labeled control-structure (block or loop)
   lazily allocates a local sentinel var to determine whether
   or not a non-local exit has occurred"
  [label body tracer]
  (with-env
    (let [scope    *scope*
          label    (rename-label label)
          sentinel (atom nil) 
          getter     (fn []
                       (when (not @sentinel)
                         (reset! sentinel (make-local scope)))
                       @sentinel)
          _        (bind label [getter tracer] scope)
          body     (with-block (serialize body tracer))
          body     (if @sentinel
                     (apply conj [[:SET! @sentinel [:VAL true]]] body)
                     body)]
      [label @sentinel body])))

(defn serialize-labeled-block
  [tag label body tracer]
  (let [[label sentinel body] (serialize-labeled-block* label body tracer)]
    (push [tag label sentinel body])))

(defn serialize-return-from
  [label expr]
  (let [[_ level _ :as label]    (lookup label)
        [sentinel-getter tracer] (lookup label)]
    (if (< level (:level *scope*))
      (let [sentinel (sentinel-getter)]
        (push
         [:IF sentinel
          (with-block
            (serialize expr tracer)
            (push [:SET! sentinel [:VAL false]]))
          []])
        (push [:NON_LOCAL_EXIT]))
      (do (serialize expr tracer)
          (push [:BREAK label])))))

(defn serialize-do-properties [label prop obj body tracer]
  (let [obj (simplify obj)]
    (with-env
      (let [prop (bind prop (make-local))
            [label sentinel body] (serialize-labeled-block* label body tracer)]
        (push [:DO_PROPERTIES label sentinel prop obj body])))))

(defn serialize-fn*
  [[args this &rest] body]
  (with-scope
    (extend-scope *scope*)
    (when this
      (let [this (rename-local this)]
        (push [:SET! this [:THIS]])))
    (when &rest
      (let [&rest (rename-local &rest)]
        (push [:&REST &rest (count args)])))
    (let [args   (rename-args args)
          return (make-local)]
      (serialize body (tracer-for return))
      (push [:RETURN return])
      (finalize-scope)
      [:FN args @(:block *scope*)])))

(defn serialize-fn
  [a b t]
  (maybe-trace (serialize-fn* a b) t))

(defn simplify* [xs]
  (doall (map simplify xs)))

(defn simplify [[tag a b c d :as x]]
  (case tag
    :FIELD  [:FIELD (simplify a) (simplify b)]
    :ARRAY  [:ARRAY (simplify* a)]
    (simplify-to-atom x)))

(defn simplify-to-atom [[tag a b c d :as x]]
    (case tag
      :GLOBAL  x
      :LOCAL   x
      :VAL     x
      :RAW     x
      :KEYWORD (make-keyword a)
      :VAR     (lookup x)
      :SET!    (do (serialize x nil)
                   (simplify a))
      (let [local (make-local)]
        (serialize x (tracer-for local))
        local)))

(defn serialize-unwind-protect
  [try-block catch-block finally-block tracer]
  (let [try-block
        (when try-block
          (serialize-block try-block tracer))
        
        catch-block
        (when-let [[err block] catch-block]
          (with-env
            (let [v (bind err (make-error))
                  b (serialize-block block nil)]
              [v b])))

        finally-block
        (when finally-block
          (serialize-block finally-block nil))]
    (push [:UNWIND_PROTECT try-block catch-block finally-block])))

(defn serialize [[tag a b c d :as x] t]
  (case tag
    :GLOBAL  (maybe-trace x t)
    :VAL     (maybe-trace x t)
    :RAW     (maybe-trace x t)
    :VAR     (maybe-trace (lookup x) t)
    :KEYWORD (maybe-trace (make-keyword a) t)
    :ARRAY   (maybe-trace [:ARRAY (simplify* a)] t)
    
    :IF     (let [a (simplify-to-atom a)
                  b (serialize-block b t)
                  c (serialize-block c t)]
              (push [:IF a b c]))

    :SET!   (serialize b (tracer-for (simplify a)))
    :FIELD  (maybe-trace [:FIELD (simplify a) (simplify b)] t)
    :BEGIN  (serialize-begin a t)
    :LET    (serialize-let a b t) 
    :LETREC (serialize-letrec a b t)
    
    :BLOCK  (serialize-labeled-block :BLOCK a b t)
    :LOOP   (serialize-labeled-block :LOOP a b t)
    
    :RETURN_FROM (serialize-return-from a b)

    :FN (serialize-fn a b t)

    :NEW    (trace [:NEW  (simplify a) (simplify* b)] t)
    :CALL   (trace [:CALL (simplify a) (simplify* b)] t)
    :OPCALL (trace [:OPCALL a (simplify* b)] t)
    :THROW  (push [:THROW (simplify a)])
    
    :UNWIND_PROTECT
    (serialize-unwind-protect a b c t)

    :DO_PROPERTIES
    (serialize-do-properties a b c d t)))

(defn compile-toplevel [x]
  (let [x (norm x)]
    ;;(println "NORMALIZE:")
    ;;(pprint x
    (let [root-scope (make-scope)]
      (binding [*scope*      root-scope
                *root-scope* root-scope]
        (let [return (make-local)]
          (serialize x (tracer-for return))
          (push [:RETURN return])
          (finalize-toplevel)
          [:CALL [:FN [] @(:block *root-scope*)] []])))))