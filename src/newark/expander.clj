(ns newark.expander
  (:refer-clojure :exclude [macroexpand macroexpand-1])
  (:require [newark.env :as env]
            [newark.syntax :as syntax]
            [newark.reader :as reader]
            [clojure.string :as str]))

(declare expand-symbol expand-list expand-array expand-function)

(defn syntax-error [msg form]
  (throw (RuntimeException.
          (str msg
               " " form
               " at " (-> form meta :position)))))

(defn baddef [form]
  (syntax-error "definition in expression context" form))

(defn maybe-resolve-to-function-macro [env form]
  (when (seq? form)
    (let [denotation (env/resolve-symbol env (first form))]
      (when (fn? denotation) denotation))))

(defn maybe-resolve-to-symbol-macro [env form]
  (when (symbol? form)
    (let [denotation (env/resolve-symbol env form)]
      (when (fn? denotation) denotation))))

(defn macroexpand-1 [e x]
  (if-let [macro (or (maybe-resolve-to-function-macro e x)
                     (maybe-resolve-to-symbol-macro e x))]
    (macro e x)
    x))

(defn macroexpand [e x]
  (let [x* (macroexpand-1 e x)]
    (if (= x x*)
      x*
      (recur e x*))))

(defn expand-form [env form]
  (let [x (macroexpand env form)]
    (cond
     (symbol? x)  (expand-symbol env x)
     (seq? x)     (expand-list env x)
     (vector? x)  (expand-array env x)
     (keyword? x) [:CONSTANT (name x)]
     :else        [:CONSTANT x])))

(defn expand-forms [env xs]
  (doall (map (fn [x] (expand-form env x)) xs)))

(defn resolves-to? [env form val]
  (and (seq? form) (= val (env/resolve-symbol env (first form)))))

(defn definition? [env form]
  (resolves-to? env form :DEFINE))

(defn include? [env form]
  (resolves-to? env form :INCLUDE))

(defn macro-definition? [env form]
  (resolves-to? env form :DEFINE_SYNTAX))

(defn symbol-macro-definition? [env form]
  (resolves-to? env form :DEFINE_SYMBOL_SYNTAX))

(defn begin? [env form]
  (resolves-to? env form :BEGIN))

(defn expand-body* [env forms expanded]  
  (if (empty? forms)
    expanded
    (let [[form & forms] forms
          form (macroexpand env form)]
      (cond
       (begin? env form)
       (recur env (concat (rest form) forms) expanded)

       (include? env form)
       (let [sexps (apply concat
                     (for [name (rest form)]
                       (reader/read-file (str name))))]
         (recur env (concat sexps forms) expanded))
       
       (definition? env form)
       (let [[_ sym expr] form
             loc          (env/make-var env sym)]
         (recur env
                forms
                (conj expanded [::DEFINITION loc expr])))

       (macro-definition? env form)
       (let [macro (syntax/make-syntax env (drop 2 form))]
         (env/bind-symbol env (second form) macro)
         (recur env forms expanded))

       (symbol-macro-definition? env form)
       (let [macro (syntax/make-symbol-syntax env (nth form 2))]
         (env/bind-symbol env (nth form 1) macro)
         (recur env forms expanded))
       
       :else
       (recur env forms (conj expanded [::EXPRESSION form]))))))

(defn reexpand-body* [env expanded reexpanded]
  (if (empty? expanded)
    (cons :BEGIN reexpanded)
    (let [[form & expanded] expanded]
      (case (first form)            
        ::DEFINITION
        (let [[_ loc expr] form
              expr* (expand-form env expr)]
          (recur env
                 expanded
                 (conj reexpanded [:SET loc expr*])))
        
        ::EXPRESSION
        (recur env
               expanded
               (conj reexpanded (expand-form env (second form))))))))

(defn expand-body [env forms]  
  (let [forms* (expand-body* env forms [])
        body   (reexpand-body* env forms* [])]
    body))

(defn dotted? [sym]
  (re-matches #"^[^\.]+(\.[^\.]+)+" (str sym)))

(defn expand-dotted-symbol [e x]
  (let [[root & segs] (str/split (str x) #"\.")
        root (expand-symbol e (with-meta (symbol nil root) (meta x)))]
    (loop [root root segs segs]
      (if (empty? segs)
        root
        (recur [:PROJECT root [:CONSTANT (first segs)]]
               (rest segs))))))

(defn expand-symbol [e x]
  (let [den (env/resolve-symbol e x)]
    (cond
     den         den
     (dotted? x) (expand-dotted-symbol e x)
     :else
     (do (println (str "[WARNING] use of unbound symbol: " x))
         (env/make-global e x)))))

(defn make-params [env params index]
  (when-let [[p & ps] (seq params)]
    (let [arg [:ARG (:level env) index]]
      (env/bind-symbol env p arg)
      (cons arg (make-params env ps (inc index))))))

(defn parse-params [params]
  (let [params (vec params)
        len    (count params)
        last-p (last params)]
    (if (= last-p '...)
      [(subvec params 0 (- len 2)) (nth params (- len 2))]
      [params nil])))

(defn expand-function [env this args params body]
  (let [env            (env/extend-scope env)
        [params &rest] (parse-params params)
        &rest          (when &rest (env/make-local env &rest))
        this           (when this  (env/make-local env this))
        args           (when (or args &rest) (env/make-local env args))
        params         (make-params env params 0)
        body           (expand-body env body)
        body (if &rest [:BEGIN [:&REST &rest args (count params)] body] body)
        body (if args  [:BEGIN [:SET args [:RAW "arguments"]] body] body)
        body (if this  [:BEGIN [:SET this [:RAW "this"]] body] body)]
    
    [:FN params (:level env) ((:next-local env) :get) body]))

(defn expand-let [env bindings body]
  (if (empty? bindings)
    (expand-body env body)
    (let [[[sym expr] & more] bindings
          expr* (expand-form env expr)
          env* (env/extend-symbols env)
          sym* (env/make-local env* sym)]
      [:BEGIN [:SET sym* expr*] (expand-let env* more body)])))

(defn expand-array [env array]
  `(:ARRAY ~(expand-forms env array)))

(defn front-dotted? [s]
  (re-matches #"^\.[^\.]+$" (str s)))

(defn maybe-resolve-to-operator [env form]
  (let [denotation (env/resolve-symbol env form)]
    (and (vector? denotation)
         (= (first denotation) :OP)
         denotation)))

(defn expand-dot-call [env head tail]
  [:CALL
   [:PROJECT
    (expand-form env (first tail))
    [:CONSTANT (apply str (rest (str head)))]]    
   (expand-forms env (rest tail))])

(defn operator? [x]
  (and (vector? x)
       (= :OP (first x))))

(defn stepmap [f xs]
  (if (> (count xs) 1)
      (cons (f (take 2 xs)) (stepmap f (rest xs)))
      '()))

(defn expand-logical-operator [op args]
  (case (count args)
    0 (throw (RuntimeException. (str op " requires at least one argument")))
    1 true
    2 [:OPCALL op args]
    (let [steps (stepmap (fn [args] [:OPCALL op args]) args)]
      [:OPCALL "&&" steps])))

(defn expand-opcall [[_ type op] args]
  (case type
    :FOLD   [:OPCALL op args]
    :BINARY [:OPCALL op args]    
    :UNARY  [:OPCALL op args]
    :LOGIC  (expand-logical-operator op args)))

(defn expand-call* [env head tail]
  (let [op   (expand-form env head)
        args (expand-forms env tail)]
    (if (operator? op)
      (expand-opcall op args)
      [:CALL op args])))

(defn expand-call [env head tail]
  (if (front-dotted? head)
    (expand-dot-call env head tail)
    (expand-call* env head tail)))

(defn expand-for-each-property [env [[property object] & body]]
  (let [obj  (expand-form env object)
        env* (env/extend-labels env)
        env* (env/extend-symbols env)
        loc  (env/make-label env nil)        
        prop (env/make-local env* property)
        body (expand-body env* body)]
    [:DO_PROPERTIES loc prop obj body]))

(defn expand-let-syntax [env bindings body]
  (if (empty? bindings)
    (expand-body env body)
    (let [[name & stx] (first bindings)
          env*         (env/extend-symbols env)
          macro        (syntax/make-syntax env* stx)]
      (env/bind-symbol env* name macro)
      (recur env* (rest bindings) body))))

(defn expand-let-symbol-syntax [env bindings body]
  (if (empty? bindings)
    (expand-body env body)
    (let [[name form] (first bindings)
          env*        (env/extend-symbols env)
          macro       (syntax/make-symbol-syntax env* form)]
      (env/bind-symbol env* name macro)
      (recur env* (rest bindings) body))))

(defn expand-quote [form]
  (cond
   (seq? form)    [:ARRAY (vec (map expand-quote form))]
   (vector? form) [:ARRAY (vec (map expand-quote form))]
   (symbol? form) [:CONSTANT (name form)]
   :else          [:CONSTANT form]))

(defn expand-set [env exprs]
  (let [n (count exprs)]
    (if (= n 2)
     (let [[a b] exprs]
       [:SET (expand-form env a) (expand-form env b)])
     (let [obj-loc (env/next-local env)
           obj-expr (expand-form env (first exprs))]
       (loop [exprs (rest exprs)
              vals  []]
         (if (empty? exprs)
           `[:BEGIN
             [:SET ~obj-loc ~obj-expr]
             ~@vals]
           (let [[a b & more] exprs
                 a (expand-form env a)
                 b (expand-form env b)]
             (recur more (conj vals [:SET [:PROJECT obj-loc a] b])))))))))

(defn expand-list [env form]
  (let [[head & tail] form]
    (case (and (symbol? head) (env/resolve-symbol env head))
      :DEFINE
      (baddef form)
      
      :DEFINE_SYNTAX
      (baddef form)

      :LET_SYNTAX
      (expand-let-syntax env (first tail) (rest tail))

      :LET_SYMBOL_SYNTAX
      (expand-let-symbol-syntax env (first tail) (rest tail))
      
      :BEGIN
      (let [env* (env/extend-symbols env)]
        (expand-body env* tail))
      
      :SET
      (expand-set env tail)
      
      :PROJECT
      (vec (cons :PROJECT (expand-forms env tail)))

      :QUOTE
      (expand-quote (first tail))
      
      :LET
      (expand-let env (first tail) (rest tail))
      
      :FN
      (let [[a b c & d] tail]
        (expand-function env a b c d))
      
      :IF
      (let [[a b c] tail]
        (vec (cons :IF (expand-forms env [a b c]))))

      :BLOCK
      (let [env*  (env/extend-labels env)
            label (env/make-label env (first tail))]
        [:BLOCK label (expand-body env (rest tail))])
      
      :LOOP
      (let [env   (env/extend-labels env)
            label (env/make-label env (first tail))]
        [:LOOP label (expand-body env (rest tail))])

      :RETURN_FROM
      [:RETURN_FROM
       (env/resolve-label env (first tail))
       (expand-form env (second tail))]
      
      :NEW
      (let [call (expand-call env (first tail) (rest tail))]
        [:NEW call])

      :FOR_EACH_PROPERTY
      (expand-for-each-property env tail)

      :THROW
      (let [err (expand-form env (first tail))]
        [:THROW err])
      
      ;; else
      (expand-call env head tail))))