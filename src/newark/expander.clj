(ns newark.expander
  (:refer-clojure :exclude [macroexpand macroexpand-1])
  (:require [newark.env :as env]
            [newark.syntax :as syntax]
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
    (let [denotation (env/resolve env (first form))]
      (when (fn? denotation) denotation))))

(defn maybe-resolve-to-symbol-macro [env form]
  (when (symbol? form)
    (let [denotation (env/resolve env form)]
      (when (fn? denotation) denotation))))

(defn macroexpand-1 [e x]
  (if-let [macro (or (maybe-resolve-to-function-macro e x)
                     (maybe-resolve-to-symbol-macro e x))]
    (macro x)
    x))

(defn macroexpand [e x]
  (let [x* (macroexpand-1 e x)]
    (if (= x x*)
      x*
      (recur e x*))))

(defn expand-form [env form]
  (let [x (macroexpand env form)]
    (cond
     (symbol? x) (expand-symbol env x)
     (seq? x)    (expand-list env x)
     (vector? x) (expand-array env x)
     :else       [:CONSTANT x])))

(defn expand-forms [env xs]
  (doall (map (fn [x] (expand-form env x)) xs)))

(defn resolves-to? [env form val]
  (and (seq? form) (= val (env/resolve env (first form)))))

(defn definition? [env form]
  (resolves-to? env form :DEFINE))

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
       
       (definition? env form)
       (let [tail (rest form)]
         (if (seq? (first tail))
           (let [[name & params] (first tail)
                 loc           (env/defvar env name)
                 body          (rest tail)]
             (recur env
                    forms
                    (conj expanded [::FUNCTION_DEFINITION loc params body])))
           (let [name (first tail)
                 loc  (env/defvar env name)
                 expr (second tail)]
             (recur env
                    forms
                    (conj expanded [::DEFINITION loc expr])))))

       (macro-definition? env form)
       (let [macro (syntax/make-syntax env (drop 2 form))]
         (env/bind env (second form) macro)
         (recur env forms expanded))

       (symbol-macro-definition? env form)
       (let [macro (syntax/make-symbol-syntax env (nth form 2))]
         (env/bind env (nth form 1) macro)
         (recur env forms expanded))
       
       :else
       (recur env forms (conj expanded [::EXPRESSION form]))))))

(defn reexpand-body* [env expanded reexpanded]
  (if (empty? expanded)
    (cons :BEGIN reexpanded)
    (let [[form & expanded] expanded]
      (case (first form)
        ::FUNCTION_DEFINITION
        (let [[_ loc params body] form
              func (expand-function env params body)]
          (recur env
                 expanded
                 (conj reexpanded [:DEF loc func])))
            
        ::DEFINITION
        (let [[_ loc expr] form
              expr* (expand-form env expr)]
          (recur env
                 expanded
                 (conj reexpanded [:DEF loc expr*])))
        
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
  (let [den (env/resolve e x)]
    (cond
     den den
     (dotted? x) (expand-dotted-symbol e x)
     :else       (env/defvar (env/get-environment-root e) x))))

(defn parse-params [params]
  (let [[pparams kwparams] (split-with symbol? params)       
        {:keys [this arguments]} (apply hash-map kwparams)]
    [pparams this arguments]))

(defn expand-function [env params body]
  (let [env*    (env/extend-environment env)
        [a b c] (parse-params params)
        params* (doall (map #(env/defvar env* %) a))
        b       (when b (env/defvar env* b))
        c       (when c (env/defvar env* c))
        body*   (expand-body env* body)
        body*   (if b `(:BEGIN (:DEF ~b (:RAW "this")) ~body*) body*)
        body*   (if c `(:BEGIN (:DEF ~c (:RAW "arguments") ~body*)) body*)]
    `(:FN ~params* ~body*)))

(defn expand-let [env bindings body]
  (let [exprs (doall (for [[_ expr] bindings] (expand-form env expr)))
        env*  (env/extend-environment env)
        locs  (doall (for [[sym _] bindings] (env/defvar env* sym)))
        defs  (doall (for [[x y] (map list locs exprs)] [:DEF x y]))
        body  (expand-body env* body)]
    `[:BEGIN ~@defs ~body]))

(defn expand-array [env array]
  `(:ARRAY ~(expand-forms env array)))

(defn front-dotted? [s]
  (re-matches #"^\.[^\.]+$" (str s)))

(defn maybe-resolve-to-operator [env form]
  (let [denotation (env/resolve env form)]
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
        env* (env/extend-environment env)
        prop (env/defvar env* property)
        body (expand-body env* body)]
    [:BEGIN
     [:DEF prop [:CONSTANT nil]]
     [:FOR_EACH_PROPERTY prop obj body]]))

(defn expand-list [env form]
  (let [[head & tail] form]
    (case (env/resolve env head)
      :DEFINE
      (baddef form)
      
      :DEFINE_SYNTAX
      (baddef form)

      :BEGIN
      (let [env* (env/extend-environment env)]
        (expand-body env* tail))
      
      :SET!
      (cons :SET! (expand-forms env tail))
      
      :PROJECT
      (cons :PROJECT (expand-forms env tail))

      :LET
      (expand-let env (first tail) (rest tail))
      
      :FN
      (expand-function env (first tail) (rest tail))
      
      :IF
      (cons :IF (expand-forms env tail))
      
      :WHILE
      (cons :WHILE (expand-forms env tail))
      
      :NEW
      (cons :NEW (expand-forms env tail))

      :FOR_EACH_PROPERTY
      (expand-for-each-property env tail)
      
      ;; else
      (expand-call env head tail))))

