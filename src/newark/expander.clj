(ns newark.expander
  (:refer-clojure :exclude [macroexpand macroexpand-1])
  (:require [newark.env :as env]
            [clojure.string :as str]))

(defn syntax-error [msg form]
  (throw (RuntimeException.
          (str msg
               " " form
               " at " (-> form meta :position)))))

(defn baddef [form]
  (syntax-error "definition in expression context" form))

(defn macroexpand-1 [e x] x)

(defn macroexpand [e x]
  (let [x* (macroexpand-1 e x)]
    (if (= x x*)
      x*
      (recur e x*))))

(declare expand-symbol expand-list expand-array)

(defn expand-form [env form]
  (let [x (macroexpand env form)]
    (cond
     (symbol? x) (expand-symbol env x)
     (seq? x)    (expand-list env x)
     (vector? x) (expand-array env x)
     :else       `(:CONSTANT ~x))))

(defn expand-forms [env xs]
  (doall (map (fn [x] (expand-form env x)) xs)))

(defn definition? [env form]
  (and (seq? form) (= :DEFINE (env/resolve env (first form)))))

(defn begin? [env form]
  (and (seq? form) (= :BEGIN (env/resolve env (first form)))))

(defn expand-body* [env forms expanded]
  (if (empty? forms)
    expanded
    (let [[form & forms] forms]
      (cond
       (begin? env form)
       (recur env (concat (rest form) forms) expanded)
       
       (definition? env form)
       (let [[_ name expr] form
             loc (env/defvar env name)]
         (recur env forms (conj expanded [::DEFINITION loc expr])))

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
                 (conj reexpanded `(:DEF ~loc ~expr*))))
        
        ::EXPRESSION
        (recur env
               expanded
               (conj reexpanded (expand-form env (second form))))))))

(defn expand-body [env forms]  
  (let [env*   (env/extend-environment env)
        forms* (expand-body* env* forms [])]    
    (reexpand-body* env* forms* [])))

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
        b       (when b (env/defvar env b))
        c       (when c (env/defvar env c))
        body*   (expand-body env body)
        body*   (if b `(:BEGIN (:DEF ~b (:RAW "this")) ~body*) body*)
        body*   (if c `(:BEGIN (:DEF ~c (:RAW "arguments") ~body*)) body*)]
    `(:FN ~params* ~body*)))

(defn expand-let [env bindings body]
  (if (empty? bindings)
    (expand-body env body)
    (let [env* (env/extend-environment env)
          loc  (env/defvar env (first bindings))
          expr (expand-form env* (second bindings))]
      `(:BEGIN (:DEF ~loc ~expr)
               ~(expand-let env* (drop 2 bindings) body)))))

(defn expand-array [env array]
  `(:ARRAY ~(expand-forms env array)))

(defn front-dotted? [s]
  (re-matches #"^\.[^\.]+$" (str s)))

(defn expand-call [env head tail]
  (if (front-dotted? head)
    `(:CALL [:PROJECT
             ~(expand-form env (first tail))
             [:CONSTANT ~(apply str (rest (str head)))]]
            ~(expand-forms env (rest tail)))
    `(:CALL ~(expand-form env head)
            ~(expand-forms env tail))))

(defn expand-list [env form]
  (let [[head & tail] form]
    (case (env/resolve env head)
      :DEFINE
      (baddef form)
      
      :DEFINE_SYNTAX
      (baddef form)

      :BEGIN
      (cons :BEGIN (expand-forms env tail))
      
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

      ;; else
      (expand-call env head tail))))

