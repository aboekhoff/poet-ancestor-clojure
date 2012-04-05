(ns newark.expander
  (:refer-clojure :exclude [macroexpand macroexpand-1])
  (:require [newark.env :as env]
            [newark.syntax :as syntax]
            [newark.reader :as reader]
            [clojure.string :as str]))

(declare expand-symbol expand-list expand-array
         expand-function expand-let expand-letrec)

(defn import-package
  [importer importee-id & {:keys [prefix]}]
  (throw (RuntimeException. "import-package not configured"))) 

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
     :else        x)))

(defn expand-forms [env xs]
  (doall (map (fn [x] (expand-form env x)) xs)))

(defn resolves-to? [env form val]
  (and (seq? form) (= val (env/resolve-symbol env (first form)))))

(defn definition? [env form]
  (resolves-to? env form 'core/define*))

(defn include? [env form]
  (resolves-to? env form 'core/include))

(defn import? [env form]
  (resolves-to? env form 'core/import))

(defn command? [env form]
  (resolves-to? env form 'core/newark-command))

(defn macro-definition? [env form]
  (resolves-to? env form 'core/define-syntax))

(defn symbol-macro-definition? [env form]
  (resolves-to? env form 'core/define-symbol-syntax))

(defn begin? [env form]
  (resolves-to? env form 'core/begin))

(defn prepare-body
  "performs a preliminary macroexpansion
   and concatenation of splicing-begins"
  [env forms]
  (when-let [[x & xs] forms]
    (let [x (macroexpand env x)]
      (if (begin? env x)
        (recur env (concat (rest x) xs))
        (lazy-seq
         (cons x (prepare-body env xs)))))))

(defn collect-defines
  "takes a prepared body and returns a (possibly empty)
   preamble of internal definitions"
  ([env forms]
     (collect-defines env forms [])) 
  ([env forms acc]
     (let [[x & xs] forms]
       (if (definition? env x)
         (recur env xs (conj acc (rest x)))
         [acc forms]))))

(defn expand-body [env body]
  (let [body        (prepare-body env body)
        [defs body] (collect-defines env body)]
    (if (empty? defs)
      (case (count body)
        0 nil
        1 (expand-form env (first body))
          (cons 'core/begin (expand-forms env body)))
      (expand-letrec env defs body))))



(defn expand-symbol [e x]
  (let [den (env/resolve-symbol e x)]    
    (cond
     (symbol? den) den
     (not den)     (env/bind-global e x)     
     :else         (syntax-error "can't take value of syntax" x))))

(defn expand-let [env defs body]
  (if (empty? defs)
    (expand-body (env/extend-env env) body)
    (let [exprs (doall (map #(expand-form env (second %)) defs))
          env*  (env/extend-env env)
          vars  (doall (map #(env/bind-symbol env* (first %)) defs))
          body  (expand-body env* body)]
      (list 'core/let (map list vars exprs) body))))

(defn expand-letrec [env defs body]
  (if (empty? defs)
    (expand-body (env/extend-env env) body)
    (let [env*  (env/extend-env env)
          vars  (doall (map #(env/bind-symbol env* (first %)) defs))
          exprs (doall (map #(expand-form env* (second %)) defs))
          body  (expand-body env* body)]
      (list 'core/letrec (map list vars exprs) body))))

(defn expand-args [env args]
  (let [[pargs &rest] (split-with #(not= % '.) args)
        &rest         (second &rest)
        pargs         (doall (map #(env/bind-symbol env %) pargs))
        pargs         (or pargs '())]    
    (if &rest
      (let [&rest (env/bind-symbol env &rest)]
        (concat pargs (list '. &rest)))
      pargs)))

(defn expand-function [env args body]
  (let [env   (env/extend-env env)
        args  (expand-args env args)
        body  (expand-body env body)]
    (list 'core/fn args body)))

(defn expand-method [env this args body]
  (let [env  (env/extend-env env)
        this (env/bind-symbol env this)
        args (expand-args env args)
        body (expand-body env body)]
    (list 'core/method (cons this args) body)))

(defn expand-array [env array]
  (vec (map #(expand-form env %) array)))

(defn front-dotted? [s]
  (and (symbol? s)
       (re-matches #"^\.[^\.]+$" (str s))))

(defn expand-call [e xs]
  (if (front-dotted? (first xs))
    (let [field (.substring (str (first xs)) 1)
          obj   (expand-form e (second xs))]
      (list* (list 'core/. obj field)
             (expand-forms e (drop 2 xs))))
    (expand-forms e xs)))

(defn expand-let-syntax [env bindings body]
  (if (empty? bindings)
    (expand-body env body)
    (let [[name & stx] (first bindings)
          env*         (env/extend-env env)
          macro        (syntax/make-syntax env* stx)]
      (env/bind-macro env* name macro)
      (recur env* (rest bindings) body))))

(defn expand-let-symbol-syntax [env bindings body]
  (if (empty? bindings)
    (expand-body env body)
    (let [[name form] (first bindings)
          env*        (env/extend-env env)
          macro       (syntax/make-symbol-syntax env* form)]
      (env/bind-macro env* name macro)
      (recur env* (rest bindings) body))))

(defn expand-quote [form]  
  (list 'core/quote form))

(defn front-dotted? [x]
  (and (symbol? x)
       (.startsWith (name x) ".")))

(defn expand-call [env head tail]
  (if (front-dotted? head)
    (let [callee (expand-form env (first tail))
          field  (.substring (name head) 1)
          args   (expand-forms env (rest tail))]
      (cons (list 'core/. callee field) args))
    (cons (expand-form env head) (expand-forms env tail))))

(comment
  (unwind-protect
   (:try ...)
   (:catch e ...)
   (:finally ...)))

(defn expand-unwind-protect [env tail]
  (let [clauses     (into {} (for [[key & body] tail] [key body]))
        try-block   (when-let [body (:try clauses)]
                      (expand-body env body))
        catch-block (when-let [[err & body] (:catch clauses)]
                      (let [env  (env/extend-env env)
                            err  (env/bind-symbol env err)
                            body (expand-body env body)]
                        (list err body)))
        finally-block (when-let [body (:finally clauses)]
                        (expand-body env body))]
    (list 'core/unwind-protect
          (list :try     try-block)
          (if catch-block
            (cons :catch catch-block)
            (list :catch nil))
          (list :finally finally-block))))


(defn expand-quasiquote [env xs])

(defn expand-list [env form]
  (let [[head & tail] form]    
    (case (keyword (env/resolve-symbol env head))            
      :core/define*
      (baddef form)
      
      :core/define-syntax
      (baddef form)

      :core/let-syntax
      (expand-let-syntax env (first tail) (rest tail))

      :core/let-symbol-syntax
      (expand-let-symbol-syntax env (first tail) (rest tail))
      
      :core/begin
      (let [env* (env/extend-env env)]
        (expand-body env* tail))
      
      :core/set!
      (case (count tail)
        2 (list* 'core/set! (expand-forms env tail))
        3 (list 'core/set!
                (list 'core/.
                      (expand-form env (first tail))
                      (expand-form env (second tail)))
                (expand-form env (nth tail 2)))
        (throw (RuntimeException.
                "core::set! requires exactly two or exactly three arguments")))      
      
      :core/.
      (cons 'core/. (expand-forms env tail))

      :core/quote
      (expand-quote (first tail))
      
      :core/let
      (expand-let env (first tail) (rest tail))

      :core/letrec
      (expand-letrec env (first tail) (rest tail))

      :core/fn
      (let [[args & body] tail]
        (expand-function env args body))

      :core/method
      (let [[args & body] tail
            [this & args] args]
        (expand-method env this args body))
      
      :core/if
      (let [[a b c] tail]
        (cons 'core/if (expand-forms env [a b c])))

      :core/block
      (let [env*  (env/extend-env env)
            label (env/bind-label env (first tail))]
        (list 'core/block label (expand-body env (rest tail))))

      :core/do-properties*
      (let [[label [prop obj] & body] tail
            obj   (expand-form env obj)
            env   (env/extend-env env)
            prop  (env/bind-symbol env prop)
            label (env/bind-label  env label)
            body  (expand-body env body)]
        (list 'core/do-properties* label (list prop obj) body))
      
      :core/loop*
      (let [env   (env/extend-env env)
            label (env/bind-label env (first tail))]
        (list 'core/loop* label (expand-body env (rest tail))))

      :core/return-from
      (list 'core/return-from
            (env/resolve-label env (first tail))
            (expand-form env (second tail)))

      :core/new
      (cons 'core/new (expand-forms env tail))

      :core/throw
      (list 'core/throw (expand-form env (first tail)))

      :core/unwind-protect
      (expand-unwind-protect env tail)

      :core/quasiquote
      (expand-quasiquote env (first tail))
      
      ;; else
      (expand-call env head tail))))

(defn expand-toplevel* [e xs]
  (when-let [[x & xs] (seq xs)]
    (let [x (macroexpand e x)]
      (lazy-seq
       (cond
        (begin? e x)
        (expand-toplevel* e (concat (rest x) xs))

        (macro-definition? e x)
        (do (env/bind-macro e
                            (second x)
                            (syntax/make-syntax e (drop 2 x))) 
            (expand-toplevel* e xs))

        (symbol-macro-definition? e x)
        (do (env/bind-macro e
                            (second x)
                            (syntax/make-symbol-syntax e (nth x 2)))
            (expand-toplevel* e xs))

        (definition? e x)
        (let [v  (env/bind-global e (second x))
              x* (expand-form e (nth x 2))]
          (cons (list 'core/set! v x*)
                (expand-toplevel* e xs)))        

        (import? e x)
        (do (apply import-package e (rest x))
            (expand-toplevel* e xs))
        
        (include? e x)       
        (let [more (for [filename (rest x)]
                     (reader/read-file filename))]
          (expand-toplevel* e (concat (apply concat more) xs)))
        
        :else
        (cons (expand-form e x)
              (expand-toplevel* e xs)))))))

(defn expand-toplevel [e xs]
  (cons 'core/begin (expand-toplevel* e xs)))

(def test-module (env/find-or-create-module 'test))
(env/import! test-module (env/find-or-create-module 'core))

(defmacro ex [& sexps]
  `(expand-toplevel test-module (quote ~sexps)))

(comment
  (defn expand-opcall [[_ type op] args]
    (case type
      :FOLD   [:OPCALL op args]
      :BINARY [:OPCALL op args]    
      :UNARY  [:OPCALL op args]
      :LOGIC  (expand-logical-operator op args))))