(ns newark.env
  (:refer-clojure :exclude [resolve])
  (:require [clojure.string :as str]
            [newark.constants :as constants]
            [newark.util :as util]))

(declare make-env)

(def next-env-id (util/make-generator))
(def tag-db (atom {}))
(def modules (atom {}))

(defn str* [x]
  (if (keyword? x) (name x) (str x)))

(defn find-or-create-module [ns]
  (let [ns (str* ns)]
    (or (get @modules ns)
        (let [e (make-env ns)]
          (swap! modules assoc ns e)
          e))))

(defn find-module [ns]
  (or (get @modules (str* ns))
      (throw (RuntimeException.
              (str "no module named " ns " exists")))))

(defn next-tag-id [env]
  (let [env-id (:id env)
        tag-id (or (get @tag-db env-id) 0)]
    (swap! tag-db assoc env-id (inc tag-id))
    tag-id))

(defn make-tag [env]
  (let [env-id (:id env)
        tag-id (next-tag-id env)]
    {:env env
     :tag (str env-id "_" tag-id)}))

(defn symbol->tags [s]
  (:tags (meta s)))

(defn symbol->tag-string [s]
  (str/join ":" (map :tag (symbol->tags s))))

(defn tagged? [s]
  (not (empty? (symbol->tags s))))

(defn symbol->key [s]
  (cond
   (nil? s)
   nil
   
   (tagged? s)
   (str "#[" (symbol->tag-string s) "]" (name s))
   
   :else
   (name s)))

(defn tag-symbol [s t]
  (if (namespace s)
    s
    (let [ts (symbol->tags s)]
      (if (= t (first ts))
        (util/merge-meta s {:tags (rest ts)})
        (util/merge-meta s {:tags (cons t ts)})))))

(defn untag-symbol [s]
  (util/merge-meta s {:tags (rest (symbol->tags s))}))

(defn sanitize [x t]
  (cond
   (symbol? x)
   (tag-symbol x t)

   (seq? x)
   (map #(sanitize % t) x)

   (vector? x)
   (vec (map #(sanitize % t) x))

   :else
   x))

(defn make-env [module & [symbols]]
  {:module  module
   :symbols (or symbols (util/make-dictionary))
   :id      (next-env-id)})

(defn extend-env [env]
  (assoc env
    :symbols (util/extend-dictionary (:symbols env))
    :id      (next-env-id)))

(defn reify-symbol [s]
  (if (tagged? s)
    (symbol (str "#:" (symbol->tag-string s) ":" (name s)))
    s))

(defn bind-macro [e s m]
  (util/dictionary-set (:symbols e) (symbol->key s) m))

(defn bind-symbol [e s]
  (let [s* (reify-symbol s)]
    (util/dictionary-set (:symbols e) (symbol->key s) s*)
    s*))

(defn bind-label [e s]
  (let [s* (reify-symbol s)]
    (util/dictionary-set (:symbols e) [:LABEL (symbol->key s)] s*)
    s*))

(defn bind-global [e s]
  (let [s1 (reify-symbol s)
        s2 (symbol (:module e) (name s1))]    
    (util/dictionary-set* (:symbols e) (symbol->key s1) s2)
    s2))

(defn resolve-label [e s]
  (util/dictionary-get (:symbols e) [:LABEL (symbol->key s)]))

(defn resolve-symbol [e s]
  (when (symbol? s)
    (or
     (when-let [ns (namespace s)]       
       (resolve-symbol (find-module ns)
                       (with-meta (symbol nil (name s)) (meta s))))
     (util/dictionary-get (:symbols e) (symbol->key s))
     (when (tagged? s)
       (recur (:env (first (symbol->tags s)))
              (untag-symbol s))))))

(def core (find-or-create-module "core"))

(doseq [x ["define"
           "define-syntax"
           "define-symbol-syntax"
           "let-syntax"
           "let-symbol-syntax"
           "if"
           "set!"
           "let"
           "letrec"
           "."
           "fn"
           "block"
           "loop"
           "return-from"
           "begin"
           "throw"
           "raw"
           "quote"
           "js"
           "newark"

           "+" "*" "-" "/"
           "<" ">" "<=" ">="

           "instanceof"
           "typeof"]]
  (bind-global core (symbol x)))

(defn import! [e1 e2 & [pred]]
  (let [pred (or pred (constantly true))]
    (doseq [[k v] @(first (:symbols e2))]
      (util/dictionary-set* (:symbols e1) k v))))

(comment 
  (def standard-symbols*
    {"#define"               :DEFINE
     "#define-syntax"        :DEFINE_SYNTAX
     "#define-symbol-syntax" :DEFINE_SYMBOL_SYNTAX
     "#let-syntax"           :LET_SYNTAX  
     "#let-symbol-syntax"    :LET_SYMBOL_SYNTAX
     "#if"                   :IF
     "#set!"                 :SET
     "#let"                  :LET
     "#."                    :PROJECT
     "#fn*"                  :FN
     "#block*"               :BLOCK
     "#loop*"                :LOOP
     "#return-from"          :RETURN_FROM
     "#do"                   :BEGIN
     "#new"                  :NEW
     "#do-properties"        :FOR_EACH_PROPERTY
     "#throw"                :THROW
     "#raw"                  :RAW
     "#include"              :INCLUDE
     "#quote"                :QUOTE
     "#js"                   [:RAW constants/GLOBAL]
     "#newark"               [:RAW constants/NEWARK]
     
     "#+" [:OP :FOLD "+"]
     "#-" [:OP :FOLD "-"]
     "#*" [:OP :FOLD "*"]
     "#/" [:OP :FOLD "/"]

     "#bit-shift-left"   [:OP :FOLD "<<"]
     "#bit-shift-right"  [:OP :FOLD ">>"]
     "#bit-shift-right*" [:OP :FOLD ">>>"]
     "#bit-and"          [:OP :FOLD "&"]
     "#bit-or"           [:OP :FOLD "|"]
     "#bit-xor"          [:OP :FOLD "^"]
     
     "#bit-not"    [:OP :UNARY "~"]   
     "#!"          [:OP :UNARY "!"]
     "#typeof"     [:OP :UNARY "typeof"]
     "#instanceof" [:OP :BINARY "instanceof"]

     "#mod" [:OP :BINARY "%"]
     
     "#<"    [:OP :LOGIC "<"]
     "#>"    [:OP :LOGIC ">"]
     "#<="   [:OP :LOGIC "<="]
     "#>="   [:OP :LOGIC ">="]
     "#="    [:OP :LOGIC "==="]
     "=?"    [:OP :LOGIC "=="]
     "#!="   [:OP :LOGIC "!=="]
     "#!=?"  [:OP :LOGIC "=="]})

  (defn import! [importer importee]
    (doseq [[k v] (deref (first (:symbols importee)))]
      (util/dictionary-set (:symbols importer) k v)))

  (defn make-standard-environment [& [package no-core]]
    (let [env (make-environment
               package
               (util/make-dictionary (list (atom standard-symbols*))))]
      (when (not no-core) (import! env @constants/CORE))
      env))

  ;; keep a reference to standard symbols for the reader

  (def base-env (make-standard-environment "__BASE__" true))
  (def base-color :BASE)
  (swap! color-db assoc base-color base-env)
)