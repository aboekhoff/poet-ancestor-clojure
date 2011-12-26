(ns newark.env
  (:refer-clojure :exclude [resolve])
  (:require [clojure.string :as str]
            [newark.constants :as constants]
            [newark.util :as util]))

;; hygiene machinery

(def color-db (atom {}))

(defn make-color [defining-env calling-env]
  (let [color (str (:package calling-env)
                   "$"
                   (:package defining-env)
                   "$"
                   ((:next-color calling-env)))]
    (swap! color-db assoc color defining-env)
    color))

(defn color->env [color]
  (get @color-db color))

(defn symbol->color-string [sym]
  (let [colors (-> sym meta ::colors)]
    (if (seq colors)
      (str/join ":" colors)
      "")))

(defn symbol->key [sym]
  (if sym
    (str (symbol->color-string sym) "#" (name sym))
    "NIL"))

(defn painted? [sym]
  (not (empty? (-> sym meta ::colors))))

(defn paint [sym color]
  (let [colors (-> sym meta ::colors)]
    (if (= color (first colors))
      (with-meta sym {::colors (rest colors)})
      (with-meta sym {::colors (cons color colors)}))))

(defn unpaint [sym]
  (let [colors (seq (-> sym meta ::colors))]    
      [(color->env (first colors))
       (with-meta sym {::colors (rest colors)})]))

;; make environment

(defn make-environment [package & [symbols]]
  {:package    package
   :toplevel?  true
   :level      0
   :symbols    (or symbols (util/make-dictionary))
   :labels     (util/make-dictionary)
   :next-local (util/make-generator)
   :next-color (util/make-generator)})

(defn extend-symbols [env]
  (assoc env
    :toplevel? false
    :symbols (util/extend-dictionary (:symbols env))))

(defn extend-labels [env]
  (assoc env
    :toplevel? false
    :labels (util/extend-dictionary (:labels env))))

(defn extend-scope [env]
  (assoc env
    :toplevel? false
    :level   (-> env :level inc)
    :symbols (util/extend-dictionary (:symbols env))
    :labels  (util/make-dictionary)
    :next-local (util/make-generator)))

(defn bind-symbol [env sym val]
  (util/dictionary-set (:symbols env) (symbol->key sym) val))

(defn bind-label [env sym val]
  (util/dictionary-set (:labels env) (symbol->key sym) val))

(defn make-global [env sym]
  (let [key (symbol->key sym)
        loc [:GLOBAL (:package env) key]]
    (util/dictionary-set*
     (:symbols env)
     key
     loc)
    loc))

(defn next-local [env & [sym]]
  [:LOCAL (:level env) ((:next-local env)) sym])

(defn make-local [env sym]
  (let [loc (next-local env sym)]
    (util/dictionary-set
     (:symbols env)
     (symbol->key sym)
     loc)
    loc))

(defn make-var [env sym]
  (if (:toplevel? env)
    (make-global env sym)
    (make-local env sym)))

(defn make-label [env sym]
  (let [loc (next-local env sym)]
    (bind-label env sym loc)
    loc))

(defn lookup [env key sym]
  (util/dictionary-get (key env) (symbol->key sym)))

(defn resolve [env key sym]
  (or (lookup env key sym)
      (when (painted? sym)
        (let [[env* sym*] (unpaint sym)]
          (recur env* key sym*)))))

(defn resolve-symbol [env sym]
  (resolve env :symbols sym ))

(defn resolve-label [env sym]
  (resolve env :labels sym))

;(defn key->symbol [sym]
; (let [string (str sym)
;      name (str/replace string #"^([\d+]:?)*#" "")
;      tags (str/replace string #"#.*" "")       
;      tags (when (seq tags)
;  (map #(Integer/parseInt %) (str/split tags #":")) )]
;  (with-meta (symbol nil name) {::tags tags})))

(defn sanitize [form color]
  (let [sanitize* (fn [form] (sanitize form color))]
    (cond
     (symbol? form) (paint form color)
     (seq? form)    (map sanitize* form)
     (vector? form) (vec (map sanitize* form))
     :else          form)))

(def standard-symbols*
    {"#def*"            :DEFINE
     "#defsyntax"       :DEFINE_SYNTAX
     "#defsymbolsyntax" :DEFINE_SYMBOL_SYNTAX
     "#letsyntax"       :LET_SYNTAX  
     "#letsymbolsyntax" :LET_SYMBOL_SYNTAX
     "#if"              :IF
     "#set!"            :SET
     "#let"             :LET
     "#."               :PROJECT
     "#fn*"             :FN
     "#block*"          :BLOCK
     "#loop*"           :LOOP
     "#return-from"     :RETURN_FROM
     "#do"              :BEGIN
     "#new"             :NEW
     "#do-properties"   :FOR_EACH_PROPERTY
     "#throw"           :THROW
     "#raw"             :RAW
     "#include"         :INCLUDE
     "#quote"           :QUOTE
     "#js"              [:RAW constants/GLOBAL]
     
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
