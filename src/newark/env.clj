(ns newark.env
  (:refer-clojure :exclude [resolve])
  (:require [clojure.string :as str]
            [newark.constants :as constants]))

(def env-db (atom []))

(defn tag->env [tag]
  (get @env-db tag))

(defn make-tag [env]
  (let [tag (count @env-db)]
    (swap! env-db conj env)
    tag))

(defn apply-tag [sym tag]
  (let [tags (-> sym meta ::tags)]
    (if (= tag (first tags))
      (with-meta sym {::tags (rest tags)})
      (with-meta sym {::tags (cons tag tags)}))))

(defn tagged? [sym]
  (not (empty? (-> sym meta ::tags))))

(defn untag [sym]
  (when (tagged? sym)
    (let [tags (seq (-> sym meta ::tags))]    
      [(tag->env (first tags))
       (with-meta sym {::tags (rest tags)})])))

(defn symbol->key [sym]
  (str (str/join ":" (-> sym meta ::tags)) "#" (str sym)))

(defn key->symbol [sym]
  (let [string (str sym)
        name (str/replace string #"^([\d+]:?)*#" "")
        tags (str/replace string #"#.*" "")       
        tags (when (seq tags)
               (map #(Integer/parseInt %) (str/split tags #":")))]
    (with-meta (symbol nil name) {::tags tags})))

(defn sanitize [form tag]
  (let [sanitize* (fn [x] (sanitize x tag))]
    (cond
     (symbol? form) (apply-tag form tag)
     (seq? form)    (map sanitize* form)
     (vector? form) (vec (map sanitize* form))
     :else          form)))

(defn make-environment [& [namespace]]
  (atom {:bindings {}
         :namespace (or namespace "user")
         :next-id 1
         :toplevel true}))

(defn extend-environment [e]
  (atom {:bindings {}
         :parent   e}))

(defn root-environment? [e]
  (-> @e :toplevel true?))

(defn get-environment-root [e]
  (if (root-environment? e) e (recur (:parent @e))))

(defn make-location [env name]
  (let [root (get-environment-root env)
        id   (:next-id @root)
        type (if (root-environment? env) :GLOBAL :LOCAL)]
    (swap! root assoc :next-id (inc id))
    [:VAR type id (str name) (:namespace @root)]))

(defn lookup* [env key]
  (when env
    (or (get-in @env [:bindings key])
        (recur (:parent @env) key))))

(defn lookup [env sym]
  (lookup* env (symbol->key sym)))

(defn resolve [env sym]
  (when (symbol? sym)
    (or (lookup env sym)
        (when (tagged? sym)
          (let [[env* sym*] (untag sym)]
            (recur env* sym*))))))

(defn bind [env sym val]
  (reset! env (assoc-in @env [:bindings (symbol->key sym)] val)))

(defn defvar [env sym]
  (let [loc (make-location env sym)]
    (bind env sym loc)
    loc))

(def standard-env*
  {"#define"               :DEFINE
   "#define-syntax"        :DEFINE_SYNTAX
   "#define-symbol-syntax" :DEFINE_SYMBOL_SYNTAX
   "#let-syntax"           :LET_SYNTAX  
   "#let-symbol-syntax"    :LET_SYMBOL_SYNTAX
   "#if"                   :IF
   "#set!"                 :SET!
   "#let"                  :LET
   "#."                    :PROJECT
   "#fn"                   :FN
   "#while"                :WHILE
   "#begin"                :BEGIN
   "#new"                  :NEW
   "#js"                   [:RAW constants/GLOBAL]
   "#for-each-property"    :FOR_EACH_PROPERTY
   
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
   
   "#<"   [:OP :LOGIC "<"]
   "#>"   [:OP :LOGIC ">"]
   "#<="  [:OP :LOGIC "<="]
   "#>="  [:OP :LOGIC ">="]
   "#="   [:OP :LOGIC "==="]
   "#!="  [:OP :LOGIC "!=="]
   "#=?"  [:OP :LOGIC "=="]
   "#!=?" [:OP :LOGIC "!="]})

(defn make-standard-environment [& [namespace no-core]]
  (let [env (make-environment namespace)
        bindings (if no-core
                   standard-env*
                   (merge standard-env* @constants/CORE))]
    (swap! env assoc :parent (atom {:bindings bindings}))
    env))

