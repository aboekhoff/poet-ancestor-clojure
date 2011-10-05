(ns newark.core
  (:gen-class)
  (:require
   [newark.reader :as reader]
   [newark.env :as env]
   [newark.expander :as expander]
   [newark.compiler :as compiler]
   [newark.emitter :as emitter]
   [newark.constants :as constants]))

(use 'clojure.pprint)

(defn compile-sexp [sexp]
  (let [env  (env/make-standard-environment)
        ast  (expander/expand-body env sexp)
        ast* (compiler/compile-expansion ast)
        _    (pprint ast*)
        js   (emitter/emit-tokens* ast*)
        js*  (emitter/wrap-toplevel js)]
    js*))

(defn compile-string [string]
  (let [sexp (reader/read-all-forms (reader/string->input-port string))]
    (compile-sexp sexp)))

(defn compile-file [file-descriptor]
  (let [sexp (reader/read-file file-descriptor)]
    (compile-sexp sexp)))

(defn -main [& args]
  (doseq [a args] (println a)))