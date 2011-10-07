(ns newark.core
  (:gen-class)
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [newark.reader :as reader]
   [newark.env :as env]
   [newark.expander :as expander]
   [newark.compiler :as compiler]
   [newark.emitter :as emitter]
   [newark.constants :as constants]))

(use 'clojure.pprint)

(defn compile-sexp [sexp & [env]]
  (let [env  (or env (env/make-standard-environment))
        ast  (expander/expand-body env sexp)
        ast* (compiler/compile-expansion ast)
        js   (emitter/emit-tokens* ast*)
        js*  (emitter/wrap-toplevel js)]
    js*))

(defn compile-string [string]
  (let [sexp (reader/read-all-forms (reader/string->input-port string))]
    (compile-sexp sexp)))

(defn compile-file [file-descriptor]
  (let [sexp (reader/read-file file-descriptor)]
    (compile-sexp sexp)))

(defn compile-and-write-file [file-descriptor]
  (let [out (str/replace file-descriptor ".newark" ".js")
        js  (compile-file file-descriptor)]
    (spit out js)))

(defn compile-core! []
  (let [src  (slurp (io/resource "core.newark"))
        port (reader/string->input-port src "core.newark")
        sexp (reader/read-all-forms port)
        env  (env/make-standard-environment "core.newark" true)]
    (compile-sexp sexp env)
    (reset! constants/CORE (-> @env :bindings))))

(def watched (atom {}))
(def watchdir (atom nil))

(defn get-file-list [dirname]
  (for [filename (.list (io/file dirname))
        :when    (.endsWith filename ".newark")
        :let     [file (io/file filename)]
        :when    (and (.exists file)
                      (not (.isDirectory file)))]
    filename))

(defn update-watch-list! [filenames]
  (reset! watched
    (reduce conj {}
            (for [filename filenames
                  :let [file (io/file filename)]]
              (if-let [last-modified (get @watched filename)]
                [filename last-modified]
                [filename 0])))))

(defn process-watched-files! [filenames]
  (doseq [filename filenames]
    (let [file (io/file filename)
          last-processed (get @watched filename)
          last-modified  (.lastModified file)]
      (when (< last-processed last-modified)
        (try
          (println (str "compiling " filename))
          (swap! watched assoc filename last-modified)
          (compile-and-write-file filename)
          (catch Exception e
            (println "error when compiling" filename)
            (.printStackTrace e)))))))

(defn start-watcher! []
  (reset! watchdir (.getCanonicalPath (io/file ".")))
  (println "watching" watchdir)
  (.start
   (Thread. (fn []
              (let [filenames (get-file-list @watchdir)]
                (update-watch-list! filenames)
                (process-watched-files! filenames)
                (Thread/sleep 500)
                (recur))))))

(defn -main [& args]
  (compile-core!)
  (if (or (= (first args) "-w")
          (= (first args) "-watch"))
    (start-watcher!)
    (doseq [a args] (compile-and-write-file a))))