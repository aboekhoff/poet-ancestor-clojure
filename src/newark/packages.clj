(ns newark.packages
  (:use newark.util)
  (:require
   [clojure.java.io  :as io]
   [clojure.string   :as str]
   [newark.expander  :as expander]
   [newark.env       :as env]
   [newark.emitter   :as emitter]
   [newark.reader    :as reader]
   [newark.compiler  :as compiler]
   [newark.constants :as constants])
  (:import
   java.io.File))

(use 'clojure.pprint)

(defn compile-toplevel [sexp env]
  (let [expansion (expander/expand-toplevel env sexp)
        ;_         (pprint expansion)
        ir        (compiler/compile-toplevel expansion)
        ;_         (pprint ir)
        js        (emitter/emit-token ir)
        ;_         (println js)
        ]
    js))

(def packages (atom {}))
(def loadpath (atom [(System/getProperty "user.dir")
                     (System/getProperty "user.home")]))

(defn add-path! [path]
  (let [paths (.split path File/pathSeparator)]
    (reset! loadpath (apply conj (vec paths) @loadpath))))

(defn id->string [id]
  (str/replace 
   (if (or (symbol? id) (keyword? id))
     (if-let [ns (namespace id)]
       (str (namespace id) File/separator (name id))
       (name id))
     (str id))
   #"/"
   File/separator))

(defn get-package [id]
  (get @packages (id->string id)))

(defn set-package! [id pkg]
  (swap! packages assoc (id->string id) pkg))

(defn package-exists? [id]
  (contains? @packages (id->string id)))

(defn get-last-modified [filename]
  (let [file (io/file filename)]
    (when (.exists file)
      (.lastModified file))))

(defn id->filename [id]
  (str 
   (str/replace (id->string id) #"\.newark$" "")
   ".newark"))

(defn id->file* [id] 
  (let [filename (id->filename id)]
    (or (loop [paths @loadpath]
          (when-let [[p & paths] (seq paths)]           
            (let [file (io/file p filename)
                  ;_    (println "locating" (.getCanonicalPath file))
                  ]
              (if (and file (.exists file))
                file
                (recur paths)))))
        (io/resource filename))))

(defn file? [x]
  (instance? java.io.File x))

(defn id->file [id]
  (or (id->file* id)
      (throw (RuntimeException.
               (str "unable to load package " (id->string id))))))

(defn get-last-modified [id]
  (let [file (id->file id)]
    (when (file? file) (.lastModified file))))

(defn package-out-of-date? [id]
  (let [pkg (get-package id)
        lm  (:last-modified pkg)]
    (and lm (< lm (get-last-modified id)))))

(defn load-package* [id]
  (let [file          (id->file id)
        last-modified (get-last-modified id)
        sexp          (reader/read-string* (slurp file) (id->string id))
        pkg-env       (env/make-standard-env (id->string id))
        js            (compile-toplevel sexp pkg-env)
        pkg
        {:last-modified last-modified
         :sexp          sexp
         :env           pkg-env
         :js            (str "// " (id->filename id) "\n\n" js)}]    
    (set-package! id pkg)
    pkg))

(defn load-package [id]
  (if (and (package-exists? id)
           (not (package-out-of-date? id)))
    (get-package id)
    (load-package* id)))

(defn import-package [importer importee-id & {:keys [prefix]}]
  (let [importee (:env (load-package importee-id))
        rename   (fn [x] (if prefix (str prefix x) (str x)))]
    (when (not importee)
      (throw (RuntimeException.
              (str "unable to load package: "
                   (id->string importee-id)))))
    (swap! (:imported-packages importer)
           conj
           (id->string importee-id))
    (doseq [[k v] @(first (:symbols importee))]
      (when (not (@(:imported-symbols importee) k))
        (let [k* (rename k)]
          (dictionary-set* (:symbols importer) k* v)
          (swap! (:imported-symbols importer) conj k*))))))

(defn id->deps* [id]
  (let [deps @(:imported-packages (:env (load-package id)))]
    (concat
     (mapcat id->deps* deps)
     deps)))

(defn id->deps [id]
  (distinct (id->deps* id)))

(defn build-package [id]
  (let [pkg  (load-package id)
        deps (id->deps id)]
    (str
     "(function() { \n\n"
     constants/PRELUDE
     "\n"
     (str/join "\n\n" (map (comp :js get-package) deps))
     "\n\n"
     (:js pkg)
     "\n\n})();")))

(alter-var-root
 (var expander/import-package)
 (constantly import-package))