(ns newark.packages
  (:use newark.util)
  (:require
   [clojure.java.io :as io]
   [clojure.string  :as str]
   [newark.expander :as expander]
   [newark.env      :as env]
   [newark.emitter  :as emitter]
   [newark.reader   :as reader]
   [newark.compiler :as compiler])
  (:import
   java.io.File))

(defn compile-toplevel [sexp env]
  (let [expansion (expander/expand-toplevel env sexp)
        ir        (compiler/compile-toplevel expansion)
        js        (emitter/emit-token ir)]
    js))

(def packages (atom {}))
(def loadpath (atom [(System/getProperty "user.dir")
                     (System/getProperty "user.home")]))

(defn id->string [id]
  (str/replace 
   (if (or (symbol? id) (keyword? id))
     (if-let [ns (namespace id)]
       (str (namespace id) File/separator (name id))
       (str id))
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

(defn id->file [id]
  (or (io/resource (id->string id))
      (loop [paths @loadpath]
        (when-let [[p & paths] (seq paths)]
          (let [filename
                (str p
                     File/separator
                     (id->string id)
                     ".newark")
                file (io/file filename)]
            (if (.exists file)
              file
              (recur paths)))))))

(defn get-last-modified [id]
  (.lastModified (io/file (id->filename id))))

(defn package-out-of-date? [id]
  (let [pkg (get-package id)]    
    (< (:last-modified pkg) (get-last-modified id))))

(defn load-package* [id]
  (let [file          (id->file id)
        last-modified (.lastModified file)
        sexp          (reader/read-file file)
        pkg-env       (env/make-standard-env (id->string id))
        js            (compile-toplevel sexp pkg-env)
        pkg
        {:last-modified last-modified
         :sexp          sexp
         :env           pkg-env
         :js            js}]    
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

(alter-var-root
 (var expander/import-package)
 (constantly import-package))