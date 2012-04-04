(ns newark.builder
  (:use newark.util
        newark.packages)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def targets (atom {}))

(defn update-target! [id]
  (let [id (id->string id)]
    (swap! targets assoc id (cons id (id->deps id)))))

(defn build-target! [id]
  (println "building" (id->filename id))
  (try 
    (load-package* id)
    (update-target! id)
    (let [js   (build-package id)
          file (id->file id)
          path (.getCanonicalPath file)
          path (str/replace path #"\.newark$" ".js")]
      (spit (io/file path) js))
    (catch Exception e
      (println "Error when building" (id->filename id))
      (.printStackTrace e))))

(defn any? [pred coll]
  (if (empty? coll)
    false
    (if (pred (first coll))
      true
      (recur pred (rest coll)))))

(defn check-target! [[main & _ :as ids]]  
  (when (any? package-out-of-date? ids)
    (build-target! main)))

(defn check-targets! []
  (doseq [[_ ids] @targets]
    (check-target! ids)))

(defn watch! []
  (check-targets!)
  (Thread/sleep 500)
  (recur))

(defn start-watcher! []
  (.start (Thread. watch!)))

(defn init! [ids]
  (doseq [id ids]
    (build-target! id)))