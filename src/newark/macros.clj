(ns newark.macros
  (:require [newark.env :as env]))

(def ^:dynamic capture (fn []))

(defn make-transformer [transformer-fn]
  (fn [calling-env input]
    (let [tag     (env/make-tag env/core)]      
      (binding
          [capture (fn [x]
                    (assert (symbol? x))
                    (env/ensure-tag x tag))]
        (env/sanitize
         (apply transformer-fn
                (env/sanitize (rest input) tag))
         tag)))))

(defn make-newark-macro [name transformer-fn]
  (env/bind-macro env/core name (make-transformer transformer-fn)))

(defmacro define-newark-macro
  [name & fn-tail]
  `(make-newark-macro (quote ~name) (fn ~@fn-tail)))

