(ns newark.scratch
  (:require [newark.expander :as expander]
            [newark.env :as env]
            [newark.compiler :as nc]
            [newark.emitter :as emitter]))

(def e0 (env/make-standard-environment "scratch"))

(defmacro ex [& exprs]
  `(expander/expand-body e0 (quote ~exprs)))

(defmacro nc [& exprs]
  `(nc/compile-expansion (ex ~@exprs)))

(defmacro nw [& exprs]
  `(println (emitter/emit-tokens (nc ~@exprs))))