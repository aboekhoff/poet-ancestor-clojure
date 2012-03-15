(ns newark.adhoc
  (:use     [clojure.pprint])
  (:require [newark.expander :as expander]
            [newark.compiler :as compiler]
            [newark.emitter  :as emitter]))

(defmacro ex [& xs]
  `(let [x# (expander/ex ~@xs)
         _# (pprint x#)
         x# (compiler/compile-toplevel x#)
         _# (pprint x#)
         x# (emitter/emit-token x#)
         _# (println x#)]))