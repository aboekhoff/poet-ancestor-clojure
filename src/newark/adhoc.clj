(ns newark.adhoc
  (:use     [clojure.pprint])
  (:require [newark.expander :as expander]
            [newark.compiler :as compiler]))

(defmacro ex [x]
  `(let [x# (expander/ex ~x)
         _# (pprint x#)
         x# (compiler/norm (first x#))
         _# (pprint x#)
         x# (compiler/serialize-block x# identity)
         _# (pprint x#)]))