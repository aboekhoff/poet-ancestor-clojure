(ns newark.constants)

(def CORE (atom {}))
(def GLOBAL "js")
(def USER* "Newark")
(def USER (str GLOBAL "." USER*))
(def PRELUDE (str "if (typeof "
                  USER " == 'undefined') { "
                  USER " = {} } \n\n"))