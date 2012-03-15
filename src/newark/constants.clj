(ns newark.constants)

(def CORE (atom {}))
(def NEWARK "G")
(def GLOBAL "JS")
(def PRELUDE
  (str
   "var " GLOBAL ", " NEWARK ";\n"
   GLOBAL "= (typeof window == 'undefined') ? global : window;\n"
   GLOBAL " = window; } else { " GLOBAL " = process; }\n"
   NEWARK " = {};\n"
   NEWARK "['core::js']=" GLOBAL ";\n"
   NEWARK "['core::newark']=" NEWARK ";\n"))