(ns newark.constants)

(def CORE (atom {}))
(def NEWARK "N")
(def GLOBAL "js")
(def PRELUDE
  (str
   "var " GLOBAL ",\n"
   "var " NEWARK ";\n"
   "if (typeof process == 'undefined') {"
   GLOBAL " = window; } else { " GLOBAL " = process; }\n"
   NEWARK " = {};\n"
   NEWARK "['core::js']=" GLOBAL ";\n"))