(ns newark.constants)

(def CORE (atom {}))
(def GLOBAL "js")
(def USER* "Newark")
(def USER (str GLOBAL "." USER*))
(def PRELUDE
  (str
   "var " GLOBAL ";\n"
   "if (typeof window == 'undefined') {"
   GLOBAL " = global; } else { " GLOBAL " = window; }\n" 
   "if (typeof "
       USER " == 'undefined') { "
       USER " = {} } \n\n"))