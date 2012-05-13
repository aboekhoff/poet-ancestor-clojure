(ns newark.user
  (:refer-clojure :exclude [destructure])
  (:use (newark core builder packages macros)))

;; repl utility namespace
;; to make it easier to recompile predefined macros

(define-newark-macro
  define
  [name & body]
  (if (seq? name)
    (let [[name & arglist] name]
      (list 'define* name (list* 'fn arglist body)))
    (list 'define* name (first body))))

(define destructure [x]
  (cond
   (symbol? x)
   x
   
   (vector? x)
   (let [tmp (gensym)]
     )))