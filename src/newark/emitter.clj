(ns newark.emitter
  (:require [clojure.string :as str]
            [newark.constants :as constants]))

(defn wrap [params args code]
  (str "(function(" (str/join "," params)
       ") {\n\n" code "\n})("
       (str/join "," args) ");"))

(defn wrap-toplevel [code]
  (wrap []
        []
        (str constants/PRELUDE code)))

(def accumulator (StringBuilder.))
(defn write! [x] (.append accumulator x))
(defn clear! []  (.setLength accumulator 0))

(def *tab-width* 2)
(def depth (atom 0))
(defn reset-depth! [] (reset! depth 0))
(defn indent! []
  (swap! depth + *tab-width*))
(defn unindent! []
  (swap! depth - *tab-width*))

(defn CL [] (write! ":"))
(defn SC [] (write! ";"))               ;
(defn NL [] (write! "\n"))
(defn SP [] (write! " "))
(defn OP [] (write! "("))
(defn CP [] (write! ")"))
(defn OB [] (write! "["))
(defn CB [] (write! "]"))
(defn OC [] (write! "{"))
(defn CC [] (write! "}"))
(defn CM [] (write! ","))
(defn TAB [] (dotimes [_ @depth] (SP)))

(defmacro in-braces [& body]
  `(do (OC) (indent!) ~@body (unindent!) (NL) (TAB) (CC)))
(defmacro in-brackets [& body]
  `(do (OB) ~@body (CB)))
(defmacro in-parens [& body]
  `(do (OP) ~@body (CP)))

(declare emit)

(defn separated-by [sep tokens]
  (when (seq tokens)
    (emit (first tokens))
    (doseq [token (rest tokens)] (write! sep) (emit token))))

(defn commas [tokens] (separated-by ", " tokens))
(defn comma-list [xs] (in-parens (commas xs)))
(defn emit-regex [s] (write! "/") (write! s) (write! "/"))

(defn emit-body [tokens]
  (if (empty? tokens)
    (write! "{;}")
    (in-braces (doseq [t tokens]
                 (NL)
                 (TAB)
                 (emit t)
                 (when-not (= :COMMENT (first t))
                   (SC))))))

(defn emit-body* [tokens]
  (doseq [t tokens]
    (emit t) (SC) (NL)))

(defn global [x y]
  (str constants/NEWARK "[\"" x "::" y "\"]"))

(defn label [x y]
  (str "label_" x "_" y))

(defn local [x y]
  (str "x_" x "_" y))

(defn arg [x y]
  (str "a_" x "_" y))

(defn emit-label [[_ x y]]
  (write! (str (label x y) ":")))

(defn emit-break [[_ x y]]
  (write! (str "break " (label x y))))

(defn if-token? [t] (= :IF (first t)))

(defn emit-if [test then else]
  (write! "if (")
  (emit test)
  (write! " == null || ")
  (emit test)
  (write! " === false)")
  (SP)
  (emit-body then)
  (when (seq else)
    (write! " else ")    
    (if (and (if-token? (first else))
             (= 1 (count else)))
      (apply emit-if (rest (first else)))      
      (emit-body else))))

(defn emit-operator [opsym tokens]
  (case (count tokens)
    0 (throw (Exception. (str "empty arglist for operator " opsym)))
    1 (in-parens (write! opsym) (SP) (emit (first tokens)))
    (in-parens (separated-by (str " " opsym " ") tokens))))

(defn emit-literal [x]
  (cond
   (nil? x)    (write! "null")
   (symbol? x) (write! (name x))
   (string? x) (write! (pr-str x))
   :else       (write! x)))

(defn emit-try-catch [t v c & [f]]
  (write! "try ")
  (emit-body t)
  (write! " catch (")
  (emit v)
  (write! ") ")
  (emit-body c)
  (when f
    (write! " finally ")
    (emit-body f)))

(defn emit-&rest [target offset]
  (let [[_ x y] target
        target  (local x y)]
    (write! (str target " = [];"))
    (NL) (TAB)
    (write! (str target".length = arguments.length-"offset";"))
    (NL) (TAB)
    (write!
     (str "for (var i = "offset", ii = arguments.length; i<ii; i++) { "))
    (indent!)
    (NL) (TAB)
    (write! (str target "[i-"offset"] = arguments[i];"))
    (unindent!)
    (NL) (TAB)
    (write! "}")))

(defn emit-labeled-block [loop? label sentinel body]
  (when sentinel
    (write! "try {")
    (indent!) (NL) (TAB))

  (emit label)
  (write! ":")
  (when loop? (write! "for (;;) "))
  (emit-body body)

  (when sentinel
    (unindent!) (NL) (TAB)
    (write! "} catch (e) {")
    (indent!) (NL) (TAB)
    (write! "if (")
    (emit sentinel)
    (write! ") { throw e; } ")
    (unindent!) (NL) (TAB)
    (write! "} finally {")
    (indent!) (NL) (TAB)
    (emit sentinel)
    (write! " = false;")
    (unindent!)
    (NL) (TAB)
    (write! "}")))

(defn emit [[tag a b c d e :as token]]
  (case tag
    :VAL      (emit-literal a)
    :RAW      (write! a)
    :REGEX    (do (write! "/") (emit a) (write! "/"))
    :ARRAY    (in-brackets (commas a))
    :FIELD    (do (emit a) (in-brackets (emit b)))
    :CALL     (do (emit a) (comma-list b))
    :NEW      (do (write! "new ") (emit a) (comma-list b))
    :BLOCK    (emit-labeled-block false a b c)
    :LOOP     (emit-labeled-block true a b c)
    :FN       (in-parens
               (write! "function ")
               (comma-list a)
               (SP)
               (emit-body b))
    :IF       (emit-if a b c)
    :SET!     (do (emit a) (write! " = ") (emit b))
    :OPCALL   (emit-operator a b) 
    :BREAK    (emit-break a)
    :RETURN   (do (write! "return ") (emit a))
    :DECLARE  (when (seq a)
                (write! "var ")
                (commas a))
    :TRY      (emit-try-catch a b c)
    :TRY*     (emit-try-catch a b c d)
    :THROW    (do (write! "throw ")
                  (emit a))
    :COMMENT  (do (write! "\n/* ")
                  (write! a)
                  (write! " */\n"))

    :NON_LOCAL_EXIT
    (do (write! "throw new Error('NON_LOCAL_EXIT')"))
    
    :FOR_EACH_PROPERTY
    (do (write! "for ")
        (in-parens (emit a) (write! " in ") (emit b))
        (SP)
        (emit-body c))

    :ARG
    (write! (arg a b))

    :LABEL
    (write! (label a b))
    
    :LOCAL
    (write! (local a b))

    :GLOBAL
    (write! (global a b))

    :&REST
    (emit-&rest a b)))

(defn emit-tokens [tokens]
  (clear!)
  (reset-depth!)
  (doseq [t tokens] (emit t))
  (str accumulator))

(defn emit-token [token]
  (clear!)
  (reset-depth!)
  (emit token)
  (write! ";")
  (str accumulator))

(defn emit-tokens* [tokens]
  (clear!)
  (reset-depth!)
  (emit-body* tokens)
  (str accumulator))