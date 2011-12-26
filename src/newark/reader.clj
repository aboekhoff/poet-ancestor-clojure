(ns newark.reader
  (:refer-clojure :exclude [read-string])
  (:require [clojure.java.io :as io])
  (:require [newark.env :as env]))

(declare read-form
         read-whitespace
         set-reader-macro
         read-list
         read-vector)

(defn base-symbol [name]
  (env/paint (symbol name) env/base-color))

(def macros (atom {}))

(defn string->input-port [s & [origin]]
  (atom {:origin origin
         :offset 0
         :line   1
         :column 1
         :chars  (vec s)
         :prev   nil}))

(def newline? #{\newline \return \formfeed})

(defn unread-char [port]
  (reset! port (merge @port (:prev @port)))
  nil)

(defn peek-char [port]
  (get (@port :chars) (@port :offset)))

(defn read-char [port]
  (when-let [c (peek-char port)]
    (swap! port assoc
           :prev   (dissoc @port :prev :chars)
           :offset (inc (:offset @port)))
    (if (newline? c)
      (swap! port assoc :line (inc (@port :line)) :column 1)
      (swap! port update-in [:column] inc))
    c))

(defn read-until [predicate port]
  (loop [chars []]
    (let [c (peek-char port)]
      (if (predicate c)
        (apply str chars)
        (recur (conj chars (read-char port)))))))

(defn terminal? [x]
  (or (nil? x) (@macros x)))

(defn get-position [port]
  (select-keys @port [:origin :offset :line :column]))

(def dispatch-macros (atom {}))

(defn set-dispatch-macro! [dispatch-char macro]
  (swap! dispatch-macros assoc dispatch-char macro))

(defn immediate-value [strings value]
  (let [validator (set strings)]
    (fn [port]
      (let [string (read-until terminal? port)]
        (if (get validator string)
          value
          (throw (RuntimeException.
                  (str "invalid dispatch macro: #" string))))))))

(defn read-dispatch-macro [port]
  (let [position (get-position port)
        _        (read-char port)]
    (if-let [macro (get @dispatch-macros (peek-char port))]
      (macro port)
      (throw (RuntimeException.
              (str "invalid dispatch macro: #" (peek-char port) " at " position))))))

(defn eof! [port]
  (throw (RuntimeException.
          (str "unexpexted eof at " (get-position port)))))

(defn mismatched-delimiter [delim]
  (fn [port]
    (throw (RuntimeException.
            (str "mismatched delimiter: " delim
                 " at " (get-position port))))))

(defn unclosed-list-error [descriptor]
  (fn [position]
    (throw (RuntimeException.
            (str "unclosed " descriptor " at " position)))))

(defn read-delimited-list [port end? process descriptor]
  (let [position (get-position port)]
    (read-char port)
    (loop [elements []]     
      (if (end? port)
        (with-meta (process elements) {:source-position position})
        (let [elt (read-form port)]
          (if (= elt ::EOF)
            (unclosed-list-error descriptor position)
            (recur (conj elements elt))))))))

(def open->close {\( \) \[ \] \{ \}})

(defn read-list [port]
  (let [end-char (open->close (peek-char port))]
    (read-delimited-list
     port
     (fn [port]
       (read-whitespace port)
       (if (= end-char (peek-char port))
         (do (read-char port) true)
         false))
     (fn [x] (apply list x))
     "list")))

(defn read-vector [port]
  (vec (read-list port)))

(defn read-whitespace [port]
  (loop [in-comment? false]
    (let [c (read-char port)]
      (case c
        nil       nil
        \space    (recur in-comment?)
        \tab      (recur in-comment?)
        \newline  (recur false)
        \formfeed (recur false)
        \return   (recur false)
        \;        (recur true)
                  (if in-comment?
                    (recur true)
                    (unread-char port))))))

(def escape-map
  {\n \newline
   \r \return
   \f \formfeed
   \t \tab
   \b \backspace
   \" \"
   \\ \\})

(defn read-string-literal [port]
  (read-char port)
  (let [position (get-position port)]
    (loop [chars []]
      (let [c (read-char port)]
        (case c
              nil (unclosed-list-error "string" position)
              \"  (apply str chars)
              \\  (recur (conj chars (escape-map (read-char port))))
                  (recur (conj chars c)))))))

(defn parse-atom [s p]
  (cond
   (re-matches #"^(\+|-)?(0|([1-9][0-9]*))$" s)
   (Integer/parseInt s)
   
   (re-matches #"^(\+|-)?0[xX][0-9a-fA-F]+$" s)
   (Integer/parseInt (apply str (drop 2 s)) 16)
   
   (re-matches #"^(\+|-)?0[0-7]+$" s)
   (Integer/parseInt s 8)
   
   (re-matches #"^(\+|-)?(0|([1-9][0-9]*))\.[0-9]+$" s)
   (Double/parseDouble s)
   
   :else
   (if (= \: (first s))
     (keyword nil (apply str (rest s)))     
     (with-meta (symbol nil s) {:source-position p}))))

(defn read-atom [port]
  (let [position (get-position port)
        string   (read-until terminal? port)]
    (parse-atom string position)))

(defn read-form [port]
  (read-whitespace port)
  (let [c (peek-char port)]
    (cond
     (nil? c)    ::EOF
     (@macros c) ((@macros c) port)
     :else       (read-atom port))))

(defn read-all-forms [port] 
  (loop [forms []]
    (let [form (read-form port)]
      (if (= form ::EOF)
        forms
        (recur (conj forms form))))))

(defn read-string [string]
  (read-form (string->input-port string)))

(defn read-quote [port]
  (read-char port)
  (let [form (read-form port)]
    (list (base-symbol "quote") form)))

(defmulti read-file type)

(defmethod read-file String [x]
  (read-file (io/file x)))

(defmethod read-file java.io.File [x]
  (when (.exists x)
    (read-all-forms (string->input-port (slurp x) (.getName x)))))

(defmulti set-reader-macro (fn [x f] (type x)))

(defmethod set-reader-macro Character [x f]
  (swap! macros assoc x f))

(defmethod set-reader-macro String [x f]  
  (doseq [c x] (set-reader-macro c f)))

(defmethod set-reader-macro clojure.lang.PersistentHashSet [x f]
  (doseq [c x] (set-reader-macro c f)))

(set-reader-macro \# read-dispatch-macro)

(set-reader-macro \' read-quote)

(set-reader-macro \( read-list)
(set-reader-macro \[ read-list)
(set-reader-macro \{ read-list)

(set-reader-macro \) (mismatched-delimiter \]))
(set-reader-macro \] (mismatched-delimiter \]))
(set-reader-macro \} (mismatched-delimiter \]))

(set-reader-macro \" read-string-literal)
(set-reader-macro " \n\r\f\t\b;" read-whitespace)

(set-dispatch-macro! \t (immediate-value ["t" "true"] true))
(set-dispatch-macro! \f (immediate-value ["f" "false"] false))
(set-dispatch-macro! \n (immediate-value ["nil"] nil))
(set-dispatch-macro! \( read-vector)
(set-dispatch-macro! \[ read-vector)
(set-dispatch-macro! \{ read-vector)