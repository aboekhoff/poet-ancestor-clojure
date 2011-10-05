(ns newark.reader
  (:refer-clojure :exclude [read-string])
  (:require [clojure.java.io :as io]))

(declare read-form read-whitespace set-reader-macro)

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

(defn get-position [port]
  (select-keys @port [:origin :offset :line :column]))

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

(def macros (atom {}))

(defmulti set-reader-macro (fn [x f] (type x)))

(defmethod set-reader-macro Character [x f]
  (swap! macros assoc x f))

(defmethod set-reader-macro String [x f]  
  (doseq [c x] (set-reader-macro c f)))

(defmethod set-reader-macro clojure.lang.PersistentHashSet [x f]
  (doseq [c x] (set-reader-macro c f)))

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

(defn read-list [port]
  (read-delimited-list
   port
   (fn [port]
     (read-whitespace port)
     (if (= \) (peek-char port))
       (do (read-char port) true)
       false))
   (fn [x] (apply list x))
   "list"))

(set-reader-macro \( read-list)
(set-reader-macro \) (mismatched-delimiter \]))

(defn read-vector [port]
  (read-delimited-list
   port
   (fn [port]
     (read-whitespace port)
     (if (= \] (peek-char port))
       (do (read-char port) true)
       false))
   vec
   "array"))

(set-reader-macro \[ read-vector)
(set-reader-macro \] (mismatched-delimiter \]))

(defn read-whitespace [port]
  (loop [in-comment? false]
    (let [c (read-char port)]
      (case c
        \space    (recur in-comment?)
        \tab      (recur in-comment?)
        \newline  (recur false)
        \formfeed (recur false)
        \return   (recur false)
        \;        (recur true)
                  (if in-comment?
                    (recur true)
                    (when c (unread-char port)))))))

(set-reader-macro " \n\r\f\t\b;" read-whitespace)

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

(set-reader-macro \" read-string-literal)

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
   (with-meta (symbol nil s) {:source-position p})))

(defn read-atom [port]
  (let [position (get-position port)]
    (loop [chars []]
      (let [c (peek-char port)]        
        (if (or (@macros c) (nil? c))         
          (parse-atom (apply str chars) position)
          (recur (conj chars (read-char port))))))))

(defn read-form [port]
  (let [c (peek-char port)]
    (cond
     (nil? c)    ::EOF
     (@macros c) (or ((@macros c) port) (recur port))
     :else       (read-atom port))))

(defn read-all-forms [port] 
  (loop [forms []]
    (let [form (read-form port)]
      (if (= form ::EOF)
        forms
        (recur (conj forms form))))))

(defn read-string [string]
  (read-form (string->input-port string)))

(defmulti read-file type)

(defmethod read-file String [x]
  (read-file (io/file x)))

(defmethod read-file java.io.File [x]
  (when (.exists x)
    (read-all-forms (string->input-port (slurp x)))))



