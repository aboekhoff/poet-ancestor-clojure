(ns newark.syntax
  (:use [newark.env :only [sanitize make-color make-environment symbol->key]]))

(def dots (symbol "..."))
(defn dots? [x] (= x dots))

(defn shortest-branch [m]
  (if-let [ns (seq (map (comp count val) m))]
    (apply min ns)
    0))

(defn map-vals [f m] (into {} (for [[k v] m] [k (f v)])))

(defn reducer
  ([f xs] (if (empty? xs)
            (f)
            (reducer f (last xs) (butlast xs))))
  ([f x xs] (reduce (fn ([x y] (f y x))) x (reverse xs))))

(defn with-meta* [obj newmeta]
  (if (instance? clojure.lang.IMeta obj)
    (let [oldmeta (meta obj)]
      (with-meta obj (merge oldmeta newmeta)))
    obj))

;; let's assume all patterns all well formed
;; it will be easier to validate a pattern separately

(defn extract-symbols
  ([p] (extract-symbols p '()))
  ([p acc]
     (cond
      (dots? p)    acc
      (symbol? p)  (cons (symbol->key p) acc)
      (seq? p)     (concat acc (mapcat extract-symbols p))
      (vector? p)  (concat acc (mapcat extract-symbols p))
      :else        acc)))

(defn combine-k-v [m [k v]]
  (let [coll (or (get m k) [])]
    (assoc m k (conj coll v))))

(defn combine-1 [m n]
  (reduce combine-k-v m n))

(defn combine [ms]
  (reduce combine-1 {} ms))

(declare match-sexp match-seq match-dots match-dots*)

(defn match [p t] (match-sexp p t))

(defn match-sexp [p t]
  (cond
   (symbol? p)  {(symbol->key p) t}
   (vector? p)  (when (vector? t)
                  (if (and (empty? p) (empty? t))
                    {}
                    (match-seq p t)))
   (seq? p)     (when (seq? t)
                  (if (and (empty? p) (empty? t))
                    {}
                    (match-seq p t))
                  (match-seq p t))
   :else        (when (= p t) {})))

(defn match-seq [[a b & more :as p] [x & xs :as t]]
  (cond
   (empty? p) (when (empty? t) {})
   (dots? b)  (let [[matches t*] (match-dots a t)]
                (merge matches (match-seq more t*)))
   :else      (when (seq t)                
                (when-let [a (match-sexp (first p) (first t))]
                  (when-let [b (match-seq (rest p) (rest t))]
                    (merge a b))))))

(defn match-dots [p ts]
  (loop [ts ts acc []]
    (if (empty? ts)
      [(combine acc) '()]
      (let [a (match-sexp p (first ts))]
        (if a
          (recur (rest ts) (conj acc a))
          [(combine acc) ts])))))

;; now for templates
;; need to produce functions that take a map and return a sexp

(declare compile-template compile-template* next-pat)

(defn compile-template [x ids]
  (cond
   (symbol? x) (let [x* (symbol->key x)]
                 (if (get ids x*)
                   [:GET x*]
                   [:PUT x]))
   
   (seq? x)    (if (empty? x)
                 [:PUT x]
                 [:SEQ (compile-template* x ids)])
   
   (vector? x) (if (empty? x)
                 [:PUT x]
                 [:VEC (compile-template* x ids)])
   
   :else       [:PUT x]))

(defn compile-template* [xs ids]
  (loop [xs xs acc []]
    (if-let [[a b] (next-pat xs ids)]
      (recur b (conj acc a))
      acc)))

(defn next-pat [[x y :as xs] ids]
  (cond
   (empty? xs) nil
   (dots? y)   (let [idents (set (extract-symbols x))
                     [a b]  (next-pat (cons x (drop 2 xs)) ids)]
                 [[:SPLICE a idents] b])
   :else       [(compile-template x ids) (rest xs)]))

(declare expand-template expand-template* expand-splice)

(defn expand-template [[tag content targets] data]
  (case tag
    :PUT content
    :GET (get data content)
    :SEQ (mapcat #(expand-template* % data) content)
    :VEC (vec (mapcat #(expand-template* % data) content))))

(defn expand-template* [[tag content targets :as template] data]
  (if (= tag :SPLICE)
    (expand-splice content targets data)
    (list (expand-template template data))))

(defn expand-splice [template targets data]
  (let [data  (select-keys data targets)
        times (shortest-branch data)]
    (loop [counter 0
           data    data
           acc     []]
      (if (< counter times)
        (let [data* (map-vals first data)]
          (recur (inc counter)
                 (map-vals rest data)
                 (apply conj acc (expand-template* template data*))))
        (seq acc)))))

(defn make-matcher-1 [pattern template]
  (let [idents   (extract-symbols pattern)
        template* (compile-template template (set idents))]
    (fn [fail]
      (fn [input]
        (if-let [data (match-sexp pattern (rest input))]
          (expand-template template* data)
          (fail input))))))

(defn gen-err [pats]
  (fn [input]
    (let [position (-> input meta :position)]
      (throw (RuntimeException.
              (apply str
                     "syntax error on input: " (str input) "\n"
                     (when position (str position "\n"))
                     "no match found among patterns:\n"
                     (interpose "\n" pats)))))))

(defn make-matcher* [pts]
  (let [pats     (map first pts)
        pats*    (map rest pats)
        tmps     (map second pts)
        matchers (map make-matcher-1 pats* tmps)
        err      (gen-err pats)]
    (reducer (fn [f g] (f g)) err matchers)))

(defn make-syntax [defining-env pts]
  (let [matcher (make-matcher* pts)]
    (fn [calling-env input]
      (let [color (make-color defining-env calling-env)]
        (-> input           
            (sanitize color)
            (matcher)          
            (sanitize color)
            (with-meta* {:position (-> input meta :position)}))))))

(defn make-symbol-syntax [defining-env template]
  (let [template* (compile-template template #{})]
    (fn [calling-env input]
      (let [color   (make-color defining-env calling-env)
            output  (expand-template template* {})
            output* (sanitize output color)]
        output*))))
