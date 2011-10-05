(ns newark.syntax)

;; NOTE: this was copy pasted from an earlier similar project
;; still needs adaptation
;; since this implementation is intended only to bootstrap
;; the javascript hosted implementation
;; macros are nice, but not needed, since we can't just
;; extend this bootstrap compiler as we please
;; and add them in properly to the javascript hosted implementation

;; just syntax-rules
;; well, syntax rules without the 'keywords'
;; we prefer the use of actual keywords for 'scheme keywords'

(def dots (symbol "..."))
(defn dots? [x] (= x dots))

;; let's assume all patterns all well formed
;; it will be easier to validate a pattern separately

(defn extract-symbols
  ([p] (extract-symbols p '()))
  ([p acc]
     (cond
      (dots? p)       acc
      (literal? p)    acc
      (symbol? p) (cons p acc)
      (seq? p)        (concat acc (mapcat extract-symbols p))
      (vector? p)     (concat acc (mapcat extract-symbols p))
      :else           acc)))

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
   (symbol? p) {p t}
   (literal? p)    (when (matches-literal? p t) {})
   (vector? p)     (when (vector? t)
                     (if (and (empty? p) (empty? t))
                       {}
                       (match-seq p t)))
   (seq? p)        (when (seq? t)
                     (if (and (empty? p) (empty? t))
                       {}
                       (match-seq p t))
                     (match-seq p t))
   :else           (when (= p t) {})))

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
   (symbol? x) (if (get ids x) [:GET x] [:PUT x])
   (seq? x)        (if (empty? x)
                     [:PUT x]
                     [:SEQ (compile-template* x ids)])
   (vector? x)     (if (empty? x)
                     [:PUT x]
                     [:VEC (compile-template* x ids)])
   :else           [:PUT x]))

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

(defn create-matcher-1 [pattern template]
  (let [idents   (extract-symbols pattern)
        template (compile-template template (set idents))]
    (fn [fail]
      (fn [input]
        (if-let [data (match-sexp pattern (rest input))]
          (expand-template template data)
          (fail input))))))

(defn gen-err [pats]
  (fn [input]
    (apply raise!
      "syntax error on input:" (apply str input) "\n"
      "no match found among patterns:\n"
      (interpose "\n" pats))))

(defn create-matcher* [pts]
  (let [pts      (partition 2 pts)
        pats     (map first pts)
        pats*    (map rest pats)
        tmps     (map second pts)
        matchers (map create-matcher-1 pats* tmps)
        err      (gen-err pats)]
    (reducer (fn [f g] (f g)) err matchers)))

(defn create-syntax [env pts]
  (let [matcher (create-matcher* pts)]
    (fn [input]
      (let [color (env->color env)]
        (-> input
            (sanitize color)
            matcher
            (sanitize color))))))

;; TODO

(defn create-symbol-syntax [env template])

