(ns newark.compiler
  (:refer-clojure :exclude [compile extend]))

;; simple back-end
;; translate the expanded ast
;; into purely imperative form

(declare simplify simplify* nofx compile compile-block compile-fn)

(defn make-scope [level locals]
  {:level  level
   :locals (atom locals)
   :block  (atom [])})

(defn make-local [scope & [name]]
  [:LOCAL (:level scope) (swap! (:locals scope) inc) name])

(defn statement [statement scope]
  (swap! (:block scope) conj statement))

(defn expression [expression tracer scope]
  (let [expression (if tracer (tracer expression) expression)]
    (statement expression scope)))

(defn pure [expression tracer scope]
  (when tracer
    (statement (tracer expression) scope)))

(defn extend-scope [scope]
  (assoc scope :block (atom [])))

(defn make-locals [current end level]
  (if (< current end)
    (cons [:LOCAL level (inc current)] (make-locals (inc current) end level))))

(defn make-declaration [level locals index]
  (when (< index locals)
    (cons [:LOCAL level index] (make-declaration level locals (inc index)))))

(defn finalize-scope [scope]
  (let [locals (deref (:locals scope))
        level  (:level scope)
        block  (deref (:block scope))        
        decl   (make-declaration level (inc locals) 1)]
    (if decl
      `[[:DECLARE ~decl] ~@block]
      block)))

(defn tracer-for [v]
  (fn [x] [:SET v x]))

(defn return-tracer [x]
  [:RETURN x])

(defn simplify [[tag a b c :as node] e]  
  (case tag        
    :RAW
    node

    :CONSTANT
    node
    
    :ARG
    node

    :LOCAL
    node

    :GLOBAL
    node
        
    :PROJECT
    (let [a (simplify a e)
          b (simplify b e)]
      [:PROJECT a b])

    :ARRAY
    (let [a (simplify* a e)]
      [:ARRAY a])   

    :CALL
    (let [a (simplify a e)
          b (simplify* b e)]
      [:CALL a b])

    :NEW
    (let [a* (simplify a e)]
      [:NEW a])

    :OPCALL
    (let [b (simplify* b e)]
      [:OPCALL a b])

    :SET
    (do (compile node nil e) a)       

    :BLOCK
    (do (compile node nil e) a)

    :LOOP
    (do (compile node nil e) a)       

    :RETURN_FROM
    (do (compile node nil e) a)
    
    ;; this needs to be repaired
    :FOR_EACH_PROPERTY
    (let [b (simplify b e)
          c (compile-block c nil e)]
      (statement [:FOR_EACH_PROPERTY a b c] e)
      [:CONSTANT nil])

    :THROW
    (do (compile node nil e)
        [:CONSTANT nil])
    
    ;; default

    (let [v (make-local e)]
      (compile node (tracer-for v) e)
      v)))

(defn simplify* [xs e]
  (loop [xs xs acc []]
    (if-let [[x & xs] (seq xs)]
      (recur xs (conj acc (simplify x e)))
      acc)))

(defn compile [[tag a b c d :as x] t e]
  (case tag
    :&REST
    (let [len (make-local e)
          idx (make-local e)]
      (statement [:SET len [:PROJECT b [:CONSTANT "length"]]] e)
      (statement [:SET idx [:CONSTANT c]] e)
      (statement [:SET a [:CALL [:RAW "Array"] [[:OPCALL "-" [len idx]]]]] e)
      (statement [:SLICE a b c idx len] e))    
    
    :RAW
    (pure [:RAW a] t e)
        
    :CONSTANT
    (pure x t e)
    
    :ARG
    (pure x t e)
    
    :LOCAL
    (pure x t e)

    :GLOBAL
    (pure x t e)
    
    :ARRAY
    (let [a* (simplify* a e)]
      (pure [:ARRAY a*] t e))
    
    :BEGIN
    (let [xs (rest x)]
      (cond
       (empty? xs)
       (pure [:CONSTANT nil] t e)
       
       (empty? (rest xs))
       (compile (first xs) t e)
       
       :else
       (loop [xs xs]
         (let [[y & ys] xs]
           (if (empty? ys)
             (compile y t e)
             (do (compile y nil e)
                 (recur ys)))))))   
   
    :PROJECT
    (let [[a* b*] (simplify* (rest x) e)]
      (pure [:PROJECT a* b*] t e))

    :BLOCK
    (let [body (compile-block b (tracer-for a) e)]
      (statement [:BLOCK a body] e)
      (pure a t e))

    :LOOP
    (let [body (compile-block b nil e)]
      (statement [:LOOP a body] e)
      (pure a t e))

    :RETURN_FROM
    (let [b (or b [:CONSTANT nil])]
      (compile b (tracer-for a) e)
      (statement [:BREAK a] e))
    
    :FOR_EACH_PROPERTY
    (pure (simplify x e) t e)    
       
    :SET
    (do (compile b (tracer-for a) e)
        (pure [:CONSTANT nil] t e))     
   
    :IF
    (let [a* (simplify a e)
          b* (compile-block b t e)
          c* (compile-block c t e)]
      (statement [:IF a* b* c*] e))
    
    :OPCALL
    (let [b* (simplify* b e)]
      (pure [:OPCALL a b*] t e))

    :NEW
    (let [a* (simplify a e)]
      (expression [:NEW a*] t e))
   
    :CALL
    (let [a* (simplify a e)
          b* (simplify* b e)]
      (expression [:CALL a* b*] t e))
   
    :FN
    (pure (compile-fn a b c d) t e)

    :THROW
    (let [a* (simplify a e)]
      (statement [:THROW a*] e))))

(defn compile-block [x t e]
  (let [scope (extend-scope e)]
    (compile x t scope)
    (deref (get scope :block))))

(defn compile-fn [params level locals body]
  (let [scope  (make-scope level locals)     
        tracer return-tracer
        _      (compile body tracer scope)
        body   (finalize-scope scope)]
    [:FN params body]))

(defn compile-expansion [env expansion]
  (let [scope (make-scope (:level env) ((:next-local env) :get))]
    (compile expansion nil scope)
    (finalize-scope scope)))