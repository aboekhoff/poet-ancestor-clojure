(ns newark.compiler
  (:refer-clojure :exclude [compile extend]))

;; simple back-end
;; translate the expanded ast
;; into purely imperative form

(declare simplify simplify* nofx compile compile-block compile-fn)

(def next-id
  (let [id (atom 0)]
    #(swap! id inc)))

(defn make-scope []
  {:locals (atom #{})
   :block  (atom [])})

(defn push-local [[_ type :as local] scope]
  (when (not= type :GLOBAL)
    (swap! (:locals scope) conj local)))

(defn gen-local [scope & [name]]
  (let [local [:VAR :AUTO (next-id)]]
    (push-local local scope)
    local))

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

(defn finalize-scope [scope]
  (let [locals (deref (:locals scope))
        block  (deref (:block scope))]
    (if (empty? locals)
      block
      `[[:DECLARE ~locals] ~@block])))

(defn tracer-for [v]
  (fn [x] [:SET! v x]))

(defn return-tracer [x]
  [:RETURN x])

(defn simplify [[tag a b c :as node] e]  
  (case tag
    :RAW
    node
    
    :VAR
    node
        
    :CONSTANT
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

    :SET!
    (do (compile node nil e)
        [:CONSTANT nil])
    
    :DEF
    (do (compile node nil e)
        [:CONSTANT nil])    

    :WHILE
    (let [e*       (extend-scope e)
          testexpr (simplify a e*)
          testbody (-> e* :block deref)         
          loopbody (compile-block b nil e*)
          body     (conj testbody [:IF testexpr loopbody [[:BREAK]]])]
      (statement [:WHILE [:CONSTANT true] body] e)
      [:CONSTANT nil])

    :FOR_EACH_PROPERTY
    (let [b (simplify b e)
          c (compile-block c nil e)]
      (statement [:FOR_EACH_PROPERTY a b c] e)
      [:CONSTANT nil])

    :THROW
    (do (compile node nil e)
        [:CONSTANT nil])
    
    ;; default

    (let [v (gen-local e)]
      (compile node (tracer-for v) e)
      v)))

(defn simplify* [xs e]
  (loop [xs xs acc []]
    (if-let [[x & xs] (seq xs)]
      (recur xs (conj acc (simplify x e)))
      acc)))

(defn compile [[tag a b c d :as x] t e]
  (case tag        
    :CONSTANT
    (pure x t e)
        
    :VAR
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

    :WHILE
    (pure (simplify x e) t e)

    :FOR_EACH_PROPERTY
    (pure (simplify x e) t e)    
    
    :DEF
    (do (push-local a e)
        (compile b (tracer-for a) e)
        (pure [:CONSTANT nil] t e))
   
    :SET!    
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
    (pure (compile-fn a b) t e)

    :RAW
    (pure [:RAW a] t e)

    :THROW
    (let [a* (simplify a e)]
      (statement [:THROW a*] e))))

(defn compile-block [x t e]
  (let [scope (extend-scope e)]
    (compile x t scope)
    (deref (get scope :block))))

(defn compile-fn [params body]
  (let [scope  (make-scope)     
        tracer return-tracer
        _      (compile body tracer scope)
        body   (finalize-scope scope)]
    `[:FN ~params ~body]))

(defn compile-expansion [expansion]
  (let [scope (make-scope)]
    (compile expansion nil scope)
    (finalize-scope scope)))