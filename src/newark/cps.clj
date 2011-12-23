(ns newark.cps)

(declare ->cps ->cps*)

(def gen-kont
  (let [id (atom 0)]
    (fn []
      (swap! id inc)
      [(symbol (str "k" @id))
       (symbol (str "r" @id))])))

(defn ->cps* [f xs ys c]
  (if (empty? xs)
    [:APP f c (vec (reverse ys))]
    (let [[k r] (gen-kont)]
      [:LETK k r       
       (->cps* f (rest xs) (cons r ys) c)
       (->cps (first xs) k)])))

(defn ->cps [[tag x1 x2 x3 :as x] c]
  (prn x1)
  (case tag
    :CONST [:APPK c x]
    :VAR   [:APPK c x]
    :SET!  (let [[k r] (gen-kont)]
             [:LETK k r [:SET! x1 r] (->cps x2 k)])
    :IF    (let [[k r] (gen-kont)]
             [:LETK k r [:IF r (->cps x2 c) (->cps x3 c)] (->cps x1 k)])
    :BEGIN (cond
            (empty? x1)        [:APPK x [:CONST nil]]
            (empty? (rest x1)) (->cps (first x1) c)
            :else              (let [[k r] (gen-kont)]
                                 [:LETK k r
                                  (->cps [:BEGIN (rest x1)] c)
                                  (->cps (first x1) k)]))
    :LAMBDA (let [[k _] (gen-kont)]
              [:APPK c [:LAMBDA k x1 (->cps x2 k)]])

    :CALL ))