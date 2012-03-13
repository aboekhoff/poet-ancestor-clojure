(ns newark.util)

(defn make-generator
  ([] (make-generator 0))
  ([seed]
    (let [value (atom seed)]
      (fn generator
        ([] (generator :inc))
        ([action & args]
           (case action
             :inc (swap! value inc)
             :get @value
             :set (reset! value (first args))))))))

(defn make-dictionary
  ([] (make-dictionary nil))
  ([parent] (cons (atom {}) parent)))

(defn extend-dictionary [dict]
  (make-dictionary dict))

(defn dictionary-get [dict key]
  (when (seq dict)
    (or (get (deref (first dict)) key)
        (recur (rest dict) key))))

(defn dictionary-set [dict key val]
  (swap! (first dict) assoc key val))

(defn dictionary-set* [dict key val]
  (swap! (last dict) assoc key val))

(defn dictionary-join [dict1 dict2]
  (concat dict1 dict2))

(defn merge-meta [x m]
  (with-meta x (merge (meta x) m)))