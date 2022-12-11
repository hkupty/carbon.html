(ns carbon.v2.opts)

(defn get-opts
  "Returns a tuple with opts and the input"
  [input]
  (let [maybe-opts (first input)]
    (if (map? maybe-opts)
      [maybe-opts (rest input)]
      [nil input])))

(defn with-opts [tree opts]
  (cond
    (nil? opts) tree
    (empty? opts) tree
    (map? (second tree)) (update tree 1 merge opts)
    :else (into [(first tree) opts] (rest tree))))

