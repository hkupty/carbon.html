(ns carbon.debug)

(def ^:private debug? (atom (clojure.lang.PersistentQueue/EMPTY)))
(def ^:private debug-gate (atom false))

(defn open-debug-gate! [] (reset! debug-gate true))
(defn close-debug-gate! [] (reset! debug-gate false))
(defn push-debug-context! [context]
  (when @debug-gate
    (swap! debug? conj context)))

(defn pop-debug-context! []
  (when @debug-gate
    (swap! debug? pop)))

(defn tap [x] (println x) x)
(defn label [& args] (when-let [stack (seq @debug?)]
                       (apply println (vec stack) args))
  (last args))

(defmacro with-debug [debug-label debug-key & form]
  `(do
     (push-debug-context! ~debug-label)
     (label ~debug-label :start ~debug-key)
     (let [result# (do ~@form)]
       (label ~debug-label :finish ~debug-key :-> result#)
       (pop-debug-context!)
       result#)))
