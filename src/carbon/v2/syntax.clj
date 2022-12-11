(ns carbon.v2.syntax
  (:require [carbon.v2.tags :as tags-v2]
            [carbon.v2.symbols :as syms]
            [carbon.v2.params :as params]
            [carbon.v2.opts :as opts]
            [carbon.util :refer [map-vals filter-keys]]
            [clojure.string :as str]))

(defn tap [x] (println x) x)

(defn keys-when-map [-kv] (cond-> -kv (map-entry? -kv) (key)))
(defn vals-when-map [-kv] (cond-> -kv (map-entry? -kv) (val)))

(defn produce-linear [processor]
  (fn -linearize ([data] (-linearize data {}))
    ([data linear]
     (let [[-k vs] (first data)
           iter (rest data)]
       (if (seq iter)
         (mapcat (fn [v] (-linearize iter (assoc linear -k v))) vs)
         (map (fn [v] (assoc linear -k (processor v))) vs))))))


(defn evaluate [expr]
  (cond
    (nil? expr) false
    (boolean? expr) expr
    (sequential? expr) (some? (seq expr))
    (map? expr) (some? (seq expr))
    (symbol? expr) (some? (some-> expr resolve))
    :else (some? expr)))

(defn slug [-str]
  (some-> -str
          (cond-> (keyword? -str) (name))
          (str/trim)
          (str/replace #"[ .]+" "_")
          (str/lower-case)))

(defn deslug [-str]
  (some-> -str
          (cond-> (keyword? -str) (name))
          (str/trim)
          (str/replace #"[-_.]+" " ")
          (str/split #" ")
          (->>
            (map str/capitalize)
            (str/join " "))))

(defn- process-map [-map-like base-map components]
  (into {}
         (comp
          params/xf-keyword-map
          (map-vals (fn [sym] (tags-v2/process-tree sym base-map components))))
        (cond-> -map-like
          (symbol? -map-like) (syms/process-symbol base-map))))

(defn pass-on [-map]
  (into {}
        (filter-keys keyword?)
        -map))


;;; Exported tags

(defn carbon-for [[binds & forms] base-map components]
  (let  [processed-map (process-map (partition 2 binds) base-map components)
         linearize (produce-linear keys-when-map)]
    (->> processed-map
         linearize
         (mapcat (fn [argmap]
                   (let [context (merge base-map argmap)]
                     (map (fn [form] (tags-v2/process-tree form context components))
                          forms)))))))
(defn carbon-forv [[binds & forms] base-map components]
  (let  [processed-map (process-map (partition 2 binds) base-map components)
         linearize (produce-linear vals-when-map)]
    (->> processed-map
         linearize
         (mapcat (fn [argmap]
                   (let [context (merge base-map argmap)]
                     (map (fn [form] (tags-v2/process-tree form context components))
                          forms)))))))

(defn carbon-with [[binds & forms] base-map components]
  (let [context (process-map (partition 2 binds) base-map components)]
    (map
      (fn [form] (tags-v2/process-tree form (merge base-map context) components))
      forms)))

(defn carbon-if [[pred form-true form-false] argmap components]
  (tags-v2/process-tree
    (if (evaluate (tags-v2/process-tree pred argmap components))
      form-true
      form-false)
    argmap
    components))


(defn carbon-when [[pred & forms] argmap components]
    (when (evaluate (tags-v2/process-tree pred argmap components))
      (map (fn [form] (tags-v2/process-tree form argmap components))
           forms)))

(defn carbon-slug [[sym] base-map components]
  (slug (cond-> sym
    (symbol? sym) (syms/process-symbol base-map))))

(defn carbon-deslug [[sym] base-map components]
  (deslug (cond-> sym
    (symbol? sym) (syms/process-symbol base-map))))

(defn carbon-id [[sym] base-map components]
  (->> (cond-> sym (symbol? sym) (syms/process-symbol base-map))
     (slug)
     (str "#")))

(defn carbon-set-option [[component opts] base-map components]
  (opts/with-opts (syms/process-symbol component base-map) opts))

(defn carbon-merge [[& maps] base-map components]
  (tap (params/merge-with-transform-nested
    {}
    cat
    (map #(tags-v2/process-tree % base-map components) maps))))

(defn carbon-defaults [[default-map form & all] base-map components]
  (let [-opts (pass-on default-map)]
    (-> form
        (tags-v2/process-tree (params/merge-params (params/as-keyword-map default-map) base-map) components)
        (cond-> (seq -opts) (opts/with-opts -opts)))))

(defn carbon-str [[& data] base-map components]
  (apply str (map #(tags-v2/process-tree % base-map components) data)))

(defn carbon-lines [[& data] _base-map _components]
  (str/join "\n" data))

(defn carbon-cond [[conditions selector] base-map components]
  (-> selector
      (tags-v2/process-tree base-map components)
      (conditions (:default conditions))
      (tags-v2/process-tree base-map components)))

(defn carbon-get [[-map -key] base-map components]
  (tags-v2/process-tree (get (cond
                              (symbol? -map) (syms/process-symbol -map base-map)
                              (map? -map) (process-map -map base-map components)
                              (vector? -map) (tags-v2/process-tree -map base-map components)) (tags-v2/process-tree -key base-map components))
                        base-map components))

(defn carbon-debug [[tree] base-map components]
  (tap [tree base-map])
  tree)

(defn carbon-select-first [[lookup value tree] base-map components]
  (first (filter (comp (partial = value) lookup)
        (tags-v2/process-tree tree base-map components))))

(defn carbon-html-escape [[-val] _base-map _components]
  (str/escape -val {\< "&lt;", \> "&gt;", \& "&amp;"}))


(def default-tags
  {:for carbon-for
   :forv carbon-forv
   :if carbon-if
   :select carbon-select-first
   :get carbon-get
   :debug carbon-debug
   :pick carbon-cond
   :when carbon-when
   :with carbon-with
   :merge carbon-merge
   :slug carbon-slug
   :deslug carbon-deslug
   :str carbon-str
   :lines carbon-lines
   :id carbon-id
   :with-defaults carbon-defaults
   :html-escape carbon-html-escape})
