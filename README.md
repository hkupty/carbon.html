# carbon.html

Carbon is a component system on top of the excellent [hiccup](https://github.com/weavejester/hiccup), for generating HTML.

## Sample

```clj
(require '[carbon.tags :refer [component process-tree]])

(def my-component
  (component
    '[css-class]
    '[:div {:class css-class} "hello, carbon"]))

(process-tree '[:my-component "box"] {} {:my-component my-component})
;; -> [:div {:class "box"} "hello, carbon"]
```

## Structure and convention

Carbon allows you to define components as simple functions. In fact, the `component` function in `carbon.tags` returns a function.
Your component must be defined as function taking three arguments, being them the child arguments of your component, the context and the other components that
can be mounted. It is expected to return data that is valid in that context (usually, html in hiccup format):

```clj
(defn title [[text] _context _components]
  [:h2.title text])
```

Carbon will ensure the following:
- All vectors in hiccup format will be processed:
  - if there is a component with that key name, it will be mounted;
- Maps will have their values processed, not their keys:
- Symbols will be replaced by values in the context map.


So, in the following example:

```clj
(defn title [[text] _context _components]
  [:h2.title text])

(process-tree '[:title text] {:text "Hello, carbon!"} {:title title})
;; -> [:h2.title "Hello, carbon!"]
```

Note that the variable is referred as a _symbol_ in the component, but declared as a _keyword_ in the context map.
The context map can contain nested values, which will be visible to the components through namespaced keywords:

```clj
(defn title [[text] _context _components]
  [:h2.title text])

(process-tree '[:title app/title] {:app {:title "Hello, carbon!"}} {:title title})
;; -> [:h2.title "Hello, carbon!"]
```

Finally, the context map can also take namespaced keywords if necessary and it will also match accordingly:

```clj
(defn title [[text] _context _components]
  [:h2.title text])

(process-tree '[:title app/title] {:app/title "Hello, carbon!"} {:title title})
;; -> [:h2.title "Hello, carbon!"]
```

If a value is not found, usually carbon will throw an error. If, however, you accept that the value can be null
in that place, you can explicitly tag that symbol as null:

```clj
(defn title [[text] _context _components]
  [:h2.title text])

(process-tree '[:title ^:nullable? app/title] {} {:title title})
;; -> [:h2.title]
```

## Default functions

Carbon ships with a few default components:

### `:for`

Simple for-loop.

```clj
(require '[carbon.syntax :refer [default-tags])
(process-tree
  '[:div
    [:for [-v data]
     [:p -v]]]
  {:data ["hello" "Carbon"]}
  default-tags)
;; -> [:div [:p "hello"] [:p "Carbon"]]
```

When applied to maps, _it will iterate over the keys_:

```clj
(require '[carbon.syntax :refer [default-tags])
(process-tree
  '[:div
    [:for [-v data]
     [:p -v]]]
  {:data {:height 100 :width 200}}
  default-tags)
;; -> [:div [:p :height] [:p :width]]
```

If you want to iterate over the values of a map, use `:forv`:

```clj
(require '[carbon.syntax :refer [default-tags])
(process-tree
  '[:div
    [:forv [-v data]
     [:p -v]]]
  {:data {:height 100 :width 200}}
  default-tags)
;; -> [:div [:p 100] [:p 200]]
```

### `:if` & `:when`

Just like clojure, carbon ships with `:if` and `:when`:

```clj
(process-tree
 '[:div
   [:when data? {:class "special"}]
   [:if data? [:p data] [:span "Nothing to show"]]]
   {:data? true
    :data "..."}
    default-tags)
;; -> [:div {:class "special"} [:p "..."]]

(process-tree
 '[:div
   [:when data? {:class "special"}]
   [:if data? [:p data] [:span "Nothing to show"]]]
   {:data? false}
    default-tags)
;; -> [:div [:span "Nothing to show"]]
```

### `:with`

Similar to clojure's let, declares a variable that will be present only in that scope

```clj

(defn random-number [[-max] _ctx _components] (rand-int -max))

(process-tree
  '[:with [rnd [:random-number 100]]
    [:p "I got " rnd " as result"]]
    {}
    (merge default-tags {:random-nubmer random-number}))
;; -> [:p "I got " 42 " as result"]
```

### Other

There are more tags and documentation for those will come in the future. Please refer to `carbon.syntax` for all the tags.

## Higher level abstractions

The examples so far were using the lower level abstractions. Carbon provide a few high level abstractions, such as `component` that we've seen before.

`component` is just a wrapper over an anonymous function that ensures:
- The tree is processed so child elements are processed as well;
- If options are supplied to it, it will be applied accordingly;
- The context map is build correctly for that context

A wise idea is to read components from edn files directly into a map, like this:

```clojure
(require '[clojure.java.io :as io]
         '[carbon.tags :as tags])

(def components
  (into {}
        (comp
          (filter is-file?)
          (filter is-edn?)
          (map io/reader)
          (map (fn [^java.io.BufferedReader reader]
                 (java.io.PushbackReader. reader)))
          (map edn/read)
          (map (fn [[-key args tree]]
                 [-key (tags/component args tree)])))
        (file-seq (io/file "components"))))
```

This will get all files in the following format:
```clojure
[:tag [arg] [:h2.title arg]]
```

And will add to the `components` map with the key as declared in the edn file.

In the `carbon.processor` namespace, there are a few available functions such as `render` and `render-page`, which
do include the `default-tags` when processing the tree.
