(ns interpreter.common)

(defn- record-keys
  [record-symbol]
  (let [ns (namespace record-symbol)
        map-to-record-fn (->> record-symbol
                              name
                              (str "map->")
                              (symbol ns)
                              symbol
                              resolve)]
    (->> (map-to-record-fn {})
         keys
         (map (comp symbol name)))))

(defn- all-records-keys
  [record-symbols]
  (->> record-symbols
       (map record-keys)
       flatten
       distinct))

(defmacro defrecord+
  [name# fields & parent-records]
  (let [parent-fields (all-records-keys parent-records)
        merged-fields (concat parent-fields fields)]
    `(defrecord ~name# [~@merged-fields])))
