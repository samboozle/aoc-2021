(ns advent-clj.december.dec-03
  (:require [helpers :refer [binary->decimal product]]))

(def parse-input
  (partial map seq))

(def frequencies-map
  (partial apply map (comp frequencies vector)))

(defn gamma-epsilon [coll]
  (reduce
   (fn [acc frq]
     (map conj acc (reverse (sort-by #(frq %) (keys frq)))))
   [[] []]
   (frequencies-map coll)))

(defn oxygen [coll]
  (loop [idx                  0
         [head & _ :as coll'] coll]
    (let [out-of-bounds?    (= (count head) idx)
          {zros \0 ones \1} (if out-of-bounds?
                              {}
                              (group-by #(nth % idx) coll'))
          zros'             (count zros)
          ones'             (count ones)
          zros-win?         (> zros' ones')]
      (cond
        (= 1 (count coll')) head
        zros-win?           (recur (inc idx) zros)
        :else               (recur (inc idx) ones)))))

(defn carbon-dioxide [coll]
  (loop [idx                  0
         [head & _ :as coll'] coll]
    (let [out-of-bounds?    (= (count head) idx)
          {zros \0 ones \1} (if out-of-bounds?
                              {}
                              (group-by #(nth % idx) coll'))
          zros'             (count zros)
          ones'             (count ones)
          ones-win?         (> zros' ones')]
      (cond
        (= 1 (count coll')) head
        ones-win?           (recur (inc idx) ones)
        :else               (recur (inc idx) zros)))))

(defn stars [input]
  (let [input' (parse-input input)]
    [(->> (gamma-epsilon input')
          (map binary->decimal)
          product)
     (product [(binary->decimal (oxygen         input'))
               (binary->decimal (carbon-dioxide input'))])]))
