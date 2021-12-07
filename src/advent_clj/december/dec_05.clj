(ns advent-clj.december.dec-05
  (:require [helpers :refer [comma split-on]]))

(def parse-input
  (partial map (comp (partial map (comp (partial map read-string)
                                        comma))
                     (split-on #"\s->\s"))))

(defn populate-lines [coordinates]
  (letfn [(make-line
            [[lo hi] other]
            (map #(list % other) (range lo (inc hi))))]
    (reduce (fn [acc [[x1 y1] [x2 y2]]]
              (cond
                (= x1 x2) (into acc (make-line (sort [y1 y2]) x1))
                (= y1 y2) (into acc (map reverse (make-line (sort [x1 x2]) y1)))
                :else     acc))
            []
            coordinates)))

(defn stars [input]
  [(->> (parse-input input)
        populate-lines
        frequencies
        (filter (fn [[_ v]] (> v 1)))
        count)])
