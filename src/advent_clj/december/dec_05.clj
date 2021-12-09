(ns advent-clj.december.dec-05
  (:require [helpers :refer [comma split-on]]))

(def parse-input
  (partial map (comp (partial map (comp (partial map read-string)
                                        comma))
                     (split-on #"\s->\s"))))

(defn make-straight [[lo hi] other]
  (map (partial list other) (range lo (inc hi))))

(defn make-diagonal [a b fns]
  (loop [a' a acc []]
    (if (= a' b)
      (conj acc a')
      (recur (map #(%1 %2) fns a') (conj acc a')))))

(defn populate-lines [diagonals? coordinates]
  (reduce (fn [acc [[x1 y1 :as a] [x2 y2 :as b]]]
            (cond
              (= x1 x2)                                    (into acc (make-straight (sort [y1 y2]) x1))
              (= y1 y2)                                    (into acc (map reverse (make-straight (sort [x1 x2]) y1)))
              (and diagonals? (= (apply - a) (apply - b))) (into acc (make-diagonal a b (if (< x1 x2) [inc inc] [dec dec])))
              (and diagonals? (= (apply + a) (apply + b))) (into acc (make-diagonal a b (if (< x1 x2) [inc dec] [dec inc])))
              :else
              acc))
          []
          coordinates))

(defn lines-to-solution [input diagonals?]
  (->> (parse-input input)
       (populate-lines diagonals?)
       frequencies
       (filter (fn [[_ v]] (> v 1)))
       count))

(defn stars [input]
  [(lines-to-solution input false)
   (lines-to-solution input true)])
