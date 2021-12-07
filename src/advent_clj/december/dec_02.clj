(ns advent-clj.december.dec-02
  (:require [helpers :refer [product words]]))

(def parse-input
  (partial map (comp (partial apply #(vector (keyword %1) (read-string %2))) words)))

(defn change-point [[hrz dep aim :as pos] [dir amp]]
  (case [dir (count pos)]
    [:forward 3] [(+ hrz amp) (+ dep (* aim amp)) aim]
    [:forward 2] [(+ hrz amp) dep]
    [:down 3]    [hrz dep (+ aim amp)]
    [:down 2]    [hrz (+ dep amp)]
    [:up 3]      [hrz dep (max (- aim amp) 0)]
    [:up 2]      [hrz (max (- dep amp) 0)]))

(defn stars [input]
  (let [input' (parse-input input)]
    [(product (reduce change-point [0 0] input'))
     (product (take 2 (reduce change-point [0 0 0] input')))]))
