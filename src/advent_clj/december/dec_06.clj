(ns advent-clj.december.dec-06
  (:require [helpers :refer [comma sum]]))

(def parse-input
 (comp (partial map read-string) comma first))

;; slow prototype
(defn exponentiate-fish [starting-generation]
  (nth (iterate (fn [fish]
                  (println (into (sorted-map) (merge-with + {0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0} (frequencies fish))))
                  (concat (map (fn [age] (if (zero? age) 6 (dec age))) fish) (repeat (count (filter zero? fish)) 8))) starting-generation) 80))

;; much faster
(defn pass-day [generation]
  (reduce (fn [prev day]
            (case day
              0 (assoc prev 8 (get generation 0))
              7 (assoc prev 6 (+ (get generation 0) (get generation 7)))
                (assoc prev (dec day) (get generation day))))
          generation
          (range 0 9)))

(defn exponentiate-fish' [starting-generation]
  (iterate pass-day (merge-with + {0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0} (frequencies starting-generation))))

(defn stars [input]
  [(sum (vals (nth (exponentiate-fish' (parse-input input)) 80)))
   (sum (vals (nth (exponentiate-fish' (parse-input input)) 256)))])
