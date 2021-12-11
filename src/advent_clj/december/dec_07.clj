(ns advent-clj.december.dec-07
  (:require [helpers :refer [abs comma triangle]]))

(def parse-input
 (comp (partial map read-string) comma first))

;; this was slow for star 2, but it yielded the correct answer
;; until I memoized 'triangle :)
(defn min-gas [input triangles?]
  (apply min
         (for [x (range (apply min input) (inc (apply max input)))]
           (reduce + (map #((if triangles? triangle identity) (abs (- x %))) input)))))

(defn stars [input]
  [(min-gas (parse-input input) false)
   (min-gas (parse-input input) true)])
