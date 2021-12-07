(ns advent-clj.core
  (:require [clojure.java.io :as io]
            [helpers         :refer [lines]]))

(defn get-daily-input
  ([day]       (get-daily-input day false))
  ([day test?] (->> day
                    (format (str "input-%02d" (when test? "-test") ".txt"))
                    io/resource
                    slurp
                    lines)))

(defn solve-day [day]
  (vec ((resolve (symbol (format "advent-clj.december.dec-%02d/stars" day))) (get-daily-input day))))

(defn solve-days [up-to]
  (vec (map solve-day (take up-to (drop 1 (range))))))

(def SOLUTIONS
  (solve-days 4))

(defn find-star [day star]
  (get-in SOLUTIONS [(dec day) (dec star)]))


