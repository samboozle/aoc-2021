(ns advent-clj.core
  (:require [clojure.java.io :as io]
            [clojure.string  :as string]))

;; Dec 01 puzzles

(def DAY-ONE-INPUT
  (-> "input-01.txt"
      io/resource
      slurp
      (string/split #"\n")
      ((partial map read-string))))

(defn count-increases-over-prior [[acc prev] line]
        (if (or (nil? prev) (>= prev line))
          [acc line]
          [(inc acc) line]))

(defn day-one-star-one []
  (first (reduce count-increases-over-prior [0 nil] DAY-ONE-INPUT)))

(defn day-one-star-two []
  (first (reduce count-increases-over-prior
                 [0 nil]
                 (map (partial reduce +) (partition 3 1 DAY-ONE-INPUT)))))

;; Dec 02 puzzles

(defn day-two-star-one [] "Unimplemented")

(defn day-two-star-two [] "Unimplemented")