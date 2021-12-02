(ns advent-clj.core
  (:require [clojure.java.io :as io]
            [clojure.string  :as string]))

;; Dec 01 puzzles

(def DAY-ONE-INPUT
  ;; input formatted as a vector of integers 
  (-> "input-01.txt"
      io/resource
      slurp
      (string/split #"\n")
      ((partial map read-string))))

(defn count-increases-over-prior [[acc prev] line]
        (if (or (nil? prev) (>= prev line))
          [acc line]
          [(inc acc) line]))

(def day-one-star-one
  (first (reduce count-increases-over-prior [0 nil] DAY-ONE-INPUT)))

(def day-one-star-two
  (first (reduce count-increases-over-prior
                 [0 nil]
                 (map (partial reduce +) (partition 3 1 DAY-ONE-INPUT)))))

;; Dec 02 puzzles

(def DAY-TWO-INPUT
  ;; input formatted as a vector of [keyword integer] tuples (vectors)
  (-> "input-02.txt"
      io/resource
      slurp
      (string/split #"\n")
      ((partial map
                (comp
                 (partial apply #(vector (keyword %1) (read-string %2)))
                 #(string/split % #"\s"))))))

(defn change-point [[hrz dep] [dir amp]]
  (case dir
    :forward [(+ hrz amp) dep]
    :down    [hrz (+ dep amp)]
    :up      [hrz (max (- dep amp) 0)]))

(defn change-point' [[hrz dep aim] [dir amp]]
  (case dir
    :forward [(+ hrz amp) (+ dep (* aim amp)) aim]
    :down    [hrz dep (+ aim amp)]
    :up      [hrz dep (max (- aim amp) 0)]))

(def day-two-star-one
  (reduce * (reduce change-point [0 0] DAY-TWO-INPUT)))

(def day-two-star-two
  (reduce * (take 2 (reduce change-point' [0 0 0] DAY-TWO-INPUT))))

;; overall result

(def SOLUTION-MAP
  {:one {:one day-one-star-one :two day-one-star-two}
   :two {:one day-two-star-one :two day-two-star-two}})

(defn find-star [day star]
  (get-in SOLUTION-MAP [day star]))