(ns advent-clj.december.dec-04
  (:require [clojure.string :refer [trim]]
            [helpers        :refer [comma words]]))

(defn parse-input [input]
  (let [[moves & boards] input]
    {:moves  (map read-string (comma moves))
     :boards (reduce (fn [[board & tail :as table] row]
                       (if (= "" row)
                         (conj table [])
                         (conj tail (conj board (map read-string (words (trim row)))))))
                     '()
                     boards)}))

(defn columns [matrix]
  (for [n (range (count matrix))]
    (map #(nth % n) matrix)))

(defn tiles [matrix]
  (apply concat matrix))

(defn avenues [matrix]
  (concat matrix (columns matrix)))

(defn winning? [numbers matrix]
  (some (partial every? numbers) (avenues matrix)))

(defn play-bingo [{moves :moves boards :boards}]
  (reduce (fn [numbers number]
            (let [numbers'     (conj numbers number)
                  [winner & _] (filter (partial winning? numbers') boards)]
              (if winner
                (reduced
                 (* number (reduce + (filter (complement numbers') (tiles winner)))))
                numbers')))
          #{}
          moves))

(defn lose-bingo [{moves :moves boards :boards}]
  (reduce (fn [numbers number]
            (let [numbers'         (disj numbers number)
                  [last-place & _] (filter (partial (complement winning?) numbers') boards)]
              (if last-place
                (reduced
                 (* number (reduce + (filter (complement numbers) (tiles last-place)))))
                numbers')))
          (into #{} moves)
          (reverse moves)))

(defn stars [input]
  (let [input' (parse-input input)]
    [(play-bingo input')
     (lose-bingo input')]))
