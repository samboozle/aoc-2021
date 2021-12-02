(ns advent-clj.core
  (:require [clojure.java.io :as io]
            [clojure.string  :as string]))

(defn day-one-star-one []
  (let [input (-> "input-01.txt"
                  io/resource
                  slurp
                  (string/split #"\n"))
        func  (fn [[acc prev] line]
                (let [line' (read-string line)]
                  (if (or (nil? prev) (>= prev line'))
                    [acc line']
                    [(inc acc) line'])))]
    (first (reduce func [0 nil] input))))

(defn day-one-star-two []
  (let [input (-> "input-01.txt"
                  io/resource
                  slurp
                  (string/split #"\n"))
        func  (fn [[acc prev] group]
                (let [group' (reduce + (map read-string group))]
                  (if (or (nil? prev) (>= prev group'))
                    [acc group']
                    [(inc acc) group'])))]
    (first (reduce func [0 nil] (partition 3 1 input)))))
