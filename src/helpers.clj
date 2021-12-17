(ns helpers
  (:require [clojure.string :refer [split]]))

(defn split-on [sep]
  #(split % sep))

(def lines
  (split-on #"\n"))

(def words
  (split-on #"\s+"))

(def comma
  (split-on #","))

(def product
  (partial reduce *))

(def sum
  (partial reduce +))

(defn binary->decimal [binary]
  (second (reduce
           (fn [[pow tot] dig]
             (let [dig' (case dig \0 0 \1 1)]
               [(inc pow) (+ tot (* dig' (reduce * (repeat pow 2))))]))
           [0 0]
           (into '() binary))))

(defn abs [value]
  (if (pos? value)
    value
    (- 0 value)))

(defn _triangle [number]
  (reduce + (range (inc number))))

(def triangle (memoize _triangle))

(def string->set
  (comp set seq))

(def digits->int
  (partial reduce #(+ %2 (* 10 %1)) 0))

(defn first-where [pred]
  (comp first (partial filter pred)))