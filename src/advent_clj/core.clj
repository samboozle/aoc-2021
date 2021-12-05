(ns advent-clj.core
  (:require [clojure.java.io :as io]
            [clojure.string  :as string]))

(def get-daily-input
  (comp #(string/split % #"\n")
        slurp
        io/resource
        (partial format "input-%02d.txt")
        read-string))

;; Dec 01 puzzles

(def DAY-ONE-INPUT
  (map read-string (get-daily-input "1")))

;; two implementations

(defn count-increases-over-prior [[acc prev] line]
  (if (or (nil? prev) (>= prev line))
    [acc line]
    [(inc acc) line]))

(defn count-increases-over-prior' [coll]
  (count (filter (partial apply <) (partition 2 1 coll))))

;; (def day-01-star-01
;;   (first (reduce count-increases-over-prior [0 nil] DAY-ONE-INPUT)))

(def day-01-star-01
  (count-increases-over-prior' DAY-ONE-INPUT))

(def day-01-star-02
  (count-increases-over-prior' (map (partial reduce +) (partition 3 1 DAY-ONE-INPUT))))

;; Dec 02 puzzles

(def DAY-TWO-INPUT
  (map (comp (partial apply #(vector (keyword %1) (read-string %2))) #(string/split % #"\s")) (get-daily-input "2")))

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

(def day-02-star-01
  (reduce * (reduce change-point [0 0] DAY-TWO-INPUT)))

(def day-02-star-02
  (reduce * (take 2 (reduce change-point' [0 0 0] DAY-TWO-INPUT))))

;; Dec 03 puzzles

(def DAY-THREE-INPUT
  (map seq (get-daily-input "3")))

(def DAY-THREE-TEST
  (->> (io/resource "input-03-test.txt")
       slurp
       (#(string/split % #"\n"))
       (map seq)))

(def frequencies-map
  (partial apply map (comp frequencies vector)))

(defn gamma-epsilon [coll]
  (reduce
   (fn [acc frq]
     (map conj acc (reverse (sort-by #(frq %) (keys frq)))))
   [[] []] ;; using lists here gives me my binary numbers backward
   (frequencies-map coll)))

(defn oxygen [coll]
  (loop [idx                  0
         [head & _ :as coll'] coll]
    (let [out-of-bounds?    (= (count head) idx)
          {zros \0 ones \1} (if out-of-bounds?
                              {}
                              (group-by #(nth % idx) coll'))
          zros'             (count zros)
          ones'             (count ones)
          zros-win?         (> zros' ones')]
      (cond
        (= 1 (count coll')) head
        zros-win?           (recur (inc idx) zros)
        :else               (recur (inc idx) ones)))))

(defn carbon-dioxide [coll]
  (loop [idx                  0
         [head & _ :as coll'] coll]
    (let [out-of-bounds?    (= (count head) idx)
          {zros \0 ones \1} (if out-of-bounds?
                              {}
                              (group-by #(nth % idx) coll'))
          zros'             (count zros)
          ones'             (count ones)
          ones-win?         (> zros' ones')]
      (cond
        (= 1 (count coll')) head
        ones-win?           (recur (inc idx) ones)
        :else               (recur (inc idx) zros)))))

(defn binary->decimal [binary]
  ;; backward binary numbers are easier to reduce to decimals
  (second (reduce
           (fn [[pow tot] dig]
             (let [dig' (case dig \0 0 \1 1)]
               [(inc pow) (+ tot (* dig' (reduce * (repeat pow 2))))]))
           [0 0]
           (into '() binary))))

(def day-03-star-01
  (->> (gamma-epsilon DAY-THREE-INPUT)
       (map binary->decimal)
       (reduce *)))

(def day-03-star-02
  (reduce * [(binary->decimal (oxygen         DAY-THREE-INPUT))
             (binary->decimal (carbon-dioxide DAY-THREE-INPUT))]))

;; overall result

(defn SOLUTION-MAP []
  [[day-01-star-01 day-01-star-02]
   [day-02-star-01 day-02-star-02]
   [day-03-star-01 day-03-star-02]])

(defn find-star [day star]
  (get-in (SOLUTION-MAP) [(dec day) (dec star)]))