(ns advent-clj.december.dec-01)

(def parse-input
  (partial map read-string))

(defn count-increases-over-prior [coll]
  (count (filter (partial apply <) (partition 2 1 coll))))

(defn stars [input]
  (let [input' (parse-input input)]
    (map count-increases-over-prior
         [input' (map (partial reduce +) (partition 3 1 input'))])))

;; another implementation

(defn count-increases-over-prior' [[acc prev] line]
  (if (or (nil? prev) (>= prev line))
    [acc line]
    [(inc acc) line]))