(ns advent-clj.december.dec-01)

(def parse-input
  (partial map read-string))

(def count-increases-over-prior
  (comp count (partial filter (partial apply <)) (partial partition 2 1)))

(defn stars [input]
  (let [input' (parse-input input)]
    (map count-increases-over-prior
         [input' (map (partial reduce +) (partition 3 1 input'))])))

;; another implementation

(defn count-increases-over-prior' [[acc prev] line]
  (if (or (nil? prev) (>= prev line))
    [acc line]
    [(inc acc) line]))
