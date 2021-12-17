(ns advent-clj.december.dec-08
  (:require [helpers :refer [digits->int first-where split-on string->set sum words]]
            [clojure.set :refer [difference]]))

;;  0:      1:      2:      3:      4:
;;  aaaa    ....    aaaa    aaaa    ....
;; b    c  .    c  .    c  .    c  b    c
;; b    c  .    c  .    c  .    c  b    c
;;  ....    ....    dddd    dddd    dddd
;; e    f  .    f  e    .  .    f  .    f
;; e    f  .    f  e    .  .    f  .    f
;;  gggg    ....    gggg    gggg    ....
;; 
;;   5:      6:      7:      8:      9:
;;  aaaa    aaaa    aaaa    aaaa    aaaa
;; b    .  b    .  .    c  b    c  b    c
;; b    .  b    .  .    c  b    c  b    c
;;  dddd    dddd    ....    dddd    dddd
;; .    f  e    f  .    f  e    f  .    f
;; .    f  e    f  .    f  e    f  .    f
;;  gggg    gggg    ....    gggg    gggg

;; (def LEGEND
;;   [#{\a \b \c \e \f \g}
;;    #{\c \f}
;;    #{\a \c \d \e \g}
;;    #{\a \c \d \f \g}
;;    #{\b \c \d \f}
;;    #{\a \b \d \f \g}
;;    #{\a \b \d \e \f \g}
;;    #{\a \c \f}
;;    #{\a \b \c \d \e \f \g}
;;    #{\a \b \c \d \f \g}])

(def parse-input
  (partial map (comp (partial map words) (split-on #"\s\|\s"))))

(defn unique-digits [input]
  (count (apply concat (map (comp (partial filter #{2 3 4 7}) (partial map count) second) input))))

;; I was too tired so I brute forced this with definite set differences rather than solving the clock face :)
(defn solve-crossed-wires [[digits display]]
  (let [[[one sev fir] fives sixes [ate]] (partition-all 3 3 (sort-by count (map string->set digits)))
        clock-face (map string->set display)
        two ((first-where #(= 2 (count (difference fir %)))) fives)
        tri ((first-where #(zero? (count (difference one %)))) fives)
        fiv ((first-where #(and (not= two %) (not= tri %))) fives)
        six ((first-where #(= 1 (count (difference one %)))) sixes)
        nin ((first-where #(zero? (count (difference fir %)))) sixes)
        zro ((first-where #(and (not= six %) (not= nin %))) sixes)
        digit-map {zro 0 one 1 two 2 tri 3 fir 4 fiv 5 six 6 sev 7 ate 8 nin 9}]
    (digits->int (map #(get digit-map % 0) clock-face))))

(defn stars [input]
  [(unique-digits (parse-input input))
   (sum (map solve-crossed-wires (parse-input input)))])
