(ns day3
  (:require
   [clojure.string :as s]
   [clojure.java.io :as io]))

(def mul-re #"mul\((\d{1,3}),(\d{1,3})\)")

(defn part1 []
  (with-open [rdr (->> "day3.txt"
                       io/resource
                       io/reader)]
    (->> rdr
        line-seq
        (mapcat #(re-seq mul-re %))
        (map rest)
        (map (fn [[f1 f2]]
               (* (parse-long f1)
                  (parse-long f2))))
        (reduce +)
        )))

(def do-re #"do\(\)")
(def dont-re #"don't\(\)")

(def p2-re #"do(?:n't)?\(\)|mul\((\d{1,3}),(\d{1,3})\)")

(defn part2 []
  (with-open [rdr (->> "day3.txt"
                       io/resource
                       io/reader)]
    (->> rdr
        line-seq
        (mapcat #(re-seq p2-re %))
        (reduce
         (fn [[do? result] [match b c]]
           (case match
             "do()"    [true result]
             "don't()" [false result]
             [do? (+ result
                     (if do?
                       (* (parse-long b)
                          (parse-long c))
                       0))]))
         [true 0])
        last
        )))

(comment
  (part1)
  (part2)
)
