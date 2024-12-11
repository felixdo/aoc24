(ns day7
  (:require
   [clojure.string :as s]
   [clojure.java.io :as io]))

(defn || [l1 l2]
  (parse-long (str l1 l2)))

(defn trysolve [[expected-result & operands]]
  (loop [queue [operands]]
    (if-let [[current-result & more-operands] (first queue)]
      (if (and (= current-result expected-result)
               (or (not (seq more-operands))
                   (= #{1} (into #{} more-operands))))
        expected-result
        (recur
         (if (and
              (< current-result expected-result)
              (seq more-operands))
           (conj (rest queue)
                 (conj (rest more-operands)
                       (+ current-result
                          (first more-operands)))
                 (conj (rest more-operands)
                       (* current-result
                          (first more-operands)))
                 (conj (rest more-operands)
                       (|| current-result
                           (first more-operands)))
                 )
           (rest queue))))
      0
      )))


;; part1 is the same, but without the last conj
(comment "Part 2"
  (with-open [r (-> "day7.txt"
                    io/resource
                    io/reader)]
    (->> r
         line-seq
         (map
          #(map parse-long (s/split % #":?\s")))
         (map trysolve)
         (reduce +)
         )
    )
  )


      

