(ns day1 
  (:require
   [clojure.string :as s]
   [clojure.java.io :as io]))

(defn process-lines [f]
  (with-open [rdr (->> "day1/input-1.txt"
                       io/resource
                       io/reader)]
    (let [[left right] (reduce (fn [[vl vr] [l r]]
                                 [(conj vl l) (conj vr r)])
                               [[] []]
                               (into []
                                     (comp 
                                      (map #(s/split % #"\s+"))
                                      (map (fn [[l r]] [(parse-long l)
                                                        (parse-long r)
                                                        ])))
                                     (line-seq rdr)))]
      (f left right))))

(defn part-1 []
  (process-lines
   (fn [left right]
      (reduce + (map #(abs (- %1 %2)) (sort left) (sort right))))))

(defn part-2 []
  (process-lines
   (fn [left right]
     (let [freqs (frequencies right)]
      (reduce
       #(+ %1 (* %2 (get freqs %2 0)))
       0
       left
       )))))

(comment
  (part-1)
  (part-2))
