(ns day13
  (:require
   [clojure.string :as s]
   [clojure.java.io :as io]))

(defn solve [[[ax ay] [bx by] [tx ty]]]
  (let [max-j 100 max-k 100]
    (for [j (range max-j)
          k (range max-k)
          :when (and
                 (= tx (+ (* j ax) (* k bx)))
                 (= ty (+ (* j ay) (* k by))))]
      (cost j k))))

(defn read-input []
  (with-open [r (-> "day13.txt" io/resource io/reader)]
    (reduce
     (fn [result [as bs prize]]
       (conj result [(mapv parse-long (re-seq #"\d+" as))
                     (mapv parse-long (re-seq #"\d+" bs))
                     (mapv parse-long (re-seq #"\d+" prize))]))
     []
     (->> r line-seq (filter #(seq %)) (partition 3)))))


(defn cost [j k]
  (+ (* 3 j) k))

(defn part-1 []
  (reduce
   +
   (mapcat
    solve
    (read-input))))

(comment (part-1))
