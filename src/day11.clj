(ns day11
  (:require
   [clojure.string :as s]
   [clojure.java.io :as io]))

(defn split [stone]
  (let [s (str stone)
        ssl (count s)]
    (if (= 0 (mod (count (str stone)) 2))
      [(parse-long (subs s 0 (/ ssl 2)))
       (parse-long (subs s (/ ssl 2) ssl))]
      nil
      )))

(defn blink [stones]
  (reduce-kv
   (fn [result n freq]
     (cond 
       (= 0 n) (-> result
                   (update 1 #(+ (or % 0) freq))
                   (update 0 #(- % freq)))
       
       (split n) (reduce (fn [result split-val]
                           (update result split-val #(+ (or % 0) freq)))
                         (update result n #(- % freq))
                         (split n))
       
       :else (-> result
                 (update (* 2024 n) #(+ (or % 0) freq))
                 (update n #(- % freq)))))
   stones
   stones))

(defn blink-times [n stones]
  (reduce + (->> (iterate blink (frequencies stones))
                 (take (inc n))
                 last
                 vals
                 )))


(comment "part 1 & 2"
  (let [stones [475449 2599064 213 0 2 65 5755 51149]]
    {:part1 (blink-times 25 stones)
     :part2 (blink-times 75 stones)})
)
