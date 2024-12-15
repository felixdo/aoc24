(ns day10
  (:require
   [clojure.string :as s]
   [clojure.java.io :as io]))

(defn trails [rectangle]
  (let [width  (count (first rectangle))
        height (count rectangle)
        data   (mapv #(parse-long (str %)) (-> rectangle s/join))
        up     #(- % height)
        dn     #(+ % height)
        l      #(if (= 0 (mod % width))           ;; make sure to not jump back to
                  -1                              ;; the end of the previous line
                  (dec %))                        ;; if at the beginning of a line
        r      #(if (= (dec width) (mod % width)) ;; make sure to not jump to the 
                  -1                              ;; beginning of the next line
                  (inc %))                        ;; if at the end of a line
        trailheads (for [x (range (count data))
                         :when (= 0 (nth data x))]
                     x)]
    (letfn [(reachable [index]
              (let [d (nth data index)]
                (if (= 9 d)
                  [index]
                  (reduce
                   (fn [result neighbor]
                     (if (= (inc d) (nth data neighbor))
                       (apply conj result (reachable neighbor))
                       result))
                   []
                   (filter #(<= 0 % (dec (count data)))
                           ((juxt up dn l r) index))))))]
      {:part1 
       (reduce
        (fn [result trailhead]
          (+ result (count (into #{} (reachable trailhead)))))
        0
        trailheads      
        )
       :part2
       (reduce
        (fn [result trailhead]
          (+ result (count (reachable trailhead))))
        0
        trailheads      
        )}
      )))

(comment "part "
         (with-open [r (-> "day10.txt"
                           io/resource
                           io/reader)]
           (trails (line-seq r)))

         )
