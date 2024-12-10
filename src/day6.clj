(ns day6
  (:require
   [clojure.string :as s]
   [clojure.java.io :as io]))

(defn guard-path [extra-obstacle rectangle]
  (let [width  (count (first rectangle))
        height (count rectangle)
        data   (s/join rectangle)
        start  (.indexOf data "^")
        up     #(if (> % width)
                  (- % width)
                  -1)
        dn     #(if (< % (* (dec height) width))
                  (+ % width)
                  -1
                  )
        l      #(if (= 0 (mod % width)) ;; make sure to not jump back to
                  -1                    ;; the end of the previous line
                  (dec %))              ;; if at the beginning of a line
        r      #(if (= (dec width) (mod % width)) ;; make sure to not jump to the 
                  -1                              ;; beginning of the next line
                  (inc %))                        ;; if at the end of a line
        turn   #(get {up r r dn dn l l up} %)
        obstacle? (fn [pos]
                    (or (= extra-obstacle pos)
                        (= \# (nth data pos))))]
    (loop [[p d] [start up]
           visited #{[p d]}]
      (let [newpos (d p)]
        (if (= -1 newpos)
          [visited false]
          (if (obstacle? newpos)
            (recur [p (turn d)] visited)
            (if (contains? visited [newpos d])
              [visited true]
              (recur [newpos d] (conj visited [newpos d])))))))))


(comment "Part 1"
  (with-open [r (-> "day6.txt"
                    io/resource
                    io/reader)]
    (->> r line-seq (guard-path nil) first (map first) (into #{}) count)))

(comment "Part 2"
  (with-open [r (-> "day6.txt"
                    io/resource
                    io/reader)]
    (let [rectangle (line-seq r)]
          (->> rectangle
               (guard-path nil)
               (first)
               (map first)
               (into #{})
               (filter (fn [extra-obstacle]
                         (->> rectangle
                              (guard-path extra-obstacle)
                              second)))
               count)))
)
          

      

