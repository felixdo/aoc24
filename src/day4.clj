(ns day4
  (:require
   [clojure.string :as s]
   [clojure.java.io :as io]))

(defn count-xmas [rectangle]
  (let [width  (count (first rectangle))
        height (count rectangle)
        data   (s/join rectangle)
        up     #(- % height)
        dn     #(+ % height)
        l      #(if (= 0 (mod % width))           ;; make sure to not jump back to
                  -1                              ;; the end of the previous line
                  (dec %))                        ;; if at the beginning of a line

        r      #(if (= (dec width) (mod % width)) ;; make sure to not jump to the 
                  -1                              ;; beginning of the next line
                  (inc %))                        ;; if at the end of a line
        ul     (comp l up)
        ur     (comp r up)
        dl     (comp l dn)
        dr     (comp r dn)
        char-at #(if (<= 0 % (- (* width height) 1))
                   (nth data %)
                   \.)
        xmas (fn [dir start]
               (if (and
                    (= \X (char-at start))
                    (= \M (char-at (dir start)))
                    (= \A (char-at (dir (dir start))))
                    (= \S (char-at (dir (dir (dir start))))))
                 1
                 0))
        rf (fn [result start]
             (+ result
                (xmas up start) (xmas dn start) (xmas r  start) (xmas l  start)
                (xmas ul start) (xmas ur start) (xmas dl start) (xmas dr start)))
        ]
    (->> data
         (map-indexed (fn [i c] [i c]))
         (filter #(= \X (second %)))
         (map first)
         (reduce rf 0))))

(comment "part 1"
  (with-open [r (-> "day4.txt"
                    io/resource
                    io/reader)]
    (count-xmas (line-seq r))))
