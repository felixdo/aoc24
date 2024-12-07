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
        x-mas (fn [start]
                (if (and (= \A (char-at start))
                         (or (and
                              (= \M (char-at (ul start)))    ;; M S
                              (= \M (char-at (dl start)))    ;;  A
                              (= \S (char-at (ur start)))    ;; M S
                              (= \S (char-at (dr start))))
                             (and
                              (= \S (char-at (ul start)))    ;; S M
                              (= \S (char-at (dl start)))    ;;  A
                              (= \M (char-at (ur start)))    ;; S M
                              (= \M (char-at (dr start))))
                             (and
                              (= \S (char-at (ul start)))    ;; S S
                              (= \M (char-at (dl start)))    ;;  A
                              (= \S (char-at (ur start)))    ;; M M
                              (= \M (char-at (dr start))))
                             (and
                              (= \M (char-at (ul start)))    ;; M M 
                              (= \S (char-at (dl start)))    ;;  A
                              (= \M (char-at (ur start)))    ;; S S
                              (= \S (char-at (dr start))))))
                  1
                  0))
        rf-part1 (fn [result start]
                   (+ result
                      (xmas up start) (xmas dn start) (xmas r  start) (xmas l  start)
                      (xmas ul start) (xmas ur start) (xmas dl start) (xmas dr start)))
        rf-part2 (fn [result start]
                   (+ result
                      (x-mas start)))]
    {:part1 (reduce rf-part1 0 (range (count data)))
     :part2 (reduce rf-part2 0 (range (count data)))
     }))

(comment "part 1 and 2"
  (with-open [r (-> "day4.txt"
                    io/resource
                    io/reader)]
    (count-xmas (line-seq r))))
