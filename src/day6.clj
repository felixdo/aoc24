(ns day4
  (:require
   [clojure.string :as s]
   [clojure.java.io :as io]))

(defn guard-path [rectangle]
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
        l      #(if (= 0 (mod % width))           ;; make sure to not jump back to
                  -1                              ;; the end of the previous line
                  (dec %))                        ;; if at the beginning of a line
        r      #(if (= (dec width) (mod % width)) ;; make sure to not jump to the 
                  -1                              ;; beginning of the next line
                  (inc %))                        ;; if at the end of a line
        turn   {up r r dn dn l l up}
        obstacle? (fn [pos] (= \# (nth data pos)))]
    (loop [path (list start)
           dir up]
      (let [p (first path)            
            newpos (dir p)]
        (if (= -1 newpos)
          path
          (if (obstacle? newpos)
            (recur path (get turn dir))
            (recur (conj path newpos) dir)))))))

(comment "Part 1"
  (with-open [r (-> "day6.txt"
                    io/resource
                    io/reader)]
    (count (into #{} (guard-path (line-seq r))))))
)
    
