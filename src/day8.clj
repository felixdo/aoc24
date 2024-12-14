(ns day8
  (:require
   [clojure.java.io :as io]
   [clojure.set]
   ))

(defn make-antenna-map [rectangle]
    (->> rectangle
         (reduce (fn [[row-index antennas] row]
                   [(inc row-index)
                    (first (reduce
                            (fn [[antennas col-index] c]
                              [(if (= \. c)
                                 antennas
                                 (update antennas c #(conj % [col-index row-index])))
                               (inc col-index)])
                            [antennas 0]
                            (seq row)))])
                 [0 {}])
         second
    ))

(comment
  (with-open [r (-> "day8.txt"
                    io/resource
                    io/reader)]
    (let [rectangle (line-seq r)
          width  (count (first rectangle))
          height (count rectangle)
          vec    (fn [[ax ay] [bx by]] [(- bx ax)
                                        (- by ay)])
          vec-min (fn [[ax ay] [bx by]]
                    (loop [[vx vy] (vec [ax ay] [bx by])]
                      (if (and (= 0 (mod vx 2))
                               (= 0 (mod vy 2)))
                        (recur [(/ vx 2) (/ vy 2)])
                        [vx vy])))
          mirror (fn [[ax ay] [bx by]]
                   (let [[vx vy] (vec [ax ay] [bx by])]
                     [(+ bx vx) (+ by vy)]))
          inside? (fn [[x y]] (and (< -1 x width)
                                 (< -1 y height)))
          antenna-map (make-antenna-map rectangle)]
      {:part1 
       (->> (vals antenna-map)
            (reduce
             (fn [result antennas]
               (apply conj result
                      (for [[x1 y1] antennas
                            [x2 y2] antennas
                            :when (not= [x1 y1] [x2 y2])]
                        (mirror [x1 y1] [x2 y2]))))
             [])
            (filter inside?)
            (into #{})
            count)
      :part2
       (->> (vals antenna-map)
            (reduce
             (fn [result antennas]
               (apply conj result
                      (for [[x1 y1] antennas
                            [x2 y2] antennas
                            :when (not= [x1 y1] [x2 y2])
                            :let [[vx vy] (vec-min [x1 y1] [x2 y2])]]
                        (reduce
                         (fn [result t]
                           (let [p [(+ x1 (* t vx))
                                    (+ y1 (* t vy))]]
                             (if (inside? p)
                               (conj result p)
                               (reduced result))))
                         #{}
                         (range)))))
             []
             )
            (apply clojure.set/union)
            count)}))

  )
