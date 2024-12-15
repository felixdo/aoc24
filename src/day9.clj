(ns day9
  (:require
   [clojure.java.io :as io]
   [clojure.set]
   ))

(defn optimize-backwards [result disk]
  (if (seq disk)
    (if (= -1 (first disk))
      ;; empty sector
      (let [[sector rest] (split-with #(= -1 %) disk)
            [fill read] (reduce
                         (fn [[fill read] c]
                           (if (= (count sector) (count fill))
                             (reduced [fill read])
                             [(if (= -1 c)
                                  fill
                                  (conj fill c))
                                (inc read)]))
                           [[] 0]
                           (reverse rest))]
        (recur
         (apply conj result fill)
         (if (< read (count sector))
           '()
           (drop-last read rest))
         ))
      ;; file sector
      (let [[sector rest]
            (split-with #(not= -1 %) disk)]
        (recur (apply conj result sector) rest)))
    result))

(defn last-file [disk]
  (let [[front trimmed-space] (trim-empty-space disk)]
    (when-let [index (-> front reverse first)]
      (let [[file tail] (split-with #(= index %) (-> front reverse))]
        [(reverse tail) file trimmed-space]))))


(defn read-input [line]
  (-> (reduce (fn [[result file? index] c]
                [(apply conj result (repeat (parse-long (str c)) (if file? index -1)))
                 (not file?)
                 (if file? (inc index) index)])
              [[] true 0]
              (seq line))
      first))

(defn checksum [disk]
  (reduce-kv
   (fn [result index value]
     (+ result (* index value)))
   0
   disk))

(comment "day 1"
  (with-open [r (-> "day9.txt"
                    io/resource
                    io/reader)
              ]
    (checksum (optimize-backwards [] (read-input (.readLine r))))))

(defn trim-empty-space [disk]
  (let [[front tail]
    (->> disk
         reverse
         (split-with #(= -1 %)))]
    [(reverse tail) front]))
