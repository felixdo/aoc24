(ns day5
  (:require
   [clojure.string :as s]
   [clojure.java.io :as io]))

(defn center [x]
  (first
   (second (split-at (dec (/ (count x) 2)) x))))

(defn check-rule [update [before after]]
  (not (= [after before]
          (reduce
           (fn [seen n]
             (if (or
                  (= n before)
                  (= n after))
               (conj seen n)
               seen))
           []
           update))))

(defn check-rules [update rules]
  (reduce
   (fn [_ rule]
     (if (not (check-rule update rule))
       (reduced false)
       true))
   true
   rules))

(defn load []
  (with-open [r (-> "day5.txt"
                    io/resource
                    io/reader)]
    (let [[rules updates _]
          (reduce
           (fn [[rules updates rule-phase?] line]
             (if (= "" line)
               [rules updates false]
               (if rule-phase?
                 [(conj rules (mapv parse-long (s/split line #"\|" ))) updates true]
                 [rules (conj updates (mapv parse-long (s/split line #","))) false])))
           [[] [] true]
           (line-seq r))]
      [rules updates]
      )
    ))

(comment "part 1"
  (let [[rules updates] (load)]
    (reduce
     (fn [result u]
       (+ result 
          (if (check-rules u rules)
            (center u)
            0)))
       0
       updates
       )
    )
  )

(defn rules-compare [rules]
  (fn [x y]
    (loop [work [x]]
      (if-let [next (first work)]
        (if (= y next)
          -1
          (let [more (->> rules
                       (filter (fn [[a b]]
                                 (= next a)))
                       (map second))]
            (recur
             (apply conj (rest work) more))))
        1
        ))))

(comment
  "part 2. warning, this brute sort approach takes ~5-10 minutes to complete!"
         (let [[rules updates] (load)
               must-fix (filter #(not (check-rules % rules)) updates)]
           (reduce
            (fn [result u]
              (+ result
                 (center 
                  (let [relevant (filter #(let [update-set (into #{} u)]
                                            (and
                                             (contains? update-set (first %))
                                             (contains? update-set (second %))))
                                         rules)
                        c (rules-compare relevant)]
                    (sort c u)))))
            0
            must-fix)
           ))
