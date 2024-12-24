(ns day14
  (:require
   [clojure.string :as s]
   [clojure.java.io :as io]))

(def width #_11 101)
(def height #_7 103)
(def center-x (/ (dec width) 2))
(def center-y (/ (dec height) 2))

(defn read-input []
  (with-open [r (-> "day14.txt" io/resource io/reader)]
    (into []
          (map (fn [line]
                 (->> line
                      (re-seq #"-?\d+")
                      (map parse-long)
                      (partition 2))))
          (line-seq r))))

(defn step [[[x y] [vx vy]]]
  [[(mod (+ x vx) width)
    (mod (+ y vy) height)]
   [vx vy]])

(defn quadrant [[[x y] _]]
  (cond
    (or (= center-x x) 
        (= center-y y))
    nil
    
    (and (> center-x x)
         (> center-y y))
    1
    
    (and (< center-x x)
         (> center-y y))
    2
    
    (and (> center-x x)
         (< center-y y))
    3
    
    (and (< center-x x)
         (< center-y y))
    4))

(comment "part 1"
(->> (read-input)
     (map #(last (take 101 (iterate step %))))
     (map quadrant)
     (filter some?)
     frequencies
     (apply *)
     )
)


(defn draw-line [l positions]
  (run! #(if (some (fn [[[x y] _]]
                     (and (= l y)
                          (= % x)))
                   positions)
           (print (or (quadrant [[% l]]) "x"))
           (print (cond (and (= center-x %)
                             (= center-y l))
                        "+"
                        (= center-x %)
                        "|"
                        (= center-y l)
                        "-"
                        :default
                        " "
                        )))
        (range #_center-x width))
  (println))


(loop [seconds 0
       positions (read-input)]
  
  (let [quads (->> positions
                   (map quadrant)
                   (filter some?)
                   frequencies
                   )
        q1 (get quads 1 0)
        q2 (get quads 2 0)
        q3 (get quads 3 0)
        q4 (get quads 4 0)]
    (if (= seconds 7339) ;; 9604
      false
      #_seconds
      (do 
        (when
            
            (and
             (= 7338 seconds)
             #_true
             #_(> 20 (abs (- q1 q2)) 10)
                 #_(> 20 (abs (- q3 q4)) 10)
                  #_(> q3 q1)
                  #_(> q4 q2)
                  #_(= q1 q2)
                  #_(= q3 q4)
                  #_(< 50 (count (for [[[x1 y1] _] positions
                                       [[x2 y2] _] positions
                                       :let [q  (quadrant [[x1 y1]])
                                             q* (quadrant [[x2 y2]])
                                             ]
                                       :when (and
                                              (= y1 y2)
                                              (= x2 (- (dec width) x1)))]
                                 
                                 
                                 
                                   1)
                               
                                    
                                    
                                 )))
            (println seconds " seconds")
            
            (run! #(draw-line % positions) #_(range center-y) (range height)))
        
        (recur (inc seconds) (map step positions))))))

