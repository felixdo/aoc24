(ns day12
  (:require
   [clojure.string :as s]
   [clojure.java.io :as io]))

(comment "part 1 and 2"
         (let [rectangle
               (with-open [r (-> "day12.txt"
                                 io/resource
                                 io/reader)]
                 (doall (line-seq r)))
               width  (count (first rectangle))
               height (count rectangle)
               data   (s/join rectangle)
               up     #(- % height)
               dn     #(+ % height)
               l      #(if (= 0 (mod % width)) ;; make sure to not jump back to
                           -1                  ;; the end of the previous line
                         (dec %))              ;; if at the beginning of a line
               r      #(if (= (dec width) (mod % width)) ;; make sure to not jump to the 
                           -1     ;; beginning of the next line
                         (inc %)) ;; if at the end of a line
               char-at #(if (<= 0 % (- (* width height) 1))
                            (nth data %)
                          \.)
               area (fn [s] (count s))
               fences (fn [s]
                        (reduce
                         #(+ %1
                               (if (some #{(up %2)} s) 0 1)
                               (if (some #{(dn %2)} s) 0 1)
                               (if (some #{(l  %2)} s) 0 1)
                               (if (some #{(r  %2)} s) 0 1))
                         0
                         s
                         ))
               
               count-distinct-sides-lr (fn [s]
                                         (first (reduce
                                                 (fn [[result prev] n]
                                                   (if (= (+ prev width) n)
                                                     [result n]
                                                     [(inc result) n]))
                                                 [0 -2]
                                                 (sort s))))
               
               count-distinct-sides-tb (fn [s]
                                         (first (reduce
                                                 (fn [[result prev] n]
                                                   (if (= (+ prev 1) n)
                                                     [result n]
                                                     [(inc result) n]))
                                                 [0 -2]
                                                 (sort s))))
               
               sides (fn [s]
                       (let [tblr (reduce
                                   (fn [result i]
                                     (cond-> result
                                       (not (contains? s (up i))) (update :tops   #(conj % i))
                                       (not (contains? s (l  i))) (update :lefts  #(conj % i))
                                       (not (contains? s (dn i))) (update :bots   #(conj % i))
                                       (not (contains? s (r  i))) (update :rights #(conj % i))))

                                   {:tops #{} :bots #{} :lefts #{} :rights #{}}
                                   s
                                   )
                             
                             tblr (-> tblr
                                      (update :tops (fn [top] (group-by #(quot % height) top )))
                                      (update :bots (fn [bot] (group-by #(quot % height) bot )))
                                      (update :lefts (fn [lefts] (group-by #(mod % width) lefts )))
                                      (update :rights (fn [rights] (group-by #(mod % width) rights ))))
                             
                             tblr  (-> tblr
                                       
                                       (update :tops (fn [tops]
                                                       (reduce
                                                        (fn [result t]
                                                          (+ result (count-distinct-sides-tb t)))
                                                        0
                                                        (vals tops))
                                                       ))
                                       
                                       (update :bots (fn [bots]            
                                                       (reduce
                                                        (fn [result t]
                                                          (+ result (count-distinct-sides-tb t)))
                                                        0
                                                        (vals bots))
                                                       ))
                                       
                                       (update :lefts (fn [lefts]
                                                        (reduce
                                                         (fn [result t]
                                                           (+ result (count-distinct-sides-lr t)))
                                                         0
                                                         (vals lefts))
                                                        ))
                                       
                                       (update :rights (fn [rights]
                                                         (reduce
                                                          (fn [result t]
                                                            (+ result (count-distinct-sides-lr t)))
                                                          0
                                                          (vals rights)))))]
                         
                         
                         
                         (reduce + 0 (vals tblr))))
               
               c-sets (reduce
                       (fn [result i]
                         (let [c (char-at i)
                               cset (get result c #{})
                               li (l i)
                               ui (up i)
                               cl (char-at li)
                               cu (char-at ui)
                               absorb-l (or (some #(and (contains? % li) %) cset) #{})
                               absorb-u (or (some #(and (contains? % ui) %) cset) #{})
                               ]
                           (assoc result c
                                  (-> cset
                                      (disj absorb-l absorb-u)
                                      (conj (clojure.set/union #{i} absorb-l absorb-u))))))
                       {}
                       (range (* width height)))
               ]
           
           {:part1 
            (reduce
             (fn [result region]
               (+ result
                  (* (area region) (fences region))
                  )
               )
             0
             (mapcat identity (vals c-sets)))
            
            :part2
            (reduce
             (fn [result region]
               (+ result
                  (* (area region) (sides region))
                  ))
             0
             (mapcat identity (vals c-sets)))
            }
           )
)
