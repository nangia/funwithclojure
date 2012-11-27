(defn the-seq-generator [x] ;; we did not take int account the last 1
  (take-while #(not= %1 1)
              (iterate (fn [n] (if (even? n) (/ n 2) (inc (* n 3)))) x)))

(defn the-seq-len [x]
  (inc (count (the-seq-generator x)))) ;; 1 added because the last 1 was not counted

(defn find-max-tuple
  ([sequence maxtuplesofar] ;; sequence is of tuples [number seqlen]
     (let [firsttuple (first sequence), theseqlen (second firsttuple),
           maxseqlensofar (second maxtuplesofar)]
       (if (nil? firsttuple)
         maxtuplesofar
         (if (> maxseqlensofar theseqlen)
           (recur (rest sequence) maxtuplesofar)
           (recur (rest sequence) firsttuple)))))
  ([sequence]
     (find-max-tuple sequence [-1 0])))
 

(time (->> (iterate inc 1 )
           (take-while #(< %1 1000000))
           (map (fn [x] [x (the-seq-len x)]))
           (find-max-tuple)
           (first)
           (pprint)
           ))

;; Alternateivly you could find this using the code below
;(def max-finder (partial max-key the-seq-len))
;(time (apply max-finder (numbers-below 1000000)))