


(defn computetrianglenum [x] (/ (* x (inc x)) 2) )

;sequence of integer -> trianglenumber -> [triangle number, count of list of factors] 
; -> drop while the count < 500 -> take 1

(defn countoffactors [n]
  (inc (count (filter #(zero? (rem n %1)) (range 1 (/ n 2))))))

(defn countoffactors2 [n]
  (let [root2 (Math/ceil (Math/sqrt n)),
        ;;_ (pprint root2),
        perfectsquare? (== (* root2 root2) n),
        ;;_ (pprint perfectsquare?)
        divisors (filter #(zero? (rem n %1)) (range 1 root2))
        ;;_ (pprint divisors)
        thecount (* 2 (count divisors))]
    (if perfectsquare?
      (inc thecount)
      thecount)))
        
    
(defn num->3tuple [x]
  (let [trianglenum (computetrianglenum x)]
    [x trianglenum (countoffactors2 trianglenum)]))

(def answer
  (->> (iterate inc 1)  ;;; generate a sequence of integers starting from 1
     (map num->3tuple)  ;; map each integer n to a tuple [n trianglenumber countOfFactorsOfTraiangleNum]
     (drop-while #(<= (%1 2) 500) ) ;; ignore numbers where countofFactors <= 500
     (take 1) ;; take the first tuple.
     ))

(time (pprint (second (first answer))))


