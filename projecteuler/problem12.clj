


(defn computetrianglenum [x] (/ (* x (inc x)) 2) )

;sequence of integer -> trianglenumber -> [triangle number, count of list of factors] 
; -> drop while the count < 500 -> take 1

(defn countoffactors [n]
  (inc (count (filter #(zero? (rem n %1)) (range 1 (inc (/ n 2)))))))

(defn num->3tuple [x]
  (let [trianglenum (computetrianglenum x)]
    [x trianglenum (countoffactors trianglenum)]))

(def answer 
  (->> (iterate inc 1)
     (map num->3tuple)
     ;(map #(do (pprint %1) %1))
     (drop-while #(<= (%1 2) 50) )
     (take 1)
     )
  )
