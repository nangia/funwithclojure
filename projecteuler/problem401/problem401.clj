
(defn factors-seq [n]
  ;; this function returns the sequence of factors of n
  (let [root2 (int (Math/ceil (Math/sqrt n))),
        perfectsquare? (== (* root2 root2) n),
        divisors (->> (range 1 root2)
                      (filter #(zero? (rem n %1)))
                      (mapcat (fn[x] (vector x (quot n x)))))
        ]
    (sort (if perfectsquare?
            (lazy-cat divisors [root2])
            divisors))))

(defn ten-power [n]
  (apply * (take n (repeat 10))))

(defn mod9num-padwithzero [x]
  (take 10 (lazy-cat x (repeat 0))))

(defn num->mod9num [n]
  (let [procnum (fn [x]
                  (let [quotient (x 0), remainder (x 1)]
                    [(quot quotient 10) (rem quotient 10)]))
        filtercriterion (fn [x]
                          (let [[quotient remainder] x]
                            (or (not (zero? quotient))
                                (not (zero? remainder)))))] 

    (if (zero? n)
      (lazy-cat [0])
      (->> (iterate procnum [n 0])
           (take-while filtercriterion)
           (drop 1)
           (take 10)
           (map #(second %1))
           (mod9num-padwithzero)))))

(defn mod9num->num [x]
  (reduce + (map * x (take 10
                           (iterate #(* 10 %1) 1)))))





;;;; finish the function below and one for multiplication
(defn mod9sum-helper
  ([x y] (mod9sum-helper x y 0))
  ([x y carry]
     (if (empty? x)
       (lazy-cat [carry])
       (let [thesum (+ carry (first x) (first y))
             newcarry (quot thesum 10)
             newsum (rem thesum 10)]
         (lazy-cat [newsum]
                   (mod9sum-helper (rest x) (rest y) newcarry))))))

(defn mod9sum [x y]
  (take 10 (mod9sum-helper)))

(defn testmod9sum [x y]
  (let [mod9x (num->mod9num x)
        mod9y (num->mod9num y)
        sum (mod9sum mod9x mod9y)]
    (print mod9x)
    (print mod9y)
    (print sum)))


  

           

(defn sigma2 [n]
  ;; calculate sum of squares of divisors
  (->> (factors-seq n) ;; series has the factors of n
       (map #(* %1 %1)) ;; map each factor to its square
       (reduce +) ;; find the sum of the squares
       ))

(defn big-sigma2 [n]
  (->> (range 1 (inc n)) ; generate values from 1 to n
       (map sigma2)
       (reduce +)))

(testmod9sum 234 456)


;(big-sigma2 (ten-power 5))
