

(defn divisionseq-helper [numerator denominator]
  (let [quotient (quot numerator denominator)
        remainder (rem numerator denominator)]
    (lazy-cat [ [quotient remainder] ]
              (if (zero? remainder)
                []
                (lazy-seq (divisionseq-helper (* remainder 10) denominator))))))

   
(defn repeatsat [x the-seq]
  (loop [index 0, myseq the-seq]
    (let [firstelem (first myseq), quotient (first firstelem), remainder (second firstelem)]
      (if (nil? firstelem)
        nil; not found
        (if (== remainder x)
          index
          (recur (inc index) (rest myseq)))))))

(repeatsat 34 [[0 0] [0 34] [0 98] [0 45]])

(defn find-result
  [divisionseq resultsofar]
  (let [thefirstelem (first divisionseq)
        quotient (first thefirstelem), remainder (second thefirstelem)
        ]
    (if (nil? thefirstelem)
      [resultsofar,[]] ;; no repeating part
      (let [repeatpoint (repeatsat remainder resultsofar)]
        (if repeatpoint
          [(subvec resultsofar 0 repeatpoint), (subvec resultsofar repeatpoint)]
          (recur (rest divisionseq) (conj resultsofar thefirstelem)))))))

(defn find-division [numerator denominator]
  (let [quotient (quot numerator denominator)
        remainder (rem numerator denominator)
        divisionsequence (drop 1 (divisionseq-helper remainder denominator))
        fractionresult (find-result divisionsequence [])]
    [quotient, (first fractionresult) (second fractionresult)]))

(count (third (find-division 1 49)))
(take 50 (divisionseq-helper 1 49))
(find-result [1 4 2 8 5 7 1 4 2 8 5 7 1 4 3] [])        
(def myresult (find-division 1 49))
(take 20 (divisionseq-helper 1 250))



;; deisred end result
;; [integerPart "." "non repeating part" "repeating part"]

