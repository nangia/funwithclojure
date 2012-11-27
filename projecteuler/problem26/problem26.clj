

(defn divisionseq-helper [numerator denominator]
  (let [quotient (quot numerator denominator)
        remainder (rem numerator denominator)]
    (lazy-cat [ [quotient remainder] ]
              (if (zero? remainder)
                []
                (lazy-seq (divisionseq-helper (* remainder 10) denominator))))))

(defn repeatsat [numerator the-seq]
  (loop [index 0, myseq the-seq]
    (let [firstelem (first myseq)]
      (if (nil? firstelem)
        nil; not found
        (if (== firstelem numerator)
          index
          (recur (inc index) (rest myseq)))))))


(defn computeresult [numerator denominator]
  (let [divisionsequence (divisionseq-helper numerator denominator)
        beforedecimal ((first divisionsequence) 0)
        afterdecimalseq (rest divisionsequence)]
    (loop [
           result [],
           thesequence afterdecimalseq
           iter 0
           _ (pprint iter)
           _ (pprint result)
           _ (pprint (first thesequence))
           ]
      (let [thevector (first thesequence) ]
        (let [upnumber (thevector 0), downnumber (thevector 1), newresult (conj result upnumber)]
          (if (zero? downnumber)
            [newresult, nil]
            (let [index (repeatsat downnumber result)]
              (if (not (nil? index))
                [newresult, index ]
                (recur newresult  (rest afterdecimalseq) (inc iter) (pprint (inc iter)) (pprint newresult) (pprint (first (rest (afterdecimalseq)))))))))))))
              
              
          
          



;; deisred end result
;; [integerPart "." "non repeating part" "repeating part"]

