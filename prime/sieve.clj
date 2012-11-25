

(defn sieve-next [sieve candidate]
  (let [isnotaprime (some #(zero? (mod candidate %)) sieve)]
    (if isnotaprime
      (recur sieve (+ 2 candidate))
      (conj sieve candidate))))

(def primes (map last (iterate #(sieve-next %1 (+ 2 (last %1)))  [2 3])))

(take 10 primes)


;(defn sieve-next [sieve candidate]
;  (let [isnotaprime (some #(zero? (mod candidate (first %))) sieve)] ; TODO: optimize till the square of the number is less than candidate being checked
;    (if isnotaprime
;      (recur sieve (+ 2 candidate))
;      (conj sieve [candidate (* candidate candidate)]))))
	  
;(def q (clojure.lang.PersistentQueue/EMPTY))
	  
;(def primes 
;	(map first (map last (iterate #(sieve-next %1 (+ 2 (last %1)))  (conj (conj q [2 4]] [3 9]))))))