
;; Consider a number 00000 00000 00000 00000 11111 11111 11111 11111
;; 0 corresponds to a horizontal move. 1 to a vertical move.
;; The number of permutations of this number will be the possible ways
;; in which grid can be traversed from top left to bottom right.
;; Total there are 2 * 40 moves.
;; As 0 is repeated 20 times and so is 1
;; So total permutations are (40!) /(20! X 20!)

(defn fact
  ([n] (fact n 1))
  ([n accumulator]
     (if (== n 0)
       accumulator
       (recur (dec n) (* n accumulator)))))

(time (-> (fact 40)
           (/ (fact 20))
           (/ (fact 20))))