;; Find ways to place n queens on a n x n chess board

(defn safe? [x1 y1 x2 y2]
  (and (not= x1 x2)
       (not= y1 y2)
       (not= (Math/abs (- x2 x1)) 
          (Math/abs (- y2 y1)))))

(defn threatens-none? [x y positions]
  (every? #(safe? x y (%1 0) (%1 1)) positions))

(defn next-iter [listOfQueenPositions n k]
  (let [new-y (- n k)]
    (for [x (range 0 n) :when (threatens-none? x new-y listOfQueenPositions)]
      (cons [x new-y] listOfQueenPositions))))

(defn queens 
  ([n] (queens (list '()) n n))
  ([partial-solved n k]
     ;; partial-solved is partial solution
     ;; i.e. on the board, (n-k) queens have been placed
     ;; such that rows 0..k have queens
     (if (zero? k)
       partial-solved
       (recur (mapcat #(next-iter %1 n k) partial-solved)
              n
              (dec k)))))

