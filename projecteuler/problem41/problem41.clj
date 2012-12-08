;We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. 
;For example, 2143 is a 4-digit pandigital and is also prime.
;What is the largest n-digit pandigital prime that exists?

(defn permute [avector]
  ;; given a vector say [4 3 2 1] produces the permutations in reverse
  ;; lexicographic order. Expected that the arguments will have numbers
  ;; in decreasing order
  (if (== 1 (count avector))
    (lazy-cat [avector])
    (mapcat
     (fn [[x restx]] (map #(cons x %1) (permute restx)))
     ;(fn [[x restx]] (map #(conj %1 x) (permute restx)))
     (for [anelement avector]
       [anelement (remove (fn equaltoanelement [x] (== anelement x)) avector)]))))

(defn permrevseq->num [theseq]
  ;; Given a seq [1 2 3 4] maps it to 4321
  ;; used as a helper by function permseq->num
  (let [restseq (rest theseq), firstinseq (first theseq)]
    (if (empty? restseq)
      firstinseq
      (+ firstinseq 
         (* 10 (permrevseq->num restseq))))))

(defn permseq->num [theseq]
  ;; Given a sequence [1 2 3 4] maps it to the number 1234
  (permrevseq->num (reverse theseq)))

(defn permseq->num [theseq]
  (Integer/parseInt (apply str theseq)))

(defn prime? [x]
  (let [sqrtx (inc (Math/ceil (Math/sqrt x)))]
    (every? #(not= 0 (mod x %1)) (range 2 sqrtx))))


(defn problem41 [n]
  (->> (range n 1 -1)
       (map #(range %1 0 -1))
       (map (fn[x] (apply vector x)))
       (mapcat permute)
       (map permseq->num)
       (filter prime?)
       (take 1)))

(problem41 9)