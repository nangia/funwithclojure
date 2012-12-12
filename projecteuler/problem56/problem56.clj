

(defn sumofdigits [n]
  (->> (str n)
       (map #(- (int %1) (int \0)))
       (reduce +)))

(defn power
  ([a b cumulator]
  (if (== b 0)
    cumulator
    (recur a (dec b) (* cumulator a))))
  ([a b]
     (power a b 1)))

(defn problem56[]
  (reduce max 
         (map sumofdigits
              (for [a (range 1 100), b (range 1 100)]
                (power a b)))))


(time (problem56))