(defn fact
  [x]
  (if (zero? x)
    1N
    (* x (fact (dec x)))))

(defn sumofdigits
  [x]
  (reduce + (map #(- (int %1) (int \0)) (str x))))

(sumofdigits (fact 100))
