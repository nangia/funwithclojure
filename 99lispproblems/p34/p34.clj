

(defn gcd [a b] (if (zero? b) a (gcd b (rem a b))))

(defn coprime? [a b] (== 1 (gcd a b)))

(defn phi [m] 
  (count (filter #(coprime? m %1) (range 1 (inc m)))))