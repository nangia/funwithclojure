;2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?


(defn gcd [a b] (if (zero? b) a (gcd b (rem a b))))

(defn lcm [a b] (* (/ a (gcd a b)) b))

(reduce lcm (range 1 21))






