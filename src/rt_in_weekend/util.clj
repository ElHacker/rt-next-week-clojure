(ns rt-in-weekend.util)

(defn rand-in-range "Generates a random number within range" [a b]
  (+ a (rand (- b a))))
