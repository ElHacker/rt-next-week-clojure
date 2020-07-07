(ns rt-in-weekend.util
  (:require [clojure.algo.generic.math-functions :as math]
            [rt-in-weekend.vec :as vec]))

(defn rand-in-range "Generates a random number within range" [a b]
  (+ a (rand (- b a))))

(defn get-sphere-uv [point]
  (let [phi (math/atan2 (vec/z point) (vec/x point))
        theta (math/asin (vec/y point))
        u (- 1
             (/ (+ phi Math/PI)
                (* 2 Math/PI)))
        v (/ (+ theta (/ Math/PI 2))
             Math/PI)]
    {:u u :v v}))
