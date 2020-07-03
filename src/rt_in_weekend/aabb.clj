(ns rt-in-weekend.aabb
  (:require [rt-in-weekend.ray :as ray]
            [rt-in-weekend.vec :as vec]))

(defn make [a b]
  {:min a :max b})

(defn mini [aabb]
  (:min aabb))

(defn maxi [aabb]
  (:max aabb))

(defn t-at-hitpoint [x origin direction]
  (/ (- x origin) direction))

(defn hit [aabb r tmin tmax]
  (loop [i 0]
    (let [idx #(get % i)
          th0 (t-at-hitpoint (idx (mini aabb)) (idx (ray/origin r)) (idx (ray/direction r)))
          th1 (t-at-hitpoint (idx (maxi aabb)) (idx (ray/origin r)) (idx (ray/direction r)))
          t0 (min th0 th1)
          t1 (max th0 th1)
          tmin (max t0 tmin)
          tmax (max t1 tmax)]
      (if (< i 3)
        (if (<= tmax tmin)
          false
          (recur (inc i)))
        true))))

; Optimized version of the function above proposed by Andrew Kesler at Pixar.
(defn hit-pixar [aabb r tmin tmax]
  (loop [i 0]
    (let [idx #(get % i)
          invD (/ 1.0 (idx (ray/direction r)))
          t0 (* (- (idx (mini aabb)) (idx (ray/origin r))) invD)
          t1 (* (- (idx (maxi aabb)) (idx (ray/origin r))) invD)
          t0 (if (neg? invD) t1 t0)
          t1 (if (neg? invD) t0 t1)
          tmin (if (> t0 tmin) t0 tmin)
          tmax (if (< t1 tmax) t1 tmax)]
      (if (< i 3)
        (if (<= tmax tmin)
          false
          (recur (inc i)))
        true))))
