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
    (if (< i 3)
      (let [th0 (t-at-hitpoint (get (mini aabb) i) (get (ray/origin r) i) (get (ray/direction r) i))
            th1 (t-at-hitpoint (get (maxi aabb) i) (get (ray/origin r) i) (get (ray/direction r) i))
            t0 (min th0 th1)
            t1 (max th0 th1)
            tmin (max t0 tmin)
            tmax (min t1 tmax)]
        (if (<= tmax tmin)
          false
          (recur (inc i))))
      true)))

; Optimized version of the function above proposed by Andrew Kensler at Pixar.
(defn hit-pixar [aabb r tmin tmax]
  (loop [i 0]
    (if (< i 3)
      (let [invD (/ 1.0 (get (ray/direction r) i))
            t0 (* (- (get (mini aabb) i) (get (ray/origin r) i)) invD)
            t1 (* (- (get (maxi aabb) i) (get (ray/origin r) i)) invD)
            tempt0 t0
            tempt1 t1
            t0 (if (neg? invD) tempt1 tempt0)
            t1 (if (neg? invD) tempt0 tempt1)
            tmini (if (> t0 tmin) t0 tmin)
            tmaxi (if (< t1 tmax) t1 tmax)]
        (if (> tmaxi tmini)
          false
          (recur (inc i))))
      true)))

(defn surrounding-box [box0 box1]
  (let [small [(min (vec/x (mini box0))
                    (vec/x (mini box1)))
               (min (vec/y (mini box0))
                    (vec/y (mini box1)))
               (min (vec/z (mini box0))
                    (vec/z (mini box1)))]
        big [(max (vec/x (maxi box0))
                  (vec/x (maxi box1)))
             (max (vec/y (maxi box0))
                  (vec/y (maxi box1)))
             (max (vec/z (maxi box0))
                  (vec/z (maxi box1)))]]
    (make small big)))
