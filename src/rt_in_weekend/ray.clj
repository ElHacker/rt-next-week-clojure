(ns rt-in-weekend.ray
  (:require [rt-in-weekend.vec :as vec]))

(defn make [origin direction timestamp]
  {:origin origin :direction direction :timestamp timestamp})

(defn origin [ray]
  (:origin ray))

(defn direction [ray]
  (:direction ray))

(defn timestamp [ray]
  (:timestamp ray))

(defn point-at [ray t]
  (vec/+ (origin ray)
         (vec/* (direction ray) t)))

(defn hit-sphere [center radius {:keys [origin direction]}]
  (let [oc (vec/- origin center)
        a (vec/length-squared direction)
        half-b (vec/dot oc direction)
        c (- (vec/length-squared oc) (* radius radius))
        discriminant (- (* half-b half-b) (* a c))]
    (if (neg? discriminant)
      -1.0
      (/ (- (- half-b)
            (Math/sqrt discriminant))
         a))))
