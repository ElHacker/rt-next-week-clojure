(ns rt-in-weekend.hittable
  (:require [rt-in-weekend.vec :as vec]
            [rt-in-weekend.ray :as ray]
            [rt-in-weekend.aabb :as aabb]))

(defprotocol Hittable
  (hit [this r t-min t-max])
  (bounding-box [this t0 t1])
  (center [this timestamp]))

(defn hit-record [r t center radius material]
  (let [p (ray/point-at r t)
        outward-normal (vec// (vec/- p center) radius)
        front-face (< (vec/dot (:direction r) outward-normal) 0)]
    {:t t :p p :normal (if front-face outward-normal (vec/- outward-normal)) :material material :front-face front-face}))

(defrecord Sphere [center radius material]
  Hittable
  (hit [this r t-min t-max]
    (let [oc (vec/- (ray/origin r) (:center this))
          a (vec/length-squared (ray/direction r))
          half-b (vec/dot oc (ray/direction r))
          c (- (vec/length-squared oc) (* (:radius this) (:radius this)))
          discriminant (- (* half-b half-b) (* a c))]
      (when (pos? discriminant)
        (let [root (Math/sqrt discriminant)
              temp (/ (- (- half-b) root) a)]
          (if (and (< temp t-max) (> temp t-min))
            (hit-record r temp (:center this) (:radius this) (:material this))
            (let [temp (/ (+ (- half-b) root) a)]
              (when (and (< temp t-max) (> temp t-min))
                (hit-record r temp (:center this) (:radius this) (:material this)))))))))

  (bounding-box [this t0 t1]
    (let [output-box (aabb/make (vec/- (:center this) [(:radius this) (:radius this) (:radius this)])
                                (vec/+ (:center this) [(:radius this) (:radius this) (:radius this)]))]
      {:has-bbox true :output-box output-box}))

  (center [this timestamp]
    (:center this)))

; Moves linearly between center0 at time0 to center1 at time1. Outside that
; time interval it continues on, so those times need not match up with the
; camera aperture open and close.
(defrecord MovingSphere [center0 center1 time0 time1 radius material]
  Hittable
  (center [this timestamp]
    (vec/+ center0
           (vec/* (vec/- center1 center0)
                  (/ (- timestamp time0)
                     (- time1 time0)))))

  (hit [this r t-min t-max]
    (let [oc (vec/- (ray/origin r) (center this (:timestamp r)))
          a (vec/length-squared (ray/direction r))
          half-b (vec/dot oc (ray/direction r))
          c (- (vec/length-squared oc) (* (:radius this) (:radius this)))
          discriminant (- (* half-b half-b) (* a c))]
      (when (pos? discriminant)
        (let [root (Math/sqrt discriminant)
              temp (/ (- (- half-b) root) a)]
          (if (and (< temp t-max) (> temp t-min))
            (hit-record r temp (center  this (:timestamp r)) (:radius this) (:material this))
            (let [temp (/ (+ (- half-b) root) a)]
              (when (and (< temp t-max) (> temp t-min))
                (hit-record r temp (center this (:timestamp r)) (:radius this) (:material this)))))))))

  (bounding-box [this t0 t1]
    (let [box0 (aabb/make (vec/- (center this t0) [(:radius this) (:radius this) (:radius this)])
                          (vec/+ (center this t0) [(:radius this) (:radius this) (:radius this)]))
          box1 (aabb/make (vec/- (center this t1) [(:radius this) (:radius this) (:radius this)])
                          (vec/+ (center this t1) [(:radius this) (:radius this) (:radius this)]))
          output-box (aabb/surrounding-box box0 box1)]
      {:has-bbox true :output-box output-box})))

(defn hittable-list [world r t-min t-max]
  (let [closest-so-far (atom t-max)
        record (atom nil)]
    (doseq [i (range 0 (count world))]
      (do
        (if-let [rec (hit (get world i) r t-min @closest-so-far)]
          (do
            (reset! closest-so-far (:t rec))
            (reset! record rec)))))
    @record))

(defn hittable-list-bounding-box [world out-box t0 t1]
  (if (empty? world)
    false
    (let [output-box (atom out-box)
          first-box (atom true)]
      (doseq [i (range 0 (count world))]
        (let [temp-box (bounding-box (get world i) t0 t1)]
          (if (not (:has-bbox temp-box))
            false
            (do
              (reset! output-box (if first-box temp-box (aabb/surrounding-box output-box temp-box)))
              (reset! first-box false)))))
      {:has-bbox true :output-box @output-box})))
