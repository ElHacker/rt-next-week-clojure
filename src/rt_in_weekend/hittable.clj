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

(defrecord bvh-node [hittable-objects start end time0 time1 left right box]
  Hittable
  (center [this timestamp] nil)

  (hit [this r t-min t-max]
    (if (not (aabb/hit-pixar box r t-min t-min))
      false
      (let [rec (hit this r t-min t-max) ; TODO(elhacker): I don't think I should compute hit on this.
            hit-left (hit left r t-min t-max)
            hit-right (hit right r t-min (if hit-left (:t rec) t-max))]
        (or hit-left hit-right))))

  (bounding-box [this t0 t1]
    {:has-bbox true :output-box box}))

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

(defn box-compare [hittable-a hittable-b axis]
  (let [box-a (bounding-box hittable-a 0 0)
        box-b (bounding-box hittable-b 0 0)]
    (if (or (not (:has-bbox box-a))
            (not (:has-bbox box-b)))
      (throw (Exception. "No bounding box in bvh-node constructor"))
      (compare (get (aabb/mini (:output-box box-a)) axis) (get (aabb/mini (:output-box box-b)) axis)))))

(defn box-x-compare [hittable-a hittable-b]
  (box-compare hittable-a hittable-b 0))

(defn box-y-compare [hittable-a hittable-b]
  (box-compare hittable-a hittable-b 1))

(defn box-z-compare [hittable-a hittable-b]
  (box-compare hittable-a hittable-b 2))

(defn bvh-node-split-build [hittable-objects start end time0 time1]
  (let [axis (rand-int 3)
        comparator-fn (cond
                        (== axis 0) box-x-compare
                        (== axis 1) box-y-compare
                        :else box-z-compare)
        object-span (- end start)
        left (atom nil)
        right (atom nil)]
    (do
      (cond (== object-span 1) (do
                                 (reset! left (get hittable-objects start))
                                 (reset! right (get hittable-objects start)))
            (== object-span 2) (if (comparator-fn (get hittable-objects start) (get hittable-objects (inc start)))
                                 (do
                                   (reset! left (get hittable-objects start))
                                   (reset! right (get hittable-objects (inc start))))
                                 (do
                                   (reset! left (get hittable-objects (inc start)))
                                   (reset! right (get hittable-objects start))))
            :else (do
                    (let [sorted (sort comparator-fn hittable-objects)
                          mid (/ (+ start object-span) 2)]
                      (do
                        (reset! left (bvh-node-split-build sorted start mid time0 time1))
                        (reset! right (bvh-node-split-build sorted mid end time0 time1))))))
      (let [box-left (bounding-box @left time0 time1)
            box-right (bounding-box @right time0 time1)]
        (if (or (not (:has-bbox box-left))
                (not (:has-bbox box-right)))
          (throw (Exception. "No bounding box in bvh-node constructor"))
          ; Return an instance of bvh-node
          (->bvh-node hittable-objects start end time0 time1 left right (aabb/surrounding-box box-left box-right)))))))

(defn hittable-list-bounding-box [world t0 t1 out-box]
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

