(ns rt-in-weekend.hittable
  (:require [rt-in-weekend.vec :as vec]
            [rt-in-weekend.ray :as ray]
            [rt-in-weekend.aabb :as aabb]
            [rt-in-weekend.util :as util]
            [rt-in-weekend.material :as materials]
            [clojure.algo.generic.math-functions :as math]))

(defprotocol Hittable
  (hit [this r t-min t-max])
  (bounding-box [this t0 t1])
  (center [this timestamp]))

(defn hit-record
  ([r t center radius material uvfn]
   (let [p (ray/point-at r t)
         outward-normal (vec// (vec/- p center) radius)
         front-face (< (vec/dot (:direction r) outward-normal) 0)
         uv (uvfn p)
         {u :u
          v :v} uv]
     {:t t :u u :v v :p p :normal (if front-face outward-normal (vec/- outward-normal)) :material material :front-face front-face}))
  ([r t center radius material]
   (hit-record r t center radius material #({:u 0 :v 0}))))

(defrecord Sphere [center radius material]
  Hittable
  (hit [this r t-min t-max]
    (let [oc (vec/- (ray/origin r) (:center this))
          a (vec/length-squared (ray/direction r))
          half-b (vec/dot oc (ray/direction r))
          c (- (vec/length-squared oc) (* (:radius this) (:radius this)))
          discriminant (- (* half-b half-b) (* a c))
          uvfn #(util/get-sphere-uv (vec// (vec/- % (:center this)) (:radius this)))]
      (when (pos? discriminant)
        (let [root (Math/sqrt discriminant)
              temp (/ (- (- half-b) root) a)]
          (if (and (< temp t-max) (> temp t-min))
            (hit-record r temp (:center this) (:radius this) (:material this) uvfn)
            (let [temp (/ (+ (- half-b) root) a)]
              (when (and (< temp t-max) (> temp t-min))
                (hit-record r temp (:center this) (:radius this) (:material this) uvfn))))))))

  (bounding-box [this t0 t1]
    (let [output-box (aabb/make (vec/- (:center this) [(:radius this) (:radius this) (:radius this)])
                                (vec/+ (:center this) [(:radius this) (:radius this) (:radius this)]))]
      output-box))

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
          discriminant (- (* half-b half-b) (* a c))
          uvfn #(util/get-sphere-uv (vec// (vec/- % (center this (:timestamp r))) (:radius this)))]
      (when (pos? discriminant)
        (let [root (Math/sqrt discriminant)
              temp (/ (- (- half-b) root) a)]
          (if (and (< temp t-max) (> temp t-min))
            (hit-record r temp (center  this (:timestamp r)) (:radius this) (:material this) uvfn)
            (let [temp (/ (+ (- half-b) root) a)]
              (when (and (< temp t-max) (> temp t-min))
                (hit-record r temp (center this (:timestamp r)) (:radius this) (:material this) uvfn))))))))

  (bounding-box [this t0 t1]
    (let [box0 (aabb/make (vec/- (center this t0) [(:radius this) (:radius this) (:radius this)])
                          (vec/+ (center this t0) [(:radius this) (:radius this) (:radius this)]))
          box1 (aabb/make (vec/- (center this t1) [(:radius this) (:radius this) (:radius this)])
                          (vec/+ (center this t1) [(:radius this) (:radius this) (:radius this)]))
          output-box (aabb/surrounding-box box0 box1)]
      output-box)))

; XY Axis aligned rectangle
(defrecord XYRect [x0 x1 y0 y1 k material]
  Hittable
  (center [this timestamp] nil)

  (hit [this r t-min t-max]
    (let [t (/ (- k (vec/z (:origin r)))
               (vec/z (:direction r)))
          x (+ (vec/x (:origin r)) (* t (vec/x (:direction r))))
          y (+ (vec/y (:origin r)) (* t (vec/y (:direction r))))
          u (/ (- x x0) (- x1 x0))
          v (/ (- y y0) (- y1 y0))
          outward-normal [0 0 1]
          front-face (< (vec/dot (:direction r) outward-normal) 0)
          p (ray/point-at r t)]
      (when (and (>= t t-min) (<= t t-max) (>= x x0) (<= x x1) (>= y y0) (<= y y1))
        {:t t :u u :v v :p p :normal (if front-face outward-normal (vec/- outward-normal)) :material material :front-face front-face})))

  ; The bounding box must have non-zero width in each dimension, so pad the Z
  ; dimension a small amount
  (bounding-box [this t0 t1]
    (let [output-box (aabb/surrounding-box [x0 y0 (- k 0.0001)] [x1 y1 (+ k 0.0001)])]
      output-box)))

; XZ Axis aligned rectangle
(defrecord XZRect [x0 x1 z0 z1 k material]
  Hittable
  (center [this timestamp] nil)

  (hit [this r t-min t-max]
   (let [t (/ (- k (vec/y (:origin r)))
               (vec/y (:direction r)))
          x (+ (vec/x (:origin r)) (* t (vec/x (:direction r))))
          z (+ (vec/z (:origin r)) (* t (vec/z (:direction r))))
          u (/ (- x x0) (- x1 x0))
          v (/ (- z z0) (- z1 z0))
          outward-normal [0 1 0]
          front-face (< (vec/dot (:direction r) outward-normal) 0)
          p (ray/point-at r t)]
      (when (and (>= t t-min) (<= t t-max) (>= x x0) (<= x x1) (>= z z0) (<= z z1))
        {:t t :u u :v v :p p :normal (if front-face outward-normal (vec/- outward-normal)) :material material :front-face front-face})))

  ; The bounding box must have non-zero width in each dimension, so pad the Y
  ; dimension a small amount
  (bounding-box [this t0 t1]
    (let [output-box (aabb/surrounding-box [x0 (- k 0.0001) z0] [x1 (+ k 0.0001) z1])]
      output-box)))

; YZ Axis aligned rectangle
(defrecord YZRect [y0 y1 z0 z1 k material]
  Hittable
  (center [this timestamp] nil)

  (hit [this r t-min t-max]
    (let [t (/ (- k (vec/x (:origin r)))
               (vec/x (:direction r)))
          y (+ (vec/y (:origin r)) (* t (vec/y (:direction r))))
          z (+ (vec/z (:origin r)) (* t (vec/z (:direction r))))
          u (/ (- y y0) (- y1 y0))
          v (/ (- z z0) (- z1 z0))
          outward-normal [1 0 0]
          front-face (< (vec/dot (:direction r) outward-normal) 0)
          p (ray/point-at r t)]
      (when (and (>= t t-min) (<= t t-max) (>= y y0) (<= y y1) (>= z z0) (<= z z1))
        {:t t :u u :v v :p p :normal (if front-face outward-normal (vec/- outward-normal)) :material material :front-face front-face})))

  ; The bounding box must have non-zero width in each dimension, so pad the X
  ; dimension a small amount
  (bounding-box [this t0 t1]
    (let [output-box (aabb/surrounding-box [(- k 0.0001) y0 z0] [(+ k 0.0001) y1 z1])]
      output-box)))

(defrecord bvh-node [hittable-objects start end time0 time1 left right surrounding-box]
  Hittable
  (center [this timestamp] nil)

  (hit [this r t-min t-max]
    (if (aabb/hit-pixar surrounding-box r t-min t-min)
      (let [hit-left (hit left r t-min t-max)
            hit-right (hit right r t-min (if hit-left (:t hit-left) t-max))]
        (or hit-left hit-right))))

  (bounding-box [this t0 t1] surrounding-box))

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
    (if (or (not  box-a)
            (not  box-b))
      (throw (Exception. "No bounding box in bvh-node constructor"))
      (compare (get (aabb/mini box-a) axis) (get (aabb/mini box-b) axis)))))

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
                    (let [sorted (vec (sort comparator-fn hittable-objects))
                          mid (+ start (quot object-span 2))]
                      (do
                        (reset! left (bvh-node-split-build sorted start mid time0 time1))
                        (reset! right (bvh-node-split-build sorted mid end time0 time1))))))
      (if (or (nil? @left)
              (nil? @right))
        (println "Error: No bounding box in bvh-node constructor"))
      (let [box-left (if (some? @left) (bounding-box @left time0 time1) nil)
            box-right (if (some? @right) (bounding-box @right time0 time1) nil)]
        ; Return an instance of bvh-node
        (->bvh-node hittable-objects start end time0 time1 @left @right (aabb/surrounding-box box-left box-right))))))

(defn hittable-list-bounding-box [world t0 t1 out-box]
  (if (empty? world)
    nil
    (let [output-box (atom out-box)
          first-box (atom true)]
      (doseq [i (range 0 (count world))]
        (let [temp-box (bounding-box (get world i) t0 t1)]
          (if (not temp-box)
            false
            (do
              (reset! output-box (if first-box temp-box (aabb/surrounding-box output-box temp-box)))
              (reset! first-box false)))))
      @output-box)))

(defn create-box-sides [p0 p1 material]
  (let [sides (atom [(->XYRect (vec/x p0) (vec/x p1) (vec/y p0) (vec/y p1) (vec/z p1) material)
                     (->XYRect (vec/x p0) (vec/x p1) (vec/y p0) (vec/y p1) (vec/z p0) material)

                     (->XZRect (vec/x p0) (vec/x p1) (vec/z p0) (vec/z p1) (vec/y p1) material)
                     (->XZRect (vec/x p0) (vec/x p1) (vec/z p0) (vec/z p1) (vec/y p0) material)

                     (->YZRect (vec/y p0) (vec/y p1) (vec/z p0) (vec/z p1) (vec/x p1) material)
                     (->YZRect (vec/y p0) (vec/y p1) (vec/z p0) (vec/z p1) (vec/x p0) material)])]
    @sides))

(def create-box-sides-memoize (memoize create-box-sides))

(defrecord Box [p0 p1 material]
  Hittable
  (center [this timestamp] nil)

  (hit [this r t-min t-max]
    (let [box-sides (create-box-sides p0 p1 material)]
      (hittable-list box-sides r t-min t-max)))

  (bounding-box [this t0 t1]
    (let [output-box (aabb/make p0 p1)]
      output-box)))

(defrecord Translate [hittable displacement]
  Hittable
  (center [this timestamp] nil)

  (hit [this r t-min t-max]
    (let [moved-r (ray/make (vec/- (ray/origin r) displacement) (ray/direction r) (ray/timestamp r))]
      (if-let [rec (hit hittable moved-r t-min t-max)]
        (let [p (vec/+ (:p rec) displacement)
              {t :t
               u :u
               v :v
               normal :normal
               material :material
               front-face :front-face} rec]
          {:t t :u u :v v :p p :normal normal :material material :front-face front-face}))))

  (bounding-box [this t0 t1]
    (if-let [hittable-output-box (bounding-box hittable t0 t1)]
      (aabb/make (vec/+ (aabb/mini hittable-output-box) displacement)
                 (vec/+ (aabb/maxi hittable-output-box) displacement)))))

(defrecord RotateY [hittable angle]
  Hittable
  (center [this timestamp] nil)

  (hit [this r t-min t-max]
    (let [radians (Math/toRadians angle)
          sin-theta (math/sin radians)
          cos-theta (math/cos radians)
          origin-x (- (* cos-theta (vec/x (ray/origin r)))
                      (* sin-theta (vec/z (ray/origin r))))
          origin-z (+ (* sin-theta (vec/x (ray/origin r)))
                      (* cos-theta (vec/z (ray/origin r))))
          direction-x (- (* cos-theta (vec/x (ray/direction r)))
                         (* sin-theta (vec/z (ray/direction r))))
          direction-z (+ (* sin-theta (vec/x (ray/direction r)))
                         (* cos-theta (vec/z (ray/direction r))))
          origin [origin-x (vec/y (ray/origin r)) origin-z]
          direction [direction-x (vec/y (ray/direction r)) direction-z]
          rotated-r (ray/make origin direction (ray/timestamp r))]
      (if-let [rec (hit hittable rotated-r t-min t-max)]
        (let [p-x (+ (* cos-theta (vec/x (:p rec)))
                     (* sin-theta (vec/z (:p rec))))
              p-z (+ (- (* sin-theta (vec/x (:p rec))))
                     (* cos-theta (vec/z (:p rec))))
              normal-x (+ (* cos-theta (vec/x (:normal rec)))
                          (* sin-theta (vec/z (:normal rec))))
              normal-z (+ (- (* sin-theta (vec/x (:normal rec))))
                          (* cos-theta (vec/z (:normal rec))))
              p [p-x (vec/y (:p rec)) p-z]
              normal [normal-x (vec/y (:normal rec)) normal-z]
              {t :t
               u :u
               v :v
               material :material
               front-face :front-face} rec]
          {:t t :u u :v v :p p :normal normal :material material :front-face front-face}))))

  (bounding-box [this t0 t1]
    (let [radians (Math/toRadians angle)
          sin-theta (math/sin radians)
          cos-theta (math/cos radians)
          bbox-min (atom [##Inf ##Inf ##Inf])
          bbox-max (atom [##-Inf ##-Inf ##-Inf])
          bbox (bounding-box hittable 0 1)]
      (doseq [i (range 2)]
        (doseq [j (range 2)]
          (doseq [k (range 2)]
            (let [compute-coordinate #(+ (* %1 (%2 (aabb/maxi bbox)))
                                         (* (- 1 %1 (%2 (aabb/mini bbox)))))
                  x (compute-coordinate i vec/x)
                  y (compute-coordinate j vec/y)
                  z (compute-coordinate k vec/z)
                  new-x (+ (* cos-theta x) (* sin-theta z))
                  new-z (+ (- (* sin-theta x)) (* cos-theta z))
                  tester [new-x y new-z]]
              (doseq [c (range 3)]
                (do
                  (swap! bbox-min assoc c (min (get @bbox-min c) (get tester c)))
                  (swap! bbox-max assoc c (max (get @bbox-max c) (get tester c)))))))))
      (aabb/make @bbox-min @bbox-max))))

(defrecord ConstantMedium [hittable density texture]
  Hittable
  (center [this timestmap] nil)

  (hit [this r t-min t-max]
    (if-let [rec1 (hit hittable r ##-Inf ##Inf)]
      (if-let [rec2 (hit hittable r (+ (:t rec1) 0.0001) ##Inf)]
        (let [rec1 (if (< (:t rec1) t-min)
                     (assoc rec1 :t t-min)
                     rec1)
              rec2 (if (> (:t rec2) t-max)
                     (assoc rec2 :t t-max)
                     rec2)]
          (if (< (:t rec1) (:t rec2))
            (let [rec1 (if (< (:t rec1) 0)
                        (assoc rec1 :t 0)
                        rec1)
                  ray-length (vec/length (ray/direction r))
                  distance-inside-boundary (* (- (:t rec2) (:t rec1)) ray-length)
                  neg-inv-density (/ -1 density)
                  hit-distance (* neg-inv-density (math/log (rand)))]
              (if (<= hit-distance distance-inside-boundary)
                (let [t (+ (:t rec1) (/ hit-distance ray-length))
                      p (ray/point-at r t)
                      normal [1 0 0] ; Arbitrary value
                      front-face true ; Also arbitrary
                      material (materials/->Isotropic texture)]
                  ; NOTE: I'm not sure if defaulting u and v to 0 is correct here.
                  {:t t :u 0 :v 0 :p p :normal normal :material material :front-face front-face}))))))))

  (bounding-box [this t0 t1]
    (bounding-box hittable t0 t1)))
