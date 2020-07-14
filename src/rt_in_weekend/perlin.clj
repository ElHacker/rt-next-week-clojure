(ns rt-in-weekend.perlin
  (:require [clojure.algo.generic.math-functions :as math]
            [rt-in-weekend.vec :as vec]
            [clojure.core :as clj]))

(def point-count 256)

(defn trilinear-interpolation [c u v w]
  (let [accum (atom 0.0)]
    (doseq [i (range 2)]
      (doseq [j (range 2)]
        (doseq [k (range 2)]
          (swap! accum + (* (+ (* i u) (* (- 1 i) (- 1 u)))
                            (+ (* j v) (* (- 1 j) (- 1 v)))
                            (+ (* k w) (* (- 1 k) (- 1 w)))
                            (get-in c [i j k]))))))
    @accum))

(defn perlin-generate-perm []
  (let [points (clj/vec (range point-count))]
    (shuffle points)))

; NOTE: prefer to call the perlin-memoize version to avoid recomputing the permutations (based on
; some tests I was able to get about 30X performance improvement)
; and also to keep the same permutations around for each texture/material scatter call and allow
; a cohesive image.
(defn perlin []
  (let [ranfloat (clj/vec (take point-count (repeatedly rand)))
        perm-x (perlin-generate-perm)
        perm-y (perlin-generate-perm)
        perm-z (perlin-generate-perm)]
    {:ranfloat ranfloat :perm-x perm-x :perm-y perm-y :perm-z perm-z}))

(def perlin-memoize (memoize perlin))

(defn noise [point]
  (let [perlin-res (perlin-memoize)
        u (- (vec/x point) (math/floor (vec/x point)))
        v (- (vec/y point) (math/floor (vec/y point)))
        w (- (vec/z point) (math/floor (vec/z point)))
        i (math/floor (vec/x point))
        j (math/floor (vec/y point))
        k (math/floor (vec/z point))
        c (atom (vec (replicate 2 (vec (replicate 2 [0 0])))))]
    (doseq [di (range 2)]
      (doseq [dj (range 2)]
        (doseq [dk (range 2)]
          (let [index (bit-xor (get (:perm-x perlin-res) (bit-and (int (+ i di)) 255))
                               (get (:perm-y perlin-res) (bit-and (int (+ j dj)) 255))
                               (get (:perm-z perlin-res) (bit-and (int (+ k dk)) 255)))]
            (swap! c assoc-in [di dj dk] (get (:ranfloat perlin-res) index))))))
    (trilinear-interpolation @c u v w)))
