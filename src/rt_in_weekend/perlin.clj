(ns rt-in-weekend.perlin
  (:require [clojure.algo.generic.math-functions :as math]
            [rt-in-weekend.vec :as vec]
            [clojure.core :as clj]))

(def point-count 256)

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
        i (bit-and (int (* 4 (vec/x point))) 255)
        j (bit-and (int (* 4 (vec/y point))) 255)
        k (bit-and (int (* 4 (vec/z point))) 255)
        index (bit-xor (get (:perm-x perlin-res) i)
                       (get (:perm-y perlin-res) j)
                       (get (:perm-z perlin-res) k))]
    (get (:ranfloat perlin-res) index)))
