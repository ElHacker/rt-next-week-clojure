(ns rt-in-weekend.texture
  (:require [clojure.algo.generic.math-functions :as math]
            [rt-in-weekend.vec :as vec]
            [rt-in-weekend.perlin :as perlin]))

(defprotocol Texture
  (value [this u v point]))

(defrecord SolidColor [color]
  Texture
  (value [this u v point]
    color))

(defrecord CheckerTexture [even odd]
  Texture
  (value [this u v point]
    (let [sines (reduce * (map math/sin (vec/* point 10)))]
      (if (neg? sines)
        (value odd u v point)
        (value even u v point)))))

(defrecord NoiseTexture [scale]
  Texture
  (value [this u v point]
    (vec/* [1.0 1.0 1.0] (perlin/noise (vec/* point scale)))))
