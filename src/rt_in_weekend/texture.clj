(ns rt-in-weekend.texture
  (:require [clojure.algo.generic.math-functions :as math]
            [rt-in-weekend.vec :as vec]))

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
