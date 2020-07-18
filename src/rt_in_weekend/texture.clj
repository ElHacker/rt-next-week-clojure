(ns rt-in-weekend.texture
  (:require [clojure.algo.generic.math-functions :as math]
            [rt-in-weekend.vec :as vec]
            [rt-in-weekend.perlin :as perlin]
            [rt-in-weekend.util :as util]
            [mikera.image.core :as image]
            [mikera.image.colours :as colours]))

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
    (vec/* [1.0 1.0 1.0] (* 0.5 (+ 1.0 (math/sin (+ (* (vec/z point) scale)
                                                    (* 10 (perlin/turbulence point)))))))))

(def load-image-memoized (memoize image/load-image))

(defrecord ImageTexture [filename]
  Texture
  (value [this u v point]
    (let [image-data (load-image-memoized filename)
          width (image/width image-data)
          height (image/height image-data)
          ; Clamp input texture coordinates to [0, 1] x [1, 0]
          u (util/clamp u 0.0 1.0)
          ; Flip v to image coordinates.
          v (- 1.0 (util/clamp v 0.0 1.0))
          i (int (* u (- width 1)))
          j (int (* v (- height 1)))
          pixel (colours/values-rgb (image/get-pixel image-data i j))]
      pixel)))
