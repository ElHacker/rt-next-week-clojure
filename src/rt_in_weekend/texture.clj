(ns rt-in-weekend.texture)

(defprotocol Texture
  (value [this u v point]))

(defrecord SolidColor [red green blue]
  Texture
  (value [this u v point]
    [red green blue]))
