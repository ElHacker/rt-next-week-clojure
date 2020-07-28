(ns rt-in-weekend.core
  (:require [rt-in-weekend.image :as img]
            [rt-in-weekend.vec :as vec]
            [rt-in-weekend.ray :as ray]
            [rt-in-weekend.hittable :as hittable]
            [rt-in-weekend.camera :as camera]
            [rt-in-weekend.material :as material]
            [rt-in-weekend.texture :as texture]
            [rt-in-weekend.util :as util]))

(defn ppm-header [width height]
  (str "P3\n" width " " height "\n255\n"))

(defn pixel-line [r g b]
  (str r " " g " " b "\n"))

(defn raytrace [width height pixels path]
  (let [header (ppm-header width height)
        body (clojure.string/join pixels)
        ppm (str header body)]
    (img/save-ppm ppm path)))

(defn ray-color [r background world depth]
  (if-let [rec (hittable/hittable-list world r 0.0001 Float/MAX_VALUE)]
    (let [result (material/scatter (:material rec) r rec)
          emitted (material/emitted (:material rec) (:u rec) (:v rec) (:p rec))]
      (if (and (> depth 0) (:ok result))
        (vec/* (vec/+ emitted (:attenuation result))
               (ray-color (:scattered result) background world (dec depth)))
        (if (<= depth 0)
          [0 0 0]
          emitted)))
    background))

(defn evolve-color [world cam background image-width image-height num-samples i j depth]
  (let [color (atom [0 0 0])]
    (doseq [_ (range num-samples)]
      (let [u (/ (+ i (rand)) (float image-width))
            v (/ (+ j (rand)) (float image-height))
            r (camera/get-ray cam u v)]
        (swap! color vec/+ (ray-color r background world depth))))
    (vec// @color (float num-samples))))

(defn make-world []
  (let [checker-texture (texture/->CheckerTexture
                          (texture/->SolidColor [0.2 0.3 0.1])
                          (texture/->SolidColor [0.9 0.9 0.9]))
        world (atom [(hittable/->Sphere [0 -1000 0] 1000 (material/->Lambertian checker-texture))])
        drand #(* (rand) (rand))]
    (doseq [a (range -11 11)
            b (range -11 11)
            :let [choose-mat (rand)
                  center [(+ a (* 0.9 (rand))) 0.2 (+ b (* 0.9 (rand)))]]]
      (if (> (vec/length (vec/- center [4 0.2 0])) 0.9)
        (cond
          (< choose-mat 0.8) ; diffuse
          (swap! world conj (hittable/->MovingSphere
                              center
                              (vec/+ center [0 (util/rand-in-range 0 0.5) 0])
                              0.0 ; t0
                              1.0 ; t1
                              0.2 ; radius
                              (material/->Lambertian (texture/->SolidColor [(drand) (drand) (drand)]))))
          (< choose-mat 0.95) ;metal
          (swap! world conj (hittable/->Sphere center 0.2 (material/->Metal [(* 0.5 (inc (rand)))
                                                                               (* 0.5 (inc (rand)))
                                                                               (* 0.5 (rand))]
                                                                               (rand))))
          :else ;glass
          (swap! world conj (hittable/->Sphere center 0.2 (material/->Dialectric 1.5)))
          )))
    (swap! world conj (hittable/->Sphere [0 1 0] 1.0 (material/->Dialectric 1.5)))
    (swap! world conj (hittable/->Sphere [-4 1 0] 1.0 (material/->Lambertian (texture/->SolidColor [0.4 0.2 0.1]))))
    (swap! world conj (hittable/->Sphere [4 1 0] 1.0 (material/->Metal [0.7 0.6 0.5] 0.0)))
    @world))

(defn final-scene []
  (let [aspect-ratio (/ 16.0 9.0)
        image-width 400
        image-height (int (/ image-width aspect-ratio))
        num-samples 30
        max-depth 50
        lookfrom [13 2 3]
        lookat [0 0 0]
        vup [0 1 0]
        dist-to-focus 10.0
        aperture 0.0
        world (make-world)
        cam (camera/make lookfrom lookat vup 20 aspect-ratio aperture dist-to-focus 0.0 1.0)
        background [0 0 0]]
    (raytrace image-width image-height
              (for [j (range (dec image-height) -1 -1)
                    i (range 0 image-width)
                    :let [color (evolve-color world cam background image-width image-height num-samples i j max-depth)
                          corrected-color (map #(Math/sqrt %) color)
                          ir (int (* 255.999 (vec/x corrected-color)))
                          ig (int (* 255.999 (vec/y corrected-color)))
                          ib (int (* 255.999 (vec/z corrected-color)))]]
                (pixel-line ir ig ib))
              "./images/checkered")))

(defn make-checkered-world []
  (let [checker-texture (texture/->CheckerTexture
                          (texture/->SolidColor [0.2 0.3 0.1])
                          (texture/->SolidColor [0.9 0.9 0.9]))
        world (atom [(hittable/->Sphere [0 -10 0] 10 (material/->Lambertian checker-texture))
                     (hittable/->Sphere [0 10 0] 10 (material/->Lambertian checker-texture))])]
  @world))


(defn two-spheres-scene []
  (let [aspect-ratio (/ 16.0 9.0)
        image-width 400
        image-height (int (/ image-width aspect-ratio))
        num-samples 30
        max-depth 50
        lookfrom [13 2 3]
        lookat [0 0 0]
        vup [0 1 0]
        dist-to-focus 10.0
        aperture 0.0
        world (make-checkered-world)
        cam (camera/make lookfrom lookat vup 20 aspect-ratio aperture dist-to-focus 0.0 1.0)
        background [0 0 0]]
    (raytrace image-width image-height
              (for [j (range (dec image-height) -1 -1)
                    i (range 0 image-width)
                    :let [color (evolve-color world cam background image-width image-height num-samples i j max-depth)
                          corrected-color (map #(Math/sqrt %) color)
                          ir (int (* 255.999 (vec/x corrected-color)))
                          ig (int (* 255.999 (vec/y corrected-color)))
                          ib (int (* 255.999 (vec/z corrected-color)))]]
                (pixel-line ir ig ib))
              "./images/checkered-spheres")))

(defn make-perlin-world []
  (let [perlin-texture (texture/->NoiseTexture 10)
        difflight (material/->DiffuseLight (texture/->SolidColor [4 4 4]))
        world (atom [(hittable/->Sphere [0 -1000 0] 1000 (material/->Lambertian perlin-texture))
                     (hittable/->Sphere [0 2 0] 2 (material/->Lambertian perlin-texture))])]
    @world))

(defn two-perlin-spheres-scene []
  (let [aspect-ratio (/ 16.0 9.0)
        image-width 400
        image-height (int (/ image-width aspect-ratio))
        num-samples 30
        max-depth 50
        lookfrom [13 2 3]
        lookat [0 0 0]
        vup [0 1 0]
        dist-to-focus 10.0
        aperture 0.0
        world (make-perlin-world)
        cam (camera/make lookfrom lookat vup 20 aspect-ratio aperture dist-to-focus 0.0 1.0)
        background [0 0 0]]
    (raytrace image-width image-height
              (for [j (range (dec image-height) -1 -1)
                    i (range 0 image-width)
                    :let [color (evolve-color world cam background image-width image-height num-samples i j max-depth)
                          corrected-color (map #(Math/sqrt %) color)
                          ir (int (* 255.999 (vec/x corrected-color)))
                          ig (int (* 255.999 (vec/y corrected-color)))
                          ib (int (* 255.999 (vec/z corrected-color)))]]
                (pixel-line ir ig ib))
              "./images/perlin-marble-smooth-spheres")))

(defn make-earth-world []
  (let [earth-texture (texture/->ImageTexture "./images/earthmap.jpg")
        earth-surface (material/->Lambertian earth-texture)
        world (atom [(hittable/->Sphere [0 0 0] 2 earth-surface)])]
    @world))

(defn earth-scene []
  (let [aspect-ratio (/ 16.0 9.0)
        image-width 400
        image-height (int (/ image-width aspect-ratio))
        num-samples 30
        max-depth 50
        lookfrom [13 2 3]
        lookat [0 0 0]
        vup [0 1 0]
        dist-to-focus 10.0
        aperture 0.0
        world (make-earth-world)
        cam (camera/make lookfrom lookat vup 20 aspect-ratio aperture dist-to-focus 0.0 1.0)
        background [0 0 0]]
    (raytrace image-width image-height
              (for [j (range (dec image-height) -1 -1)
                    i (range 0 image-width)
                    :let [color (evolve-color world cam background image-width image-height num-samples i j max-depth)
                          corrected-color (map #(Math/sqrt %) color)
                          ir (int (* 255.999 (vec/x corrected-color)))
                          ig (int (* 255.999 (vec/y corrected-color)))
                          ib (int (* 255.999 (vec/z corrected-color)))]]
                (pixel-line ir ig ib))
              "./images/earth-sphere")))

(defn make-light-world []
  (let [perlin-texture (texture/->NoiseTexture 10)
        difflight (material/->DiffuseLight (texture/->SolidColor [4 4 4]))
        world (atom [(hittable/->Sphere [0 -1000 0] 1000 (material/->Lambertian perlin-texture))
                     (hittable/->Sphere [0 2 0] 2 (material/->Lambertian perlin-texture))
                     (hittable/->Sphere [0 7 0] 2 difflight)
                     (hittable/->XYRect 3 5 1 3 -2 difflight)])]
    @world))

(defn light-scene []
  (let [aspect-ratio (/ 16.0 9.0)
        image-width 400
        image-height (int (/ image-width aspect-ratio))
        num-samples 100
        max-depth 50
        lookfrom [26 3 6]
        lookat [0 2 0]
        vup [0 1 0]
        dist-to-focus 10.0
        aperture 0.0
        world (make-light-world)
        cam (camera/make lookfrom lookat vup 20 aspect-ratio aperture dist-to-focus 0.0 1.0)
        background [0 0 0]]
    (raytrace image-width image-height
              (for [j (range (dec image-height) -1 -1)
                    i (range 0 image-width)
                    :let [color (evolve-color world cam background image-width image-height num-samples i j max-depth)
                          corrected-color (map #(Math/sqrt %) color)
                          ir (int (* 255.999 (vec/x corrected-color)))
                          ig (int (* 255.999 (vec/y corrected-color)))
                          ib (int (* 255.999 (vec/z corrected-color)))]]
                (pixel-line ir ig ib))
              "./images/rectangle-sphere-light-source")))

(defn make-cornell-world []
  (let [red (material/->Lambertian (texture/->SolidColor [0.65 0.05 0.05]))
        white (material/->Lambertian (texture/->SolidColor [0.73 0.73 0.73]))
        green (material/->Lambertian (texture/->SolidColor [0.12 0.45 0.15]))
        light (material/->DiffuseLight (texture/->SolidColor [7 7 7]))
        box1 (hittable/->Box [0 0 0] [165 330 165] white)
        box2 (hittable/->Box [0 0 0] [165 165 165] white)
        cmbox1 (hittable/->ConstantMedium
                 (hittable/->Translate (hittable/->RotateY box1 15) [265 0 295])
                 0.01
                 (texture/->SolidColor [0 0 0]))
        cmbox2 (hittable/->ConstantMedium
                 (hittable/->Translate (hittable/->RotateY box2 -18) [130 0 65])
                 0.01
                 (texture/->SolidColor [1 1 1]))
        boxes [cmbox1 cmbox2]
        world (atom [(hittable/->YZRect 0 555 0 555 555 green)
                     (hittable/->YZRect 0 555 0 555 0 red)
                     (hittable/->XZRect 113 443 127 432 554 light)
                     (hittable/->XZRect 0 555 0 555 0 white)
                     (hittable/->XZRect 0 555 0 555 555 white)
                     (hittable/->XYRect 0 555 0 555 555 white)
                     (hittable/bvh-node-split-build boxes 0 2 0 0)])]
    @world))

(defn cornell-box-scene []
  (let [aspect-ratio (/ 16.0 9.0)
        image-width 600
        image-height (int (/ image-width aspect-ratio))
        num-samples 30
        max-depth 20
        lookfrom [278 278 -800]
        lookat [278 278 0]
        vup [0 1 0]
        dist-to-focus 10.0
        aperture 0.0
        world (make-cornell-world)
        cam (camera/make lookfrom lookat vup 40 aspect-ratio aperture dist-to-focus 0.0 1.0)
        background [0 0 0]]
    (raytrace image-width image-height
              (for [j (range (dec image-height) -1 -1)
                    i (range 0 image-width)
                    :let [color (evolve-color world cam background image-width image-height num-samples i j max-depth)
                          corrected-color (map #(Math/sqrt %) color)
                          ir (int (* 255.999 (vec/x corrected-color)))
                          ig (int (* 255.999 (vec/y corrected-color)))
                          ib (int (* 255.999 (vec/z corrected-color)))]]
                (pixel-line ir ig ib))
              "./images/cornell-smoke-bvh")))

(defn create-ppm []
  (let [image-width 256,
        image-height 256,
        header (ppm-header image-width image-height)
        pixels (for [j (range (dec image-height) -1 -1)
                     i (range 0 image-width)
                     :let [r (int (* 255.999 (/ i (dec image-width))))
                           g (int (* 255.999 (/ j (dec image-height))))
                           b (int (* 255.999 0.25))]]
                 (pixel-line r g b))
        body (clojure.string/join pixels)
        ppm (str header body)]
    (img/save-ppm ppm "./images/image")))

(time (cornell-box-scene))
