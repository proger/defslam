(ns slam.quilkit
  (:require [quil.core :as q]
            [quil.middleware :as quil-middleware]))

(defn skip-index
  [millis duration xs]
  (let [len (count xs)
        step (mod millis duration)
        n (-> (q/map-range step 0 duration 0 len)
              Math/round)]
    (nth xs n)))

(defn screen->rot
  ([x coef]
   (q/map-range x 0 500 (* Math/PI 0 coef) (* Math/PI 2 coef)))
  ([x]
   (screen->rot x 1)))

(defn draw-unit-frame []
  ;; (q/no-fill)
  ;; (q/box 2)
  (q/stroke-weight 0.05)

  (q/begin-shape :lines)
  ;; red: x
  (q/with-stroke [255 0 0]
    (q/vertex 0 0 0)
    (q/vertex 1 0 0))

  ;; green: y
  (q/with-stroke [0 255 0]
    (q/vertex 0 0 0)
    (q/vertex 0 1 0))

  ;; blue: z
  (q/with-stroke [0 0 255]
    (q/vertex 0 0 0)
    (q/vertex 0 0 1))
  (q/end-shape))

(defn draw-grid
  ([] (draw-grid 10))
  ([limit]
   (let [verticals
         (for [x (range (inc limit))
               y [limit]
               z [0]]
           [[x 0 z] [x y z]])

         horizontals
         (for [x [limit]
               y (range (inc limit))
               z [0]]
           [[0 y z] [x y z]])

         all (concat verticals horizontals)]

     (q/with-stroke [255 255 255]
       (doseq [[start end] all]
         (q/begin-shape :lines)
         (apply q/vertex start)
         (apply q/vertex end)
         (q/end-shape))))))
