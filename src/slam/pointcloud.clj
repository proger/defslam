(ns slam.pointcloud
  (:require [clojure.core.matrix :as mat :refer [mmul]]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]
            [quil.core :as q]
            [quil.middleware :as quil-middleware]

            [slam.functional :as F :refer [sqr]]
            [slam.linear :as linear]
            [slam.quilkit :as qk]))

(defmacro uncomment [body]
  body)

(defn draw-quadrotor []
  (q/with-stroke [200 0 200]

    (q/with-translation [0 0]
      (apply q/apply-matrix [0.2 0 0 0
                             0 0.2 0 0
                             0 0 0.1 0
                             0 0 0 1])
      (q/with-fill [255 0 0]
        (q/box 1)))
    (doseq [pos [[ 1  1 0]
                 [-1  1 0]
                 [ 1 -1 0]
                 [-1 -1 0]]]
      (q/begin-shape :lines)
      (apply q/vertex (map #(* 0.1 %) pos))
      (apply q/vertex (map #(* 0.8 %) pos))
      (q/end-shape)
      (q/with-translation (map #(* 0.8 %) pos)
        (apply q/apply-matrix [0.2 0 0 0
                               0 0.2 0 0
                               0 0 0.08 0
                               0 0 0 1])
        (q/sphere 1)))))


(defn draw-table []
  (q/with-translation [0 0 1]
    (apply q/apply-matrix [2 0 0 0
                           0 2 0 0
                           0 0 0.5 0
                           0 0 0 1])
    (q/box 1))
  (q/with-translation [0 0 0]
    (apply q/apply-matrix [0.4 0 0 0
                           0 0.4 0 0
                           0 0 1 0
                           0 0 0 1])
    (q/box 1)))

(defn draw [state]
  (q/background 200)
  (q/lights)
  (q/fill 150 100 150) ;; purple
  ;;(q/stroke 255 255 255)
  (q/no-fill)
  (q/no-stroke)
  (q/stroke 255 255 255)
  (q/stroke-weight 0.01)

  (when (not (true? (:lock state)))

    (q/reset-matrix)

    (let [height (q/height)
          width  (q/width)
          fovy   (/ Math/PI 3.0)
          aspect (/ width height)
          z      (/ (* height 0.5)
                   (mat/tan (* Math/PI 60.0 (/ 1 360.0))))
          z-near (* z 0.1)
          z-far  (* z 10)

          z-off (q/map-range (q/mouse-y) 0 (q/height) -100 100)]
      ;; (q/perspective
      ;;  fovy aspect z-near z-far)

      (q/camera -300 -300 150 ;; eye-xyz
                0 0 z-off ;; look-at
                0 0 -1 ;; where is up
                )
      (q/frustum -100 100 -100 100 125 10000))

    (q/begin-camera)
    (q/rotate-z (qk/screen->rot (q/mouse-x)))
    ;;(q/rotate-x (qk/screen->rot (q/mouse-y) 0.09))
    (q/end-camera))

  (q/scale 30)
  (q/translate [0 -5 0])

  (let [x 0]
    (doseq [object-pos
            [[ x  0 1]
             [ x  2 1]
             [ x  2 3]
             [ x  4 1]
             [ x  4 3]
             [ x  4 5]
             ]]
      (q/with-translation object-pos
        (qk/draw-unit-frame)
        (q/fill 150 100 150 100)
        (q/box 2))))


  (let [x 8
        y-off 2]
    (doseq [object-pos
            [[ (+ y-off 4) x 1]
             [ (+ y-off 2) x 1]
             [ (+ y-off 2) x 3]
             [ (+ y-off 0) x 1]
             [ (+ y-off 0) x 3]
             [ (+ y-off 0) x 5]
             ]]
      (q/with-translation object-pos
        (qk/draw-unit-frame)
        (q/fill 150 100 150 100)
        (q/box 2))))

  (q/stroke 255 255 255 0.8)
  (q/fill 0 200 10 100)

  (q/with-translation [ 4 4 0]
    (draw-table))

  (q/with-translation [ 4 2 0]
    (draw-table))

  (q/with-stroke [255 0 0 30]
    (q/with-translation [0 0 15]
      (q/no-fill)
      ;;(q/box 100)
      (q/box 30))
    ;;(q/box 50
    )

  ;; (q/translate [-50 -50 0])
  ;; (q/scale 1)
  ;; (q/stroke-weight 0.02)

  ;; (qk/draw-grid 100)
  (-> state
      ;;(counter 100 :snapshot #(q/save-frame "snapshot-#####.png"))
      identity))

(defn key-pressed [state event]
  (condp = (:key event)
    :q (do (q/exit)
           state)
    :l (let [new-state (update state :lock not)]
         (println "lock:" (:lock new-state))
         new-state)
    :p
    (do (println "camera")
        (q/print-camera)
        (println "projection")
        (q/print-projection)
        (println "matrix")
        (q/print-matrix)
        (println "mouse" (q/mouse-x) (q/mouse-y))
        state)
    state))



(q/defsketch my-sketch
  :draw draw
  :setup #(q/smooth 8)
  :key-pressed key-pressed
  :size [800 600]
  :renderer :p3d
  :features [:resizable]
  :navigation-3d {:step-size 100}
  :middleware [quil-middleware/fun-mode
               ;;quil-middleware/navigation-3d
               ])
