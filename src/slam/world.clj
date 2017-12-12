(ns slam.world
  (:require [clojure.core.matrix :as mat :refer [mmul]]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]
            [quil.core :as q]
            [quil.middleware :as quil-middleware]

            [clojure.java.shell :as shell]
            [gnuplot.core :as g]

            [slam.functional :as F :refer [sqr]]
            [slam.linear :as linear]
            [slam.quat :as quat]
            [slam.dynamics :as dynamics]
            [slam.quilkit :as qk :refer [skip-index screen->rot draw-unit-frame draw-grid]]))


(defn helix-translate
  []
  (let [duration 5000
        half-duration (* 1/2 duration)
        t (q/millis)
        step (mod t duration)
        z (if (>= step half-duration)
            (q/map-range step half-duration (dec duration) (* Math/PI 2) 0)
            (q/map-range step 0 (dec half-duration) 0 (* Math/PI 2)))]
    (apply q/translate (F/helix z))))

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



(defn counter [state max name f]
  (let [value (get state name 0)]
    (when (< value max)
      (f)
      (assoc state name (inc value)))))

(defn draw [state]
  (q/background 200)
  (q/lights)
  (q/fill 150 100 150) ;; purple
  ;;(q/stroke 255 255 255)
  (q/no-fill)
  (q/no-stroke)
  (q/stroke 255 255 255)
  (q/stroke-weight 0.01)

  (q/reset-matrix)

  (let [height (q/height)
        width  (q/width)
        fovy   (/ Math/PI 3.0)
        aspect (/ width height)
        z      (/ (* height 0.5)
                 (mat/tan (* Math/PI 60.0 (/ 1 360.0))))
        z-near (* z 0.1)
        z-far  (* z 10)]
    ;; (q/perspective
    ;;  fovy aspect z-near z-far)

    (q/camera -300 -300 80 ;; eye-xyz
              0 0 0 ;; look-at
              0 0 -1 ;; where is up
              )
    (q/frustum -100 100 -100 100 125 10000))

  (q/begin-camera)

  ;;(q/rotate-x 0)

  ;; (q/rotate-x (* Math/PI 0.75))
  ;;(q/rotate-y (screen->rot (q/mouse-y) 0.2))
  (q/rotate-z (screen->rot (q/mouse-x)))
  ;; ;;(q/rotate-y (* Math/PI 0))
  ;; (q/rotate-z (* Math/PI 0.05))

  ;; (q/rotate-y (screen->rot (q/mouse-x)))
  ;; (q/rotate-z (screen->rot (q/mouse-y) 0.1))
  (q/end-camera)


  (q/scale 15)

  (doseq [drone-pos
          [[ 0  0 2]
           [ -2 10 -2]
           [ 5 5 1]
           [ -3 3 1]
           [ -5 5 1]
           [ -3 8 1]
           ]]
    (q/with-translation drone-pos
      ;;(helix-translate)
      (let [current-step (-> (q/millis) (mod 1000))
            step (nth dynamics/timesteps current-step)
            [x y z qs qx qy qz] (take 7 step)
            rotation (quat/quat->rot [qs qx qy qz])
            ]
        (apply q/apply-matrix (linear/mat3x3->4x4 rotation))
        (q/translate x y z)
        )
      ;; (q/rotate-y (q/map-range (-> (q/millis)
      ;;                              (mod 1000))
      ;;                          0
      ;;                          999
      ;;                          0
      ;;                          (* 2 Math/PI)))
      ;;(q/rotate-x (q/random 0.02 0.1))
      ;;(q/scale 50)
      ;;(q/scale 1)
      (draw-unit-frame)
      (draw-quadrotor))
    )


  (q/with-stroke [255 0 0]
    (q/with-translation [0 0 15]
      (q/no-fill)
      ;;(q/box 100)
      (q/box 30))
    ;;(q/box 50
    )

  (q/translate [-50 -50 0])
  (q/scale 1)
  (q/stroke-weight 0.02)

  (draw-grid 100)

  ;; (q/with-translation [0 0]

  ;;   (draw-unit-frame)
  ;;   (draw-quadrotor))

  ;; (q/with-translation [0 0 0.2]
  ;;   ;;(q/reset-matrix)
  ;;   (q/scale 10)
  ;;   (q/stroke-weight 0.01)
  ;;   (q/apply-matrix  1 0 0 0
  ;;                    0 1 0 0
  ;;                    0 0 1 0
  ;;                    0 0 0 1)
  ;;   (q/scale 1)
  ;;   (draw-unit-frame))
  ;; (draw-quadrotor)

  ;; (let [text-buf (q/create-graphics 200 200)
  ;;       comic-sans (q/create-font "ComicSansMS" 20 true)]
  ;;   (q/with-graphics text-buf
  ;;     (q/with-translation [0 0 0]
  ;;       (q/text-font comic-sans)
  ;;       (q/stroke 150 100 100)
  ;;       (q/fill 150 100 100)
  ;;       (q/text (format "ts %d" (-> (q/millis) (mod 1000))) 0 0 200 200)))
  ;;   (q/copy text-buf [0 0 200 200] [10 10 210 210]))


  (-> state
      ;;(counter 100 :snapshot #(q/save-frame "snapshot-#####.png"))
      identity))



(defn key-pressed [state event]
  (condp = (:key event)
    :q (q/exit)
    :p
    (do (println "camera")
        (q/print-camera)
        (println "projection")
        (q/print-projection)
        (println "matrix")
        (q/print-matrix)
        (println "mouse" (q/mouse-x) (q/mouse-y)))
    nil)
  state)



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

