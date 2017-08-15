(ns slam.world
  (:require [clojure.core.matrix :as mat :refer [mmul]]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]
            [quil.core :as q]
            [quil.middleware :as quil-middleware]))

(def lxapply mat/mmul)

(defn rowextend
  "Add a row to matrix of column vectors, use 1.0 as fill-value by default.
   Use :last to set the rightmost element to fill-value and keep others at 0."
  ([matrix fill-value & args]
   (let [[_ ncols] (mat/shape matrix)
         zero (mat/reshape (mat/zero-vector ncols) [1 ncols])
         filled (if (some #{:last} args)
                  (mat/set-indices! zero [[0 (dec ncols)]] [fill-value])
                  (mat/fill! zero fill-value))]
     (mat/join-along 0 matrix filled)))
  ([matrix]
   (rowextend matrix 1.0)))

(defn hom-translation
  "Create a Nx(N+1) matrix that represents a translation.
   Use `base` as the base matrix when given, otherwise use a diagonal identity."
  ([base pos]
   (let [[n] (mat/shape pos)
         pos' (mat/reshape pos [n 1])
         nxn+1 (mat/join-along 1 base pos')]
     nxn+1))
  ([pos]
   (let [[n] (mat/shape pos)]
     (hom-translation (mat/diagonal-matrix (repeat n 1.0)) pos))))


(defn rotation-z
  "Rotation matrix about the Z axis. Also a 2d rotation about origin if you take the upper 2x2 part."
  [angle]
  (let [cos (mat/cos angle)
        sin (mat/sin angle)]
    (mat/matrix [[ cos (- sin) 0]
                 [ sin    cos  0]
                 [   0      0  1]])))

(defn rotation-y
  [angle]
  (let [cos (mat/cos angle)
        sin (mat/sin angle)]
    (mat/matrix [[    cos    0   sin]
                 [      0    1     0]
                 [ (- sin)   0   cos]])))

(defn rotation-x
  [angle]
  (let [cos (mat/cos angle)
        sin (mat/sin angle)]
    (mat/matrix [[  1     0      0]
                 [  0   cos (- sin)]
                 [  0   sin    cos]])))

(defn euler-zxy [yaw-psi-z
                 roll-phi-x
                 pitch-theta-y]
  (mat/mmul (rotation-y pitch-theta-y) (rotation-x roll-phi-x) (rotation-z yaw-psi-z)))

(defn translation>>rotation
  "Produce a [R|t] matrix. Yaw is a rotation about z axis, roll is about x, pitch is about y."
  [yaw roll pitch xyz]
  ;; 3x4 4x3
  (hom-translation (euler-zxy yaw roll pitch) xyz))


(defn deg->rad [deg] (* deg Math/PI (/ 1.0 180.0)))
(defn rad->deg [rad] (* rad 180 (/ 1 Math/PI)))

(comment
  (def position [50 50 0])
  (def orientation {:yaw 0
                    :pitch 0
                    :roll 0})

  ;; draw orientation of a drone with a [1 0 0] vector in drone's reference frame

  (def arrow (mat/transpose [[0 0 0] [1 0 0]])) ;; 3x2 matrix: two column 3d vectors
  (mat/mmul (translation>>rotation (deg->rad 45) 0 0 position) (rowextend arrow))
  :ok)


(defn screen->rot
  ([x coef]
   (q/map-range x 0 500 (* Math/PI 0 coef) (* Math/PI 2 coef)))
  ([x]
   (screen->rot x 1)))

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


(defn draw [state]
  (q/background 180)
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

    ;; camera
    ;;  000.7071 -000.7071  000.0000  000.0000
    ;;  000.4082  000.4082 -000.8165  000.0000
    ;;  000.5774  000.5774  000.5774 -346.4102
    ;;  000.0000  000.0000  000.0000  001.0000
    ;; projection
    ;;  002.0000  000.0000  000.0000  000.0000
    ;;  000.0000 -002.0000  000.0000  000.0000
    ;;  000.0000  000.0000 -002.3333 -666.6667
    ;;  000.0000  000.0000 -001.0000  000.0000

    (q/camera 100 100 200 0 0 0 0 0 -1)
    (q/frustum -100 100 -100 100 100 10000)

    ;;(q/frustum -100 100 -100 100 -100 100)
    ;;(q/ortho 0 width 0 height -10 10)
    )

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


  ;; (q/begin-camera)
  ;; (q/reset-matrix)
  ;; (q/apply-matrix  1 0 0 0
  ;;                  0 1 0 0
  ;;                  0 0 1 -1000
  ;;                  0 0 0 1)
  ;; ;; (q/rotate-x -0.1)
  ;; ;; (q/rotate-y Math/PI)
  ;; ;; (q/rotate-z Math/PI)

  ;; (q/rotate-x (* Math/PI 0.75))
  ;; ;; (q/rotate-y (screen->rot (q/mouse-x)))
  ;; ;; ;;(q/rotate-y (* Math/PI 0))
  ;; ;; (q/rotate-z (* Math/PI 0.05))

  ;; ;; (q/rotate-x -0.1)
  ;; ;; (q/rotate-y (screen->rot (q/mouse-x)))
  ;; ;; (q/rotate-z (screen->rot (q/mouse-y) 0.1))
  ;; (q/end-camera)

  ;; (q/reset-matrix)


  (q/scale 50)

  (doseq [drone-pos
          [[ 0  0 0]
           [ 5 5 1]
           [ -3 3 1]]]
    (q/with-translation drone-pos
      (q/rotate-x (q/random 0.02 0.08))
      ;;(q/scale 50)
      ;;(q/scale 1)
      ;;(draw-unit-frame)
      (draw-quadrotor)
      ))

  (q/translate [-50 -50 0])
  (q/scale 1)
  (q/stroke-weight 0.01)

  (q/with-stroke [255 0 0]
    ;;(q/box 100)
    ;;(q/box 30)
    ;;(q/box 50)
    )

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

  state)

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


(defn draw-settings [state]
  (q/smooth)
  state)

(q/defsketch my-sketch
  :draw draw
  :settings draw-settings
  :key-pressed key-pressed
  :size [800 600]
  :renderer :p3d
  :navigation-3d {:step-size 100}
  :middleware [quil-middleware/fun-mode
               ;;quil-middleware/navigation-3d
               ])
