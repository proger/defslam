(ns slam.world
  (:require [clojure.core.matrix :as mat :refer [mmul]]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]
            [quil.core :as q]
            [quil.middleware :as quil-middleware]

            [clojure.java.shell :as shell]
            [gnuplot.core :as g]

            [slam.quat :as quat]))

(def pi Math/PI)
(defn sqr [x] (* x x))
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
  "Rotation matrix about the Z axis. Also a 2d rotation about origin if you take the upper left 2x2 part."
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

(defn sigmoid
  [x]
  (if (neg? x)
    (let [z (Math/exp x)] (/ z (inc z)))
    (/ 1 (inc (Math/exp (- x))))))

(defn helix [t]
  (let [z (* t Math/PI 1/4)]
    [(mat/cos z) (mat/sin z) z]))

(defn helix-translate
  []
  (let [duration 5000
        half-duration (* 1/2 duration)
        t (q/millis)
        step (mod t duration)
        z (if (>= step half-duration)
            (q/map-range step half-duration (dec duration) (* Math/PI 2) 0)
            (q/map-range step 0 (dec half-duration) 0 (* Math/PI 2)))]
    (apply q/translate (helix z))))

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

    (q/camera -300 -300 150 ;; eye-xyz
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


  (q/scale 30)

  (doseq [drone-pos
          [[ 0  0 2]
           ;;[ -2 10 -2]
           ;;[ 5 5 1]
           ;;[ -3 3 1]
           ]]
    (q/with-translation drone-pos
      (helix-translate)
      (q/rotate-y (q/map-range (-> (q/millis)
                                   (mod 1000))
                               0
                               999
                               0
                               (* 2 pi)))
      (q/rotate-x (q/random 0.02 0.1))
      ;;(q/scale 50)
      ;;(q/scale 1)
      (draw-unit-frame)
      (draw-quadrotor)
      ))


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


;; (my-sketch)

;; dynamics:
;; y'' = -g + motor-input
;; second-order linear system. want to produce motor-input to control it
;; why PID? we want the error to converge exponentially to zero
;;


;; ω  (angular velocity - radians of a point per second)
;; rpm - revolutions of a point per minute

(def scalar* *)

(defn rpm->ω
  "revolutions of a point per minute to radians per second (angular velocity)"
  [rpm]
  (scalar* 2 pi 1/60 rpm))


(defn force-
  "The angular velocity of rotor, denoted with ω, creates force in the direction of the rotor axis."
  [lift ω]
  (scalar* lift (sqr ω)))

;; [yaw-psi-z
;;  roll-phi-x
;;  pitch-theta-y]

(defn torque
  "The angular velocity and acceleration of the rotor also create torque τ around the rotor axis."
  [drag ω inertia ω']
  (+ (scalar* drag (sqr ω)) (scalar* inertia ω')))


;; torque orientation
;;  ^ x (front)
;; ω₄ CCW  ω₁ CW
;;                  -> y (right)
;; ω₃  CW  ω₂ CCW

;; hover: rotor speeds have to compensate the weight
;; we have a force we have to produce
;; to produce force motors can generate torque

(defn equilibrium [ω]
  (let [lift :constant
        mass :constant
        g 9.81]
    (= (force- lift ω) (* 1/4 mass g))))

;; thrust is only up the z axis
;;

(defn power->torque [] :todo)
(defn torque->thrust [] :todo)
(defn torque->thrust [] :todo)
(defn torque->acceleration [] :todo)


(defn rigid-state []
  {:position [0 0 0]
   :orientation (quat/rot->quat (euler-zxy 1 0 0))
   :linear-momentum [0 0 0] ;; P; P-dot is F - force
   :angular-momentum [0 0 0] ;; L; L-dot is τ - torque
   })

;; (rigid-state)

(defn skew-hat [[x y z]]
  (mat/matrix [[    0  (- z)    y]
               [    z     0 (- x)]
               [(- y)     x     0]]))

;; 2.10 Inertia Tensor
;; 2 Mathematical model of quadcopter
;; The quadcopter is assumed to have symmetric structure with the
;; four arms aligned with the body x- and y-axes.
;; Thus, the inertia matrix is diagonal matrix I in which Ixx = Iyy
(defn angular:momentum->velocity [inertia-matrix-inv angular-momentum]
  (mat/mmul inertia-matrix-inv angular-momentum))


;; y-dot = y
;; y(0) = 1

;; y' = f(t,y)
;; want to approximate y at t = (+ x0 step-size)
(defn euler-step [f x & {:keys [step-size]
                         :or {step-size 1}}]
  (mat/add x (mat/mul step-size (f x))))

(defn euler [f x & {:keys [step-size t₀ t]
                    :or {step-size 0.0001
                         t₀ 0
                         t  100}}]
  (let [t-span (- t t₀)
        iters  (/ t-span step-size)]
    (->> x
         (iterate #(euler-step f % :step-size step-size))
         (take iters))))

(defn control [z-target [y z φ Dy Dz Dφ]]
  (let [g 9.81
        m 1]
    (min 12
         (+ (* m g)
            (* 10 (- z-target z))
            (* -3.7 Dz)))
    ))

(->> (let [   ;; inputs
           u₁  0 ;; thrust force
           u₂  0 ;; moment
           ;; constants
           Iₓₓ 1 ;; inertia component
           m   1 ;; mass
           g   9.81

           init-state [0 0 0 0 0 0]
           step-fn (fn planar
                     [y z φ Dy Dz Dφ]
                     (let [u₁ (control 10 [y z φ Dy Dz Dφ])]
                       [Dy
                        Dz
                        Dφ
                        (/ (* (- (mat/sin φ)) u₁) m)
                        (- (/ (* (mat/cos φ)  u₁) m) g)
                        (/ u₂ Iₓₓ)]))]
       (euler #(apply step-fn %) init-state :t 10 :step-size 0.01))
     plot-states
     preview-flip-flop)

;; now: take these states and build a 3d thing

(defn preview-flip-flop [& args]
  (shell/sh "osascript" "-e" "tell application \"Preview.app\" to activate")
  (shell/sh "osascript" "-e" "tell application \"Emacs.app\" to activate")
  args)


(defn plot-states
  [states]
  (g/raw-plot! [[:set :output "gnuplot.png"]
                [:set :term :png :truecolor :size (g/list 600 400)]
                '[set autoscale]
                ;;'[set key outside top right]
                [:plot "-" :title "location" :with :points]]
               [(->> states
                     (map (fn [[x y _ _ _ _]] [x y])))]))


(let
    [t 4]
    (-
     (mat/exp t)
     (last
      (euler identity 1
             :t t
             :step-size 0.00001))))
