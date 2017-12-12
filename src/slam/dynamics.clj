(ns slam.dynamics
  (:require [clojure.core.matrix :as mat :refer [mmul]]

            [clojure.java.shell :as shell]
            [gnuplot.core :as g]

            [slam.functional :as F]
            [slam.linear :as linear]
            [slam.quat :as quat]

            [taoensso.tufte :as tufte]))

(tufte/add-basic-println-handler! {})


;; 2.10 Inertia Tensor
;; 2 Mathematical model of quadcopter
;; The quadcopter is assumed to have symmetric structure with the
;; four arms aligned with the body x- and y-axes.
;; Thus, the inertia matrix is diagonal matrix I in which Ixx = Iyy
(defn angular:momentum->velocity [inertia-matrix-inv angular-momentum]
  (mat/mmul inertia-matrix-inv angular-momentum))


(defn euler-step
  "Numerical integration using Euler's first-order method.
   Performs an infinitesimal step to solve an initial value problem
   ẋ = f(x) through linearization."
  [f x & {:keys [step-size]
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

(let
    [t 4]
    (-
     (mat/exp t)
     (last
      (euler identity 1
             :t t
             :step-size 0.00001))))


(defn control [z-target [y z φ Dy Dz Dφ]]
  (let [g 9.81
        m 1]
    (min 12
         (+ (* m g)
            (* 10 (- z-target z))
            (* -3.7 Dz)))
    ))

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

(comment (->> (let [   ;; inputs
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
              preview-flip-flop))


;; now: take these states and build a 3d thing

;; quad 3d:
;; state: x   y  z
;;        Dx Dy Dz
;;        rotation (quads or matrix)
;;        angular velocity (ω = [p q r])

;; control:
;; motor thrusts, motor moments


(defn init-state []
  (let
      [[x y z] [0 0 0]
       [qs qx qy qz] (quat/rot->quat (linear/euler-zxy 0 0 0))
       [Dx Dy Dz] [0 0 0]
       [p q r] [0 0 0]]
    [x y z qs qx qy qz Dx Dy Dz p q r]))

(def drone-params
  {:mass 1
   :gravity 9.81
   :inertia (mat/identity-matrix 3)
   :inertia-inv (mat/inverse (mat/identity-matrix 3))})

(defn quad3 [state params control]
  (let [[x y z qs qx qy qz Dx Dy Dz p q r] state
        {mass :mass
         inertia :inertia
         inertia-inv :inertia-inv
         gravity :gravity} params
        {total-thrust :thrust
         moments      :moments} control

        quat [qs qx qy qz]
        omega [p q r]
        body-to-world (quat/quat->rot quat) ;; ?
        world-to-body (mat/transpose body-to-world)

        [[DDx DDy DDz]] (mat/columns
                         (mat/mul (/ 1 mass) (mat/sub
                                              (mat/mmul
                                               world-to-body
                                               (mat/column-matrix [0 0 total-thrust]))
                                              (mat/column-matrix [0 0 (* mass gravity)]))))

        K-quat 2
        quat-error (- 1 (apply + (map F/sqr quat)))
        q-dot (mat/add (mat/mmul -1/2
                                 (mat/matrix [[0 (- p) (- q) (- r)]
                                              [p    0  (- r)    q ]
                                              [q    r     0  (- p)]
                                              [r (- q)    p     0 ]])
                                 quat)
                       (mat/mmul K-quat quat-error quat))

        [Dqs Dqx Dqy Dqz] q-dot

        omegadot (mat/mmul inertia-inv
                           (mat/sub
                            moments
                            (mat/mmul (linear/skew-hat omega)
                                      inertia
                                      omega)))
        [Dp Dq Dr] omegadot]
    [Dx
     Dy
     Dz
     Dqs
     Dqx
     Dqy
     Dqz
     DDx
     DDy
     DDz
     Dp
     Dq
     Dr]))

(def timesteps (euler #(tufte/p :quad3 (quad3 %
                                              drone-params
                                              {:thrust 10
                                               :moments [0.1 0.1 0.1]}))
                      (init-state)
                      :t 10
                      :step-size 0.01))

;; (comment (quad3 (init-state) {:thrust 40
;;                               :moments [1 1 1]})

;;          (tufte/profile {} (last (euler #(tufte/p :quad3 (quad3 %
;;                                                                 drone-params
;;                                                                 {:thrust 0
;;                                                                  :moments [0 0 0]}))
;;                                         (init-state)
;;                                         :t 10
;;                                         :step-size 0.01))))
