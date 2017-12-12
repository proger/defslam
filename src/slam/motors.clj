(ns slam.motors
  (:require [slam.linear :refer [sqr]]))

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

(defn power->torque [] :todo)
(defn torque->thrust [] :todo)
