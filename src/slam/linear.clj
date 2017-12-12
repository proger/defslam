(ns slam.linear
  (:require [clojure.core.matrix :as mat :refer [mmul]]))

(def pi Math/PI)
(defn sqr [x] (* x x))
(def lxapply mat/mmul)

(defn deg->rad [deg] (* deg Math/PI (/ 1.0 180.0)))
(defn rad->deg [rad] (* rad 180 (/ 1 Math/PI)))

(defn skew-hat [[x y z]]
  (mat/matrix [[    0  (- z)    y]
               [    z     0 (- x)]
               [(- y)     x     0]]))

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

(comment
  (def position [50 50 0])
  (def orientation {:yaw 0
                    :pitch 0
                    :roll 0})

  ;; draw orientation of a drone with a [1 0 0] vector in drone's reference frame

  (def arrow (mat/transpose [[0 0 0] [1 0 0]])) ;; 3x2 matrix: two column 3d vectors
  (mat/mmul (translation>>rotation (deg->rad 45) 0 0 position) (rowextend arrow))
  :ok)


(defn mat3x3->4x4 [[[a b c]
                   [d e f]
                   [g h i]]]
  [a b c 0 d e f 0 g h i 0 0 0 0 1])
