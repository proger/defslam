(ns slam.functional
  (:require [clojure.core.matrix :as mat]))

(defn sqr [x] (* x x))

(defn sigmoid
  [x]
  (if (neg? x)
    (let [z (Math/exp x)] (/ z (inc z)))
    (/ 1 (inc (Math/exp (- x))))))

(defn helix [t]
  (let [z (* t Math/PI 1/4)]
    [(mat/cos z) (mat/sin z) z]))
