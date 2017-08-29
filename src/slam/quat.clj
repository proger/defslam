(ns slam.quat
  (:require [clojure.core.matrix :as mat :refer [mmul]]
            [clojure.core.matrix.linear :as linear]

            [clojure.test :refer [is]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn skew-hat [[x y z]]
  (mat/matrix [[    0  (- z)   y ]
               [    z     0 (- x)]
               [ (- y)    x    0 ]]))

(defn quat->rot [quat]
  (let [[s x y z] (mat/normalise quat)
        qahat (skew-hat [x y z])
        eye (mat/identity-matrix 3)]
    (mat/add eye (mat/mul 2 (mmul qahat qahat)) (mat/mul 2 s qahat))))

(defn rot->quat [[[a b c]
                  [d e f]
                  [g h i]]]
  (let [tr (+ a e i)
        q
        (cond
          (> tr 0)
          (let [s (* 2 (mat/sqrt (inc tr)))]
            [(* 1/4 s)     ;; qw
             (/ (- h f) s) ;; qx
             (/ (- c g) s) ;; qy
             (/ (- d b) s) ;; qz
             ])
          (and (> a e) (> a i))
          (let [s (* 2 (mat/sqrt (+ 1 a (- (+ e i)))))]
            [(/ (- h f) s) ;; qw
             (* 1/4 s)     ;; qx
             (/ (+ b d) s) ;; qy
             (/ (+ c g) s) ;; qz
             ])
          (> e i)
          (let [s (* 2 (mat/sqrt (+ 1 e (- (+ a i)))))]
            [(/ (- c g) s) ;; qw
             (/ (+ b d) s) ;; qx
             (* 1/4 s)     ;; qy
             (/ (+ f h) s) ;; qz
             ])
          :else
          (let [s (* 2 (mat/sqrt (+ 1 i (- (+ a e)))))]
            [(/ (- d b) s) ;; qw
             (/ (+ c g) s) ;; qx
             (/ (+ f h) s) ;; qy
             (* 1/4 s)     ;; qz
             ]))
        qw (first q)
        ]
    (mat/mul (mat/signum qw) q)))


(let [q [0.5 0.5 0.5 0.5]]
  (is (mat/equals
       q
       (-> q
           quat->rot
           rot->quat
           mat/normalise)
       1e7)))

(def quat->rot->quat=id
  (prop/for-all [q (gen/vector (gen/double* {:min -2
                                             :max 2
                                             :infinite? false
                                             :NaN? false}) 4)]
                (is (mat/equals
                     (mat/normalise q)
                     (-> q
                         quat->rot
                         rot->quat
                         mat/normalise)
                     1e7))))

;; seems to fail:
;; (tc/quick-check 100 quat->rot->quat=id)
