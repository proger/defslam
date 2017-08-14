;; http://www.robots.ox.ac.uk/~fwood/anglican/examples/viewer/?worksheet=kalman
(ns slam.kalman
  (:require [anglican.core :as ac :refer [doquery]]
            [anglican.runtime :as ar :refer [uniform-continuous mvn normal]]
            [anglican.emit :as ae :refer [defquery defm with-primitive-procedures]]
            [anglican.stat :as stat]
            [clojure.core.matrix :as mat
             :refer [matrix identity-matrix zero-vector
                     shape inverse transpose
                     mul mmul add sub div
                     cos sin]]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]
            [slam.anglican :as sa]
            [slam.vega :as vega :refer [+plot-colors+]]
            [gorilla-plot.core :as plot]))


(defn rotation-matrix
  "A 2D rotation matrix"
  [angle]
  (mat/matrix [[(cos angle) (sin (- angle))]
               [(sin angle) (cos angle)]]))

;; number of time points
(def +T+ 100)
;; dimension of observation space
(def +D+ 20)
;; dimension of latent space
(def +K+ 2)

;; parameters
(def +init-state+ [0. 1.])
(def +omega+ (/ (* 4.0 Math/PI) +T+))
(def +trans-matrix+ (rotation-matrix +omega+))
(def +q+ 0.01)
(def +trans-cov+ (mul +q+ (identity-matrix +K+)))
(def +obs-matrix+
  (transpose
    (matrix
      (repeatedly 2 #(ar/sample* (ar/dirichlet (repeat +D+ 0.1)))))))
(def +r+ 0.01)
(def +obs-cov+ (mul +r+ (identity-matrix +D+)))
(def +init-mean+ +init-state+)
(def +init-cov+ +trans-cov+)
(def +parameters+ [+init-mean+ +init-cov+
                   +obs-matrix+ +obs-cov+
                   +trans-matrix+ +trans-cov+])

 ;; sample state sequence and observations
(def +states+
  (let [trans-dist (mvn (zero-vector +K+) +trans-cov+)]
    (reduce (fn [states _]
              (conj states
                    (add (mmul +trans-matrix+ (peek states))
                         (ar/sample* trans-dist))))
            [+init-state+]
            (range 1 +T+))))

(def +observations+
   (let [obs-dist (mvn (zero-vector +D+) +obs-cov+)]
     (map (fn [state]
            (add (mmul +obs-matrix+ state)
                       (ar/sample* obs-dist)))
          +states+)))



(def +x-limits+ (gorilla/get-limits +states+))
(def +y-limits+ (gorilla/get-limits +observations+))



(vega/remoteplot
 (apply plot/compose
        (for [[ys color] (map vector
                              (transpose +observations+)
                              +plot-colors+)]
          (plot/list-plot ys
                          :joined true
                          :color color
                          :plot-range [:all +y-limits+]))))

(with-primitive-procedures [mmul add sub shape matrix zero-vector]
  (defquery kalman
    "A basic Kalman smoother. Predicts a state sequence
    from the posterior given observations"
    [observations obs-matrix obs-cov
     trans-matrix trans-cov init-mean init-cov]
    (let [;; D is dimensionality of observation space,
          ;; K is dimensionality of latent space
          [D K] (shape obs-matrix)
          ;; prior on observation noise
          obs-dist (mvn (zero-vector D) obs-cov)
          ;; prior on initial state
          start-dist (mvn init-mean init-cov)
          ;; prior on transition noise
          trans-dist (mvn (zero-vector K) trans-cov)]

      (matrix (sa/reductions
               (fn [state obs]
                 (observe obs-dist (sub (mmul obs-matrix state) obs))

                 ;; new state:
                 (add (mmul trans-matrix state) (sample trans-dist)))
               (sample start-dist)
               observations)))))

(def samples
  (->> (doquery :smc
             kalman
             [+observations+
              +obs-matrix+ +obs-cov+
              +trans-matrix+ +trans-cov+
              +init-mean+ +init-cov+]
             :number-of-particles 1000)
       (take 10000)
       doall
       time))


(defn empirical-moments [predict-id samples]
  (let [weighted-states (stat/collect-by predict-id samples)]
    {:mean (stat/empirical-mean weighted-states)
     :var (stat/empirical-variance weighted-states)}))

(def moments (empirical-moments :result samples))





(vega/remoteplot
 (plot/compose
  (plot/list-plot (map #(into [] %) +states+)
                  :joined true
                  :color (first +plot-colors+)
                  ;;:plot-range [+x-limits+ +x-limits+]
                  )
  (plot/list-plot (->>
                   moments
                   :mean
                   mat/rows
                   (map #(into [] %)))
                   :joined true
                   :color (second +plot-colors+))))
