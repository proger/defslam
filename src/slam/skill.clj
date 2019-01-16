

(ns slam.skill
    (:import java.util.Random
           [cern.jet.random.tdouble NegativeBinomial]
           [cern.jet.stat.tdouble Probability]
           [cern.jet.random.tdouble.engine DoubleRandomEngine DoubleMersenneTwister])
  (:require [anglican.core :as ac :refer [doquery]]
            [anglican.runtime :as ar :refer [uniform-continuous uniform-discrete mvn normal bernoulli poisson exponential sample* dirichlet wishart binomial categorical discrete observe* gamma flip defdist exp]]
            [anglican.emit :as ae :refer [defquery defm with-primitive-procedures fm query]]
            [anglican.stat :as stat]
            [anglican.inference :refer [infer]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.core.matrix :as mat
             :refer [matrix identity-matrix zero-vector
                     shape inverse transpose
                     mul mmul add sub div
                     diagonal-matrix to-nested-vectors
                     ecount
                     cos sin shape get-row]]
            [clojure.core.matrix.operators :as op]
            [clojure.core.matrix.linear :as linalg]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint print-table]]
            [clojure.string :refer [join split-lines]]
            [clojure.string :as string]
            [clojure.set :as s]
            [slam.anglican :as sa]
            [slam.vega :as vega :refer [comp-histogram simple-histogram! points-2d!]]
            [slam.world2d :as w]
            [gnuplot.core :as g]
            [gorilla-plot.core :as plot]))


(defdist dirac [x]
  (sample* [this] x)
  (observe* [this value]
            (if (= value x)
              0
              (- (/ 1.0 0.0)))))


(with-primitive-procedures [dirac]
  (defquery skill []
    (let [np (sample (normal 0 1))
          st (sample (normal 0 1))
          koti (sample (normal 0 1))]
      (observe (dirac true) (> np st))
      (observe (dirac true) (> np koti))
      (observe (dirac true) (> st koti))
      np)))

(def npskill
  (->>
   (doquery :rmh skill [])
   (drop 5000)
   (take 5000)))

(last npskill)

(stat/empirical-mean (stat/collect-by :result npskill))
(stat/empirical-std (stat/collect-by :result npskill))

(g/raw-plot!
         [[:set :output (vega/home-png "npskill")]
          [:set :term :png :truecolor :size (g/list 600 600) :fontscale 1]
          [:set :autoscale]
          [:set :style :fill :solid 0.6]
          [:plot (g/list ["-" :using (g/lit "1:2") :with :boxes])]]
         [(comp-histogram (map :result npskill))])

(simple-histogram! "npskill" npskill)


(defm sigmoid [x]
  (double (/ 1 (+ 1 (exp (- x))))))

(sigmoid 0)

(defquery skill-bern []
  (let [np (sample (normal 0 1))
        st (sample (normal 0 1))
        koti (sample (normal 0 1))]
    (observe (flip (sigmoid (- np st))) true)
    (observe (flip (sigmoid (- np koti))) true)
    (observe (flip (sigmoid (- st koti))) true)
    np))

;; seen three datapoints, what skill do we think they now have?

(def npskill
  (->>
   (doquery :rmh skill-bern [])
   (drop 5000)
   (take 5000)))

(g/raw-plot!
         [[:set :output (vega/home-png "npskill")]
          [:set :term :png :truecolor :size (g/list 600 600) :fontscale 1]
          [:set :autoscale]
          [:set :style :fill :solid 0.6]
          [:plot (g/list ["-" :using (g/lit "1:2") :with :boxes])]]
         [(comp-histogram (map :result npskill))])



(sample* (poisson 3))

(defquery many-coins [N]
 (reduce +
  (map
   (fn [_]
    (if (sample (flip 0.003)) 1 0))
   (range N))))

(defquery binomial-np [N p]
 (loop [t 0]
  ))

;; == (binomial N 0.5)

(->>
 (doquery :importance many-coins [1000])
 (take 1000)
 (map :result)
 (frequencies)
 (sort-by first)
 (into [])
 ;;(stat/collect-results)
 ;;(stat/empirical-distribution)
 ;;((fn [samples] (comp-histogram samples :normalize :probability)))
 ((fn [x] (simple-histogram! "many-coins" x :size (g/list 600 600))))
 )

(->>
 (doquery :importance many-coins [10])
 (take 10000)
 (map :result)
 (frequencies)
 (sort-by first)
 (into [])
 ;;(stat/collect-results)
 ;;(stat/empirical-distribution)
 ;;((fn [samples] (comp-histogram samples :normalize :probability)))
 ((fn [x] (simple-histogram! "poisson" x :size (g/list 600 600))))
 )

(defquery binomial- [N p]
  (loop [t 0 n 0]
    (if (= n N)
      t
      (recur (if (sample (flip p)) (inc t) t) (inc n)))))


(defquery negative-binomial [r p]
  (loop [t 0 f 0]
    (if (= r f) t
        (if (sample (flip p))
          (recur (inc t) f)
          (recur t       (inc f))))))



(->>
 (doquery :importance negative-binomial [1 0.5])
 (take 5000)
 (map :result)
 (frequencies)
 (sort-by first)
 (into [])
 ;;(stat/collect-results)
 ;;(stat/empirical-distribution)
 ;;((fn [samples] (comp-histogram samples :normalize :probability)))
 ((fn [x] (simple-histogram! "negative-binomial" x :size (g/list 600 600))))
 )

(into {} (map (fn [team] [team (sample* (normal 0 1))])) [:NT :SP])

(defn dump [data]
  (with-open [writer (io/writer "out-file.csv")]
    (csv/write-csv writer data)))


(defquery goals [n-teams matches]
  (let [homeadv (sample (normal 0 1))
        μ-atk (sample (normal 0 1))
        τ-atk (sample (gamma 0.1 1))
        μ-def (sample (normal 0 1))
        τ-def (sample (gamma 0.1 1))
        atk (repeatedly n-teams #(sample (normal μ-atk τ-atk)))
        def (repeatedly n-teams #(sample (normal μ-def τ-def)))]
    (reduce (fn [_ [home away scored conceded]]
              (observe (poisson (exp (+ homeadv (nth atk home) (nth def away)))) scored)
              (observe (poisson (exp (+ (nth atk away) (nth def home)))) conceded))
            nil matches)
    (into [] (concat atk def [homeadv]))))

(def epl-1617-nteams 20)
(def epl-1617 [[0 13 0 1] [1 16 0 1] [2 14 1 1] [3 11 2 1] [4 17 2 1] [5 12 1 1] [6 15 1 1] [7 19 3 4] [8 10 1 3] [9 18 2 1] [10 6 2 0] [0 19 2 0] [11 7 0 0] [12 4 1 4] [13 3 0 2] [14 1 1 0] [15 9 1 2] [16 2 1 2] [17 5 1 2] [18 8 1 0] [9 0 3 0] [1 8 1 1] [2 12 1 0] [3 10 0 1] [11 13 2 1] [6 17 1 1] [14 19 1 1] [15 7 1 3] [4 18 3 1] [16 5 0 0] [7 6 2 1] [8 16 1 0] [0 3 1 1] [19 11 4 1] [10 4 1 2] [5 1 1 2] [12 14 0 4] [18 15 2 4] [13 9 2 2] [17 2 0 3] [9 19 1 2] [2 5 3 1] [3 7 1 4] [11 0 3 0] [4 8 4 0] [16 18 4 2] [1 12 4 1] [6 13 1 0] [14 17 1 0] [15 10 3 1] [7 9 3 0] [8 2 1 0] [19 3 5 1] [10 11 4 1] [5 14 1 2] [12 16 1 1] [17 1 2 3] [13 4 1 3] [18 6 0 3] [0 15 2 0] [2 1 1 1] [3 9 0 2] [17 16 1 1] [13 19 1 2] [15 8 2 2] [18 5 1 1] [0 7 0 1] [11 6 0 0] [10 12 1 1] [14 4 2 0] [7 13 3 2] [8 3 6 1] [9 11 3 0] [1 18 0 1] [4 2 1 1] [12 17 2 0] [16 14 1 1] [5 15 0 1] [6 0 3 1] [19 10 0 0] [7 5 0 0] [8 14 0 0] [0 2 2 1] [3 12 0 2] [11 1 3 1] [19 16 2 1] [13 15 0 0] [18 17 1 0] [9 10 4 0] [4 6 1 1] [1 19 2 4] [10 0 0 0] [5 8 2 0] [17 7 1 4] [14 11 1 1] [15 3 1 0] [16 4 0 4] [2 18 2 0] [6 9 0 2] [12 13 3 1] [8 17 1 2] [0 1 3 2] [9 2 5 0] [4 5 1 1] [18 12 1 1] [7 14 1 1] [3 6 2 1] [11 16 1 2] [19 15 6 1] [13 10 1 3] [1 4 1 2] [2 13 1 1] [10 7 1 1] [6 19 0 0] [12 8 0 1] [17 3 3 0] [14 18 3 2] [15 11 2 1] [5 9 0 1] [16 0 4 0] [0 4 1 2] [9 14 2 1] [3 16 1 1] [11 5 2 2] [19 17 2 0] [13 1 5 4] [7 8 3 1] [10 18 1 1] [6 2 1 0] [15 12 0 1] [1 6 3 0] [4 9 1 3] [12 0 2 0] [17 11 2 1] [14 13 5 0] [16 15 3 1] [18 7 1 5] [8 19 4 3] [2 10 1 1] [5 3 1 0] [7 12 3 1] [0 8 3 2] [3 1 3 3] [11 4 4 2] [13 17 3 0] [15 2 3 2] [9 16 1 0] [19 18 2 2] [10 14 1 0] [6 5 1 0] [8 11 1 0] [2 7 2 1] [1 10 1 2] [4 15 2 0] [5 19 0 3] [12 6 0 0] [17 9 0 1] [14 3 3 0] [16 13 3 1] [18 0 1 0] [1 9 0 1] [5 13 3 0] [12 11 2 2] [17 15 1 0] [16 10 0 2] [18 3 1 0] [8 6 1 3] [4 7 2 1] [14 0 2 1] [2 19 0 1] [7 16 1 0] [0 5 1 0] [9 8 3 0] [3 4 0 3] [11 2 0 2] [10 17 3 1] [13 18 1 4] [15 1 1 1] [19 12 4 1] [6 14 1 4] [3 2 2 2] [0 17 4 1] [9 12 4 2] [11 18 1 0] [19 4 1 0] [10 5 2 1] [6 16 1 2] [13 8 0 3] [7 1 2 0] [15 14 1 4] [2 6 3 0] [4 0 2 1] [5 11 0 0] [17 19 2 2] [16 3 3 1] [18 10 0 2] [8 7 3 3] [1 13 1 2] [12 15 2 0] [14 9 2 0] [0 6 1 0] [3 8 3 1] [11 9 0 3] [17 12 1 3] [13 7 0 4] [14 16 4 0] [15 5 0 0] [18 1 3 0] [2 4 4 0] [10 19 1 1] [8 15 2 2] [1 2 0 1] [19 13 2 3] [4 14 2 2] [5 18 1 3] [12 10 1 1] [16 17 2 0] [7 0 2 1] [9 3 2 0] [6 11 3 0] [7 15 1 2] [8 1 0 2] [0 11 1 0] [19 9 1 1] [5 16 1 1] [17 14 0 0] [13 6 2 1] [10 3 0 0] [12 2 1 1] [18 4 0 4] [9 7 3 1] [1 17 0 4] [2 8 6 3] [3 19 2 0] [6 18 1 3] [14 5 1 0] [15 0 2 1] [16 12 1 0] [11 10 0 3] [4 13 2 1] [7 3 2 0] [19 14 2 0] [10 15 2 0] [5 2 0 0] [12 1 1 0] [17 6 0 4] [18 16 2 2] [0 9 1 1] [13 11 2 0] [8 4 0 2] [9 13 3 1] [1 5 1 0] [2 17 2 0] [3 0 1 1] [15 18 1 1] [16 8 2 1] [14 12 4 0] [11 19 3 1] [11 3 3 1] [19 7 3 1] [10 8 1 1] [12 5 2 0] [13 0 3 2] [15 6 3 4] [16 1 0 2] [17 4 0 2] [14 2 3 2] [18 9 1 2] [4 12 0 0] [8 18 3 2] [2 16 3 0] [3 13 2 1] [19 0 2 1] [8 13 2 0] [1 15 1 0] [2 3 4 0] [12 9 1 2] [17 0 0 0] [16 7 3 1] [18 11 2 3] [4 19 1 1] [5 10 1 3] [14 6 2 1] [0 14 0 2] [9 1 1 2] [3 18 2 1] [11 12 2 0] [19 2 3 1] [10 16 0 0] [6 8 0 0] [15 17 1 0] [7 4 2 2] [13 5 0 0] [0 12 1 0] [11 17 2 0] [10 2 1 1] [15 16 2 0] [7 18 3 0] [9 4 2 1] [3 5 4 2] [19 8 2 2] [6 1 3 1] [13 14 1 3] [8 9 1 3] [4 3 3 1] [5 0 0 0] [12 19 1 2] [14 15 4 0] [16 6 0 1] [18 13 1 0] [2 11 4 2] [17 10 0 3] [1 7 3 0] [1 11 2 2] [2 0 3 1] [6 4 0 3] [12 3 3 1] [17 18 2 2] [14 8 4 0] [15 13 1 0] [10 9 2 0] [16 19 0 1] [5 7 1 2] [8 5 4 0] [3 15 2 0] [13 12 2 0] [18 2 0 0] [0 10 0 2] [19 1 1 2] [9 6 4 2] [7 11 1 0] [1 14 0 1] [5 17 1 0] [4 10 0 0] [1 0 0 2] [6 3 0 0] [12 18 0 0] [17 8 0 1] [16 11 0 1] [2 9 0 3] [10 13 1 1] [5 4 2 2] [14 7 2 0] [15 19 0 1] [18 14 1 0] [8 12 2 2] [0 16 2 2] [3 17 0 2] [11 15 3 0] [4 1 5 0] [13 2 1 0] [7 10 2 0] [19 6 0 0] [9 5 3 0] [6 7 0 2] [2 15 1 0] [16 9 0 1] [8 0 2 1] [4 11 2 1] [5 6 1 2] [12 7 1 4] [17 13 0 2] [1 3 4 0] [14 10 2 1] [18 19 0 4] [9 15 4 3] [7 17 2 0] [4 16 3 1] [6 10 0 0] [11 14 1 6]])



(->>
 (doquery :rmh goals [epl-1617-nteams epl-1617])
 (take 10000)
 (map :result)
 (doall)
 (dump))

;; how sure are we they'll win next time?






;; https://github.com/incanter/incanter/blob/68f65781c86fd24baa87f6ef9422babff02be4ae/modules/incanter-core/src/incanter/distributions.clj#L704
(defdist neg-binomial [r p]
  [obj (NegativeBinomial. r p (DoubleMersenneTwister.))]
  (sample* [this] (cern.jet.random.tdouble.NegativeBinomial/staticNextInt r p))
  (observe* [this value] (Math/log (.pdf obj value))))

(sample* (neg-binomial 100 0.1))
