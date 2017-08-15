;; (remove-ns 'slam.core)
(ns slam.core
  (:require [anglican.core :as ac :refer [doquery]]
            [anglican.runtime :as ar :refer [uniform-continuous mvn normal]]
            [anglican.emit :as ae :refer [defquery defm with-primitive-procedures]]
            [anglican.stat :as stat]
            [clojure.core.matrix :as mat]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]
            [slam.anglican :as sa]
            ;;[slam.plot :as plot]
            [slam.vega :as vega]
            [slam.world2d :as world2d]
            [gorilla-plot.core :as plot]))

(defn unit-weight
  "returns a value together with its weight, useful for stat/empirical-*"
  [x]
  [x 1])

(def magnitude #(mat/magnitude %))
(def distance mat/distance)
(def mat-add mat/add)

;; given true poses and noisy measurements,
;; sample from landmark positions, try to compute their measurements
;; and (observe (normal fake-measurement big-sigma) real-measurement)

;; landmarks and poses:
;; initial pose: sample from uniform grid
;; at every step, infer the new pose by applying a motion model (integrating) and sampling from noise model
;; compute distance guesses with the pose sample

;; idea: try independent queries for every landmark (assuming true paths)

(with-primitive-procedures [distance mat-add]
  (defquery slam-landmarks [{:keys [initial-world observations num-landmarks]}]
    (let [[max-x max-y] (-> initial-world :bounds)
          landmarks (into (sorted-map) (map (fn [i] [i [(sample (uniform-continuous 0 max-x))
                                                        (sample (uniform-continuous 0 max-y))]])
                                            (range num-landmarks)))

          sensor-sigma 1
          motion-sigma (-> initial-world :motion-sigma)

          poses (rest (sa/reductions (fn [last-pose {:keys [control motion]}]
                                       (let [
                                             pose (mat-add
                                                   last-pose
                                                   (sample (mvn control motion-sigma)))
                                             ;;pose (mat-add last-pose motion)
                                             ]
                                         pose)) [0 0] observations))
          _ (assert (= (count poses) (count observations)))]
      (map
       (fn obs [{:keys [measurements]} pose]
         (map
          (fn meas [{:keys [id dist true-dist]}]
            (let [landmark-loc (get landmarks id)
                  distance-guess (distance pose landmark-loc)]
              (observe (normal distance-guess sensor-sigma) true-dist)))
          measurements))
       observations poses)
      {:landmarks (into [] landmarks)
       :poses poses})))



(defn norm-error [[index x] [_ y]]
  {:index index
   :error (mat/distance x y)
   :real x
   :infer y})

(defn norm-error-print [{index :index
                         error :error
                         [x1 x2] :real
                         [y1 y2] :infer
                         :as term}]
  (printf "[%d :error %.2f :real [%.2f %.2f] :inferred [%.2f %.2f]]\n" index error x1 x2 y1 y2)
  term)

(defn report-progress
  "a batch of samples goes in, a metric comes out"
  [real-landmarks samples]
  (let [x (->> samples
               last
               :result
               :landmarks)
        errors (map (comp unit-weight :error norm-error) real-landmarks x)
        average-error (stat/empirical-mean errors)]
    average-error))


(def query-cache (atom nil))

;; when using importance sampling gotta use log-weights, not just take last.
(def samples
  (let [
        _ (reset! query-cache nil) ;; how to resume?
        cached @query-cache

        data (if (some? cached) (:data cached) (world2d/simulate)) ;;@!data

        _ (if (some? cached)
            (println "countinuing with obs count: " (count (:observations data)))
            nil)

        query (if (some? cached)
                (:query cached)
                (do
                  (doquery :palmh slam-landmarks [data])))

        real-landmarks (->> data
                            :initial-world
                            :landmarks)

        _ (reset! query-cache {:query query
                               :data data})]
    (->>
     [
      (plot/list-plot (->> data world2d/ground-truth :poses)
                      :joined true
                      :color (first vega/+plot-colors+)
                      :plot-range [[-20 120] [-20 120]]
                      )
      (plot/list-plot (->> data world2d/pose-guesses)
                      :joined true
                      :color (second vega/+plot-colors+))
      ;; vega doesn't want to plot markers
      ;; (plot/list-plot [[0 0] [ 100 100]]
      ;;                  :joined false
      ;;                  :color (second vega/+plot-colors+))

      ]
     (apply plot/compose)
     vega/remoteplot)
    (->>
     query
     ;;(doquery :ipmcmc slam-landmarks  [data] :number-of-nodes 8 :number-of-particles 4)
     ;;(doquery :smc slam-landmarks [data] :number-of-particles 1000)
     ;;(doquery :almh slam-landmarks [data])
     ;;(doquery :rmh slam-landmarks [data])
     ;;(doquery :pgas slam-landmarks-known-init [data])
     ;;(doquery :ipmcmc slam-landmarks [data])
     (partition 100)

     (map-indexed (fn [i samples]
                    (println :batch i :last-sample-error (report-progress real-landmarks samples)
                             ;;:val (last samples)
                             )
                    samples))
     (take 1000)

     flatten ;; undo partitioning

     doall
     time

     last :result :landmarks (map (comp norm-error-print norm-error) real-landmarks) doall)))

   ;; :result
   ;; (trace-header "landmark matching: actual vs inferred")
   ;; (map (comp norm-error-print norm-error) real-landmarks)
   ;; (map (comp unit-weight :error))
   ;; stat/empirical-mean
   ;; (trace-pprint "average error of the last sample")
