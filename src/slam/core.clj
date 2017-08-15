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
            [gorilla-plot.core :as plot]))

(defn trace-header [message x]
  (println message)
  x)

(defn trace-pprint [message x]
  (print "=====" message "\n")
  (pprint x)
  (println "=====")
  x)

(defn traceln [x]
  (println x)
  x)

(defn unit-weight
  "returns a value together with its weight, useful for stat/empirical-*"
  [x]
  [x 1])

(defn map2 [f xs]
  (map (fn [[a b]] [a (f b)]) xs))

(defn filter2 [pred xs]
  (filter (fn [[a b]] (pred b)) xs))

(defn random-vector [[max-x max-y]]
  [(* max-x (rand)) (* max-y (rand))])

(defn grid-vector [[max-x max-y]]
  (let [loc-sigma 0.05
        loc (ar/sample* (ar/uniform-continuous 0.2 0.8))
        loc-noise (ar/normal 0 loc-sigma)]
    [(max 0 (min max-x (* max-x (+ loc (ar/sample* loc-noise)))))
     (max 0 (min max-y (* max-y (+ loc (ar/sample* loc-noise)))))]))

;; (ar/sample* (ar/normal 0 0.05))
;; (grid-vector [100 100])

(defn in-bounds [[max-x max-y] [x y]]
  (and (< x max-x) (< y max-y) (> y 0) (> x 0)))

(defn random-world [[max-x max-y :as bounds]]
  (let [num-landmarks 10]
    {:bounds bounds
     :num-landmarks num-landmarks
     ;; the robot does not know true locations of landmarks
     :landmarks (map (fn [i] [i (grid-vector bounds)]) (range num-landmarks))
     ;; the robot does not know its starting location
     :robot [(/ max-x 2) (/ max-y 2)]
     :sensor-range 10
     :sensor-sigma 0.3
     :motion-sigma (mat/diagonal-matrix [0.5 0.5])}))

(defn move% [{robot :robot
              bounds :bounds
              sigma :motion-sigma
              :as world} [dx dy]]
  (let [motion (ar/sample* (ar/mvn [dx dy] sigma))
        loc (mat/add robot motion)]
    (if (in-bounds bounds loc)
      (assoc world :robot loc :last-motion motion)
      nil)))


(defn sense [{landmarks :landmarks
              loc :robot
              range :sensor-range
              sigma :sensor-sigma}]
  (->> landmarks
       (map2 #(mat/distance % loc))
       (filter2 #(< % range))
       (map (fn [[id dist]] {:id id
                             :dist (ar/sample* (ar/normal dist sigma))
                             :true-dist dist}))))

(defn mk-observation% [world control motion]
  (when (some? world) {:measurements (sense world)
                       :world world
                       :control control
                       :motion motion}))

(defn explore-step
  "move in one direction until it hits a world boundary"
  [world]
  (let [orientation (ar/sample* (ar/uniform-continuous 0 6.28))
        distance 3
        motion [(* (mat/cos orientation) distance)
                (* (mat/sin orientation) distance)]]
    (->> world
         (iterate #(move% % motion))
         rest
         (map #(mk-observation% % motion (:last-motion %)))
         (take-while some?))))

(defn explore
  "keep moving around until you see all landmarks"
  [{num-landmarks :num-landmarks
    initial-pose :robot
    :as world}]
  (loop [observations (concat [(mk-observation% world initial-pose initial-pose)] (explore-step world))
         niters 0]
    (let [seen
          (->> observations
               (map :measurements)
               (map #(map :id %))
               (flatten)
               (set))
          last-world (-> observations last :world)]
      (when (zero? (rem niters 10))
        (println "exploring:" niters "turns;"
                 "landmarks seen:" (count seen)
                 "need:" num-landmarks ))
      (if (= num-landmarks (count seen))
        {:observations observations
         :initial-world world
         :num-landmarks num-landmarks}
        (recur (concat observations (explore-step last-world))
               (inc niters))))))

;;(take 5 (iterate inc 0))

;; :observations [{:measurements :control :world}]
(def !data (atom []))

(defn simulate []
  (let [world (random-world [100 100])
        {obs :observations
         :as data} (explore world)]
    (reset! !data data)
    (println "simulated observations:" (count obs))
    data))

;;(simulate)

;; random variables: N poses + 10 landmarks

(defn ground-truth [{:keys [observations num-landmarks initial-world]}]
  (let [landmarks (:landmarks initial-world)
        poses (map #(-> % :world :robot) observations)
        starting-pose (-> initial-world :robot)]
    {:poses poses ;;(concat [starting-pose] poses) ;; the first observation is now included
     :landmarks landmarks}))

(defm sample-values [map-]
  (into {} (map (fn [[k v]] [k (sample v)]) map-)))

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


(defn pose-guesses [{:keys [observations]}]
  (let [base-world (-> (first observations) :world)
        [max-x max-y] (-> base-world :bounds)
        initial-pose (-> base-world :robot)
        sigma (-> base-world :motion-sigma)

        pose-guesses (rest (reductions (fn [last-pose {:keys [control]}]
                                         (let [pose (mat/add
                                                     last-pose
                                                     (ar/sample* (mvn control sigma)))]
                                           pose)) [0 0] observations))]
    pose-guesses))






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

        data (if (some? cached) (:data cached) (simulate)) ;;@!data

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
      (plot/list-plot (->> data ground-truth :poses)
                      :joined true
                      :color (first vega/+plot-colors+)
                      :plot-range [[-20 120] [-20 120]]
                      )
      (plot/list-plot (->> data pose-guesses)
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
