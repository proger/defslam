(ns slam.core
  (:require [anglican.core :as ac :refer [doquery]]
            [anglican.runtime :as ar :refer [uniform-continuous mvn normal]]
            [anglican.emit :as ae :refer [defquery defm with-primitive-procedures]]
            [anglican.stat :as stat]
            [clojure.core.matrix :as mat]
            [slam.plot :as plot]))


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
     :motion-sigma (mat/diagonal-matrix [2 2])}))

(defn move% [{robot :robot
             bounds :bounds
             sigma :motion-sigma
             :as world} [dx dy]]
  (let [motion (ar/sample* (ar/mvn [dx dy] sigma))
        loc (mat/add robot motion)]
    (if (in-bounds bounds loc)
      (assoc world :robot loc)
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

(defn explore-step
  "move in one direction until it hits a world boundary"
  [world]
  (let [orientation (ar/sample* (ar/uniform-continuous 0 6.28))
        distance 3
        motion [(* (mat/cos orientation) distance)
                (* (mat/sin orientation) distance)]]
    (->> world
         (iterate #(move% % motion))
         (map (fn [world] (when (some? world) {:measurements (sense world)
                                               :world world
                                               :control motion})))
         (take-while some?)
         (take 40))))

(defn explore
  "keep moving around until you see all landmarks"
  [{num-landmarks :num-landmarks :as world}]
  (loop [observations (explore-step world)
         niters 0]
    (let [seen
          (->> observations
               (map :measurements)
               (map #(map :id %))
               (flatten)
               (set))]
      (when (zero? (rem niters 20))
        (println "exploring:" niters "passes"
                 "seen:" (count seen)
                 "need:" num-landmarks ))
      (if (= num-landmarks (count seen))
        {:observations observations
         :num-landmarks num-landmarks}
        (recur (concat observations (explore-step world))
               (inc niters))))))


;; :observations [{:measurements :control :world}]
(def !data (atom []))

(defn simulate []
  (let [world (random-world [100 100])
        {obs :observations
         :as data} (explore world)]
    (reset! !data data)
    (count obs)))

;; (simulate)

;; random variables: N poses + 10 landmarks

(defn ground-truth [{:keys [observations num-landmarks]}]
  (let [any-world (:world (first observations))
        landmarks (:landmarks any-world)
        poses (map #(-> % :world :robot) observations)]
    {:poses poses
     :starting-pose (-> any-world (:robot))
     :landmarks landmarks}))

(ground-truth @!data)

;; @!data
;;(matrix)

(defm sample-values [map-]
  (into {} (map (fn [[k v]] [k (sample v)]) map-)))

(def magnitude #(mat/magnitude %))

;; given true poses and noisy measurements,
;; sample from landmark positions, try to compute their measurements
;; and (observe (normal fake-measurement big-sigma) real-measurement)

;; using magnitude instead of mat/distance because it takes one argument
(with-primitive-procedures [magnitude]
  (defquery slam-landmarks [{:keys [observations num-landmarks]}]
    (let [[max-x max-y] (-> (first observations) (:world) (:bounds))
          landmark-dists (map (fn [i] [i [(uniform-continuous 0 max-x)
                                          (uniform-continuous 0 max-y)]])
                              (range num-landmarks))
          ;;true-poses (map #(-> % :world :robot) observations)
          ;;samples (map #(sample-values %) landmarks)
          ]
      (reduce
       (fn [state {:keys [measurements world control]}]
         (let [[pose-x pose-y] (:robot world)
               sensor-sigma (:sensor-sigma world)
               landmark-guesses (map
                                 (fn [[i [x-dist y-dist]]]
                                   [i [(sample x-dist) (sample y-dist)]])
                                 landmark-dists)
               distance-guesses (map
                                 (fn [[id [x y]]]
                                   (magnitude
                                    [(- pose-x x) (- pose-y y)]))
                                 landmark-guesses)
               ;;_ (println distance-guesses)
               ]

              (reduce
               (fn [_ {:keys [id dist]}]
                 (observe (normal (nth distance-guesses id) sensor-sigma) dist))
               nil
               measurements)

              landmark-guesses))
       []
       observations))))


;; (defn report-every [n coll]
;;   ())

;; when using importance sampling gotta use log-weights, not just take last.
(def samples
  (->> ;;(doquery :ipmcmc slam-landmarks  [@!data] :number-of-nodes 8 :number-of-particles 4)
       (doquery :smc slam-landmarks  [@!data] :number-of-particles 100)
       (take 50000)
       (doall)
       time))

(def empirical-posterior
  (->> samples
       stat/collect-results
       stat/empirical-distribution
       ;;(into (sorted-map))
       ))

(get  empirical-posterior (apply max-key val empirical-posterior))

(apply max-key val empirical-posterior)

(def real-landmarks (->> @!data
                         :observations
                         first
                         :world
                         :landmarks))

(->> empirical-posterior
     (apply max-key val)
     key
     (map println real-landmarks))
