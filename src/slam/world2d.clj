(ns slam.world2d
  (:require [anglican.runtime :as ar :refer [uniform-continuous mvn normal]]
            [anglican.stat :as stat]
            [clojure.core.matrix :as mat]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]
            [slam.anglican :as sa]
            ;;[slam.plot :as plot]
            [slam.vega :as vega]
            [gorilla-plot.core :as plot]))

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
