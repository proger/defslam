(ns slam.vega
  (:require [gorilla-plot.core :as plot]
            [gorilla-renderable.core :as render]
            [clojure.core.matrix :as mat]
            [cheshire.core :as json]
            [clojure.java.io :as io]))

(def list-plot plot/list-plot)
(def histogram plot/histogram)

(def plot-colors ["#05A" "#A05" "#0A5" "#50A" "#A50" "#5A0"])
(def +plot-colors+ plot-colors)

(defn write-spec [term]
  (with-open [s (io/writer "/Users/vladki/src/vega-remote/app/assets/spec.json")]
      (json/generate-stream term s)))

(defn remoteplot [term]
  (write-spec (:content (render/render term))))

(defn get-limits
  ([points pad]
  (let [xs (mat/reshape points [(mat/ecount points)])
        x-min (reduce min xs)
        x-max (reduce max xs)
        x-pad (* pad (- x-max x-min))]
    [(- x-min x-pad) (+ x-max x-pad)]))
   ([points]
    (get-limits points 0.1)))

(comment ;; demo
  (def prior-samples [0 0 0 0 0 0 0 -1 1])
  (def posterior-samples [1 1 1 1 1 1 0 2])

  (remoteplot
   (plot/compose
    (plot/histogram prior-samples
                    :normalize :probability-density :bins 40)
    (plot/histogram posterior-samples
                    :normalize :probability-density :bins 40 :color :green)))

  (remoteplot
   (plot/list-plot (map vector (range 100) (map #(Math/sin (* % 0.1)) (range 100)))
                   :joined true)))
