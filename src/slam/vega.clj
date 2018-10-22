(ns slam.vega
  (:require [gorilla-plot.core :as plot]
            [gorilla-plot.util :as util]
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

(defn comp-histogram
  "Plot the histogram of a sample."
  [data & {:keys [plot-range bins normalize normalise plot-size aspect-ratio colour color opacity fill-opacity x-title y-title]
           :or   {plot-range   [:all :all]
                  bins         :automatic
                  plot-size    400
                  aspect-ratio 1.618
                  opacity      1
                  fill-opacity  0.4}
           :as   opts}]
  (let [bin-range-spec (first plot-range)
        range-min (if (= bin-range-spec :all) (apply min data) (first bin-range-spec))
        range-max-raw (if (= bin-range-spec :all) (apply max data) (second bin-range-spec))
        ;; ensure the largest point is included
        ;; TODO: does this always work? With Clojure numeric types?
        range-max (+ range-max-raw (Math/ulp (double range-max-raw)))
        points-in-range (util/count-in-range data range-min range-max)
        ;; if bins :automatic then use the Sturges rule (it's simple)
        num-bins (if (= bins :automatic) (Math/ceil (+ 1 (/ (Math/log points-in-range) (Math/log 2)))) bins)
        bin-size-raw (/ (- range-max range-min) (double num-bins))
        ;; this is a hack to catch the case when all of the points are identical.
        ;; TODO: this could probably be done in a much nicer way.
        bin-size (if (< bin-size-raw 1e-15) 1.0 bin-size-raw)
        cat-counts (util/bin-counts data range-min range-max bin-size)
        ;; optionally normalise to probability - note that the normalisation is wrt the whole dataset, not just the
        ;; plotted portion.
        norm (case (or normalize normalise :count)
               :probability (count data)
               :probability-density (* (count data) bin-size)
               :count 1)
        cat-data (map #(/ % (double norm)) cat-counts)
        ]
    (let [;; we use a modified line plot to draw the histogram, rather than the more obvious bar-chart (as then the
          ;; scales are easier to work with, especially when adding lines). This requires jumping through some hoops:
          ;; move the x-points to be in the middle of their bins and add two extra
          x-data (map (partial + bin-size) (range (- range-min bin-size) (+ range-max bin-size) bin-size))
          ;; bookend the y-data with zeroes.
          y-data (concat [0] cat-data [0])
          plot-data (map vector x-data y-data)]
      plot-data)))
