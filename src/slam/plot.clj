(ns slam.plot
  (:use [quil.core]))

;; Color conversion function, courtesy of Jack Rusher
(defn hex-to-rgb [hex]
  (map (comp #(Integer/parseInt % 16) (partial apply str))
       (partition 2 (.replace hex "#" ""))))

(defn hex-to-color [hex]
  (apply color (hex-to-rgb hex)))

(defn setup []
  (smooth))

(def bar-offset-w 50)
(def bar-offset-h 10)
(def bar-width 10)

(def +dataset+ (atom []))

(defn draw-plot-area
  "Render the plot area as a white box"
  []
  (background 224)
  (fill 255)
  (no-stroke)
  (rect-mode :corners)
  (rect (- bar-offset-w (/ bar-width 2)) bar-offset-h (- (width) bar-offset-w (- (/ bar-width 2))) (- (height) bar-offset-h)))

(defn draw-data-bar [[x- y-] {:keys [x-min x-max y-min y-max]}]
  (stroke-weight 2)
  (stroke (hex-to-color "#111111"))
  (fill (apply color (hex-to-rgb "#5679C1")))
  (let [x (map-range x- x-min x-max bar-offset-w (- (width) bar-offset-w))
        y (map-range y- y-min y-max bar-offset-h (- (height) bar-offset-h))
        w bar-width]
    (rect (- x (/ w 2))
          (- (height) y)
          (+ x (/ w 2))
          (height))))

(defn draw []
  (draw-plot-area)

  (let [dataset' @+dataset+
        dataset (if (empty? dataset') [[0 0]] dataset')
        x-min (apply min (map first dataset))
        x-max (apply max (map first dataset))
        y-min (apply min (map second dataset))
        y-max (apply max (map second dataset))]

      (doall
       (map (fn [[x y]]
              (draw-data-bar [x y] {:x-min x-min
                                    :x-max x-max
                                    :y-min y-min
                                    :y-max y-max})
              (stroke-weight 5)
              (stroke (hex-to-color "#00FF00"))
              (point
               (map-range x x-min x-max bar-offset-w (- (width) bar-offset-w))
               (map-range y y-min y-max (- (height) bar-offset-h) bar-offset-h)))
            dataset))) )


(defn reset-barchart [data]
  (reset! +dataset+ data))

(defn show-barchart []
  (defsketch barchart-
    :title "Empirical Distribution"
    :settings setup
    :draw draw
    :size [720 400]))

;; (barchart [[0 10] [1 20] [2 15] [3 14] [5 12]])
;; (apply min (map second  [[0 10] [1 20]]))
