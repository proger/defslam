

(ns slam.playground
  (:require [anglican.core :as ac :refer [doquery]]
            [anglican.runtime :as ar :refer [uniform-continuous uniform-discrete mvn normal bernoulli poisson exponential sample* dirichlet wishart binomial categorical discrete observe* gamma flip defdist exp]]
            [anglican.emit :as ae :refer [defquery defm with-primitive-procedures fm query]]
            [anglican.stat :as stat]
            [anglican.inference :refer [infer]]
            [clojure.data.csv :as csv]
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
            [clojure.java.io :as io]
            [gorilla-plot.core :as plot]))


;; (defmacro export [& names]
;;   (into {} (map #(vector (keyword %) %) names)))

;; (defmacro export [& names]
;;   `(into
;;     {}
;;     (map vector [~@(map keyword names)] [~@names])))

(defn timestamp [] (Math/round (/ (System/currentTimeMillis) 1000.)))

(defn enumerate [& xs]
  (apply map vector (range) xs))

(defn round
  "Round a number to the given number of significant digits"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defquery template []
  "query template"
  (let [x (sample (bernoulli 0.5))]
    (observe (normal x 1.) 0.8)
    (predict x)))

(sample* (bernoulli 0.5))

(->>
     ;;doquery-result
     ;;(doquery :ipmcmc slam-landmarks  [data] :number-of-nodes 8 :number-of-particles 4)
     ;;(doquery :smc slam-landmarks [data] :number-of-particles 1000)
     ;;(doquery :almh slam-landmarks [data])
     (doquery :importance template [])
     ;;(doquery :pgas slam-landmarks-known-init [data])
     ;;(doquery :ipmcmc slam-landmarks [data])
     (take 1)
     )

;;     Brand
 ;; "hp": 519.68,                   # 520, 584,650,719,792
 ;; "hpperlevel": 88,               # fake
 ;; "mp": 469,                      # 469, 484,499,516,533
 ;; "mpperlevel": 21,
 ;; "movespeed": 340,
 ;; "armor": 21.88,                 # 22, 24, 27, 30, 33
 ;; "armorperlevel": 3.5,
 ;; "spellblock": 30,               # 30, 30, 31, 31, 32
 ;; "spellblockperlevel": 0.5,      # inc trully
 ;; "attackrange": 550,
 ;; "hpregen": 5.5,                 # fake 1.1(6), 1.2(6),1.3(6),1.3(7), 1.4(7)
 ;; "hpregenperlevel": 0.55,        # fake
 ;; "mpregen": 10.665,              # fake 2.1(11), 2.1(11),2.3(12),2.4(12),2.5(13)
 ;; "mpregenperlevel": 0.6,         # fake
 ;; "crit": 0,
 ;; "critperlevel": 0,
 ;; "attackdamage": 57.04,          # 57,59,61, 64, 66
 ;;                                 # 14,14,14,14,14
 ;; "attackdamageperlevel": 3,
 ;; "attackspeedoffset": 0,
 ;; "attackspeedperlevel": 1.36     # fake 0.659,0.665,0.672, 0.679, 0.686

 ;; Ashe
 ;; "hp": 539,                      # 603,,732,802
 ;; "hpperlevel": 85,
 ;; "mp": 280,                      # 303,,352,378
 ;; "mpperlevel": 32,
 ;; "movespeed": 325,
 ;; "armor": 26,                    # 28, 31, 34,37
 ;; "armorperlevel": 3.4,
 ;; "spellblock": 30,               # 30, 31, 31,32
 ;; "spellblockperlevel": 0.5,
 ;; "attackrange": 600,
 ;; "hpregen": 3.5,                 # 0.7,0.8, ,0.9(5), 1(5)
 ;; "hpregenperlevel": 0.55,
 ;; "mpregen": 6.972,               # 1.4, 1.5, , 1.6(8), 1.6(8)
 ;; "mpregenperlevel": 0.4,
 ;; "crit": 0,
 ;; "critperlevel": 0,
 ;; "attackdamage": 61,             # 63(+8),65(+8), 68(+8),70(+8)
 ;; "attackdamageperlevel": 2.96,
 ;; "attackspeedoffset": -0.05,     #0.694, 0.701, 0.727, 0.744, 0.762
 ;; "attackspeedperlevel": 3.33

;; http://leagueoflegends.wikia.com/wiki/Critical_strike
(def crit-atk-rate 2)

;; http://leagueoflegends.wikia.com/wiki/Attack_speed
(defn atk-speed [atk-delay] (/ 0.625 (inc atk-delay)))

(defn champ-statistic [base bonus level]
  (+ base
     (*
      bonus
      (dec level)
      (+
       0.7025
       (* 0.0175 (dec level))))))

(def brand
  {:base {:hp 519
          :armor 22
          :regen 6
          :atk 57
          :atkdelay 0.1
          :crit-chance 0}
   :bonus {:hp 88
           :armor 4
           :regen 1
           :atk 3
           :atkdelay 0.01
           :crit-chance 0}})

(def ashe
  {:base {:hp 539
          :armor 26
          :regen 4
          :atk 61
          :atkdelay 0.1
          :crit-chance 0}
   :bonus {:hp 85
           :armor 3
           :regen 1
           :atk 3
           :atkdelay 0.01
           :crit-chance 0}})


;; duel outcome: right clicking + spell combinations use and timing

(-> brand :base :hp)
(-> ashe :base :hp)


(def some-headers {"Content-Type" "application/json"
                   "Host" "localhost"})

;; (defn ann [state obs & rest]
;;   (concat ))

(list
 :method "GET" :lit " " :path "/" :lit " " :http "HTTP/1.1" :lit "\r\n"
 :k "Host" :lit ": " :v "localhost" :lit "\r\n"
 :k "Accept" :lit ": " :v "text/plain" :lit "\r\n"
 :lit "\r\n")

(defn http-request [{method :method
                     path :path
                     headers :headers
                     body :body?}]
  (let [all-headers
        (->>
         headers
         (map #(join ": " %))
         (map #(str % "\r\n"))
         (apply str))]
      (str method " " path " HTTP/1.1\r\n"
           all-headers
           "\r\n"
           body)))

(http-request {:method "GET"
               :path "/"
               :headers some-headers
               :body? "LOL"})

1
(repeatedly 10 #(sample* (poisson 5)))

(def message-counts
  (->>
   "/Users/proger/message-counts.jun-oct"
   (slurp)
   (string/split-lines)
   (map #(string/split % #" +"))
   (map #(drop 1 %))
   (map first)
   (map #(Integer/parseInt %))
   vec))

(def ndays (count message-counts))

(defn mean [xs]
  (/ (reduce + xs) (float (count xs))))

(def alpha (/ 1 (mean message-counts)))

(defquery switchpoint []
  (let [λ-dist ;;(exponential (/ 1 (mean message-counts)))
        (uniform-discrete 10 (apply max message-counts))
        λ₁ (sample λ-dist)
        λ₂ (sample λ-dist)
        λ₃ (sample λ-dist)
        τ₁ (sample (uniform-discrete 0 ndays))
        τ₂ (sample (uniform-discrete τ₁ ndays))

        _ (reduce
           (fn [_ [day count]]
             (let [λ (cond
                       (< day τ₁) λ₁
                       (< day τ₂) λ₂
                       :else      λ₃)]
               (observe (poisson λ) count)
               nil))
           (map vector (range) message-counts))]
    {:λ₁ λ₁ :λ₂ λ₂ :λ₃ λ₃ :τ₁ τ₁ :τ₂ τ₂}))

(repeatedly
 5
 (fn []
    (->>
     (doquery :rmh switchpoint [] :alpha 0.5 :sigma 2)
     (map :result)
     (take 4000)
     ((fn [xs]
        (g/raw-plot!
         [[:set :output (str "/Users/proger/plot-" (timestamp) ".png")]
          [:set :term :png :truecolor :size (g/list 1000 200) :fontscale 0.5]
          [:set :style :fill :solid 0.6]
          [:set :autoscale]
          [:set :xrange (g/range 0 ndays)]
          [:plot (g/list
                  ;; ["-" :title "λ1" :with :boxes]
                  ;; ["-" :title "λ2" :with :boxes]
                  ;; ["-" :title "λ3" :with :boxes]
                  ["-" :title "τ1" :with :boxes]
                  ["-" :title "τ2" :with :boxes])]]
         (map #(comp-histogram (map % xs) :normalize :probability)
                            [ ;;:λ₁ :λ₂ :λ₃
                             :τ₁ :τ₂]))))
     )
    ))



(def smcq (doquery :smc switchpoint []))
(comment
  (->>
   smcq
   (drop 10)
   (take 1)
   )
  )

(repeatedly 10 #(sample* (exponential alpha)))

(sample*  (uniform-discrete 0 ndays))



(defn flo [x] (Float/parseFloat x))

(def iris-matrix
  (->>
   "https://raw.githubusercontent.com/uiuc-cse/data-fa14/gh-pages/data/iris.csv"
   slurp
   csv/read-csv
   rest
   (map
    (fn [[sl sw pl pw class]]
      [(flo sl) (flo sw) (flo pl) (flo pw) (keyword class)]))
   (mat/matrix)))

(def iris-X
  (mat/select iris-matrix :all [0 1 2 3]))

(def iris-y
  (mat/select iris-matrix :all [4]))

(defn iris-true-label [n]
  (first (mat/get-row iris-y n)))



(sample*  (bernoulli 0.5))


(simple-histogram!
 "bernoulli"
 (->>
  (repeatedly 1000 #(sample* (bernoulli 0.5)))
  (frequencies))
 :xrange (g/range -1 2)
 :yrange (g/range 0 1000))

(defn prob-histogram [xs] (comp-histogram xs :normalize :probability))

(simple-histogram!
 "clt"
 (->>
  (repeatedly 50 #(sample* (uniform-continuous 0 100)))
  (reductions +)
  (map (fn [n x] (/ x n)) (map (comp float inc) (range)))
  (prob-histogram))
 :xrange (g/range 0 100)
 :yrange (g/range 0 1)
 :size (g/list 500 500)
 :with :boxes)


(with-primitive-procedures [diagonal-matrix]
  (defquery circles []
    (let [Σ (diagonal-matrix [0.00001 0.00001])
          one (mvn [0 0] Σ)
          two (mvn [0.1 0.1] Σ)
          three (mvn [-0.1 0.1] Σ)]
      [(sample one) (sample two) (sample three)])))

(def some-circles
  (->>
   (doquery :rmh circles [])
   (map :result)
   (take 20)
   (reduce concat)))

(->>
 some-circles
 ;;(map #(map (fn [[x y]] (str x " " y)) %))
 (points-2d! "circles")
 )

(def some-circles-mat (mat/matrix some-circles))

;;(for [[x y] (map #(mat/select % [0 1]) (take 2 (mat/rows some-circles-mat)))] (println x y))

(def mat-columns mat/columns)
(def mat-rows mat/rows)
(def mat-select mat/select)


(with-primitive-procedures [diagonal-matrix mat-columns mat-rows mat-select]
  (defquery circles-unknown-means [obs]
    (let [Σ (diagonal-matrix [0.001 0.001])

          assignment (dirichlet [1.0 1.0 1.0])
          cluster-probabilities (map vector [:one :two :three] (sample assignment))

          ;; m1 [0 0]
          ;; m2 [10 10]
          ;; m3 [-10 10]
          ;; dists {:one (mvn m1 Σ)
          ;;        :two (mvn m2 Σ)
          ;;        :three (mvn m3 Σ)}

          [xs ys] (apply vector (mat-columns obs))
          xs-min (apply min xs)
          ys-min (apply min ys)
          xs-max (apply max xs)
          ys-max (apply max ys)
          m1 [(sample (uniform-continuous xs-min xs-max))
              (sample (uniform-continuous ys-min ys-max))]
          m2 [(sample (uniform-continuous xs-min xs-max))
              (sample (uniform-continuous ys-min ys-max))]
          m3 [(sample (uniform-continuous xs-min xs-max))
              (sample (uniform-continuous ys-min ys-max))]
          dists {:one   (mvn m1 Σ)
                 :two   (mvn m2 Σ)
                 :three (mvn m3 Σ)}

          ]
      (map (fn [row]
             (let [cluster (sample (categorical cluster-probabilities))]
               (observe (get dists cluster) row)))
           (mat-rows obs))
      [m1 m2 m3])))

(def samples
  (->>
   (doquery :palmh circles-unknown-means [some-circles-mat])
   (drop 10000)
   (take 1000)))

(last samples)

(def sorted-importance-weighted-posterior-samples
  (->> samples
     (stat/collect-by :result)
     (stat/empirical-distribution)
     (#(into (sorted-map-by (fn [key1 key2]
                              (compare [(get % key2) key2]
                                       [(get % key1) key1]))) %))
     ))

(pprint (take 3 sorted-importance-weighted-posterior-samples))

(doquery :siman circles-unknown-means [some-circles-mat])

(map #(observe* (mvn [0 0] (diagonal-matrix [1 1])) %) (take 10 (mat/rows some-circles-mat)))

(sample* (discrete [1 1 1]))
(sample* (categorical [[:a 1] [:b 1] [:c 1]]))

(->>
 (dirichlet [1.0 1.0 1.0])
 (sample*)
 (map vector [:one :two :three])
 (categorical)
 (sample*)
 )


(->>
 (dirichlet [1.0 1.0 1.0])
 (sample*)
 (discrete)
 (sample*)
 )











;; helper methods
(defn row-mean [data] (op// (reduce op/+ data) (mat/row-count data)))
(defn invert
  ([W] (linalg/solve W))
  ([kappa W] (linalg/solve (op/* kappa W))))


(with-primitive-procedures [row-mean invert shape identity-matrix get-row diagonal-matrix]
  (defquery gmm [data & [hyperparams]]
    (println "provided hyperparameters:" hyperparams)
    (let [[N D] (shape data)

          ;; there are many hyperparameters; we provide defaults
          K         (:K         hyperparams 10)
          alpha     (:alpha     hyperparams 1.0)
          mu-0      (:mu-0      hyperparams (row-mean data))
          lambda-0  (:lambda-0  hyperparams (identity-matrix D))
          nu        (:nu        hyperparams (inc D))
          kappa     (:kappa     hyperparams 1.0)

          ;; sample the latent variables.
          ;;
          ;; mu and sigma are per-cluster; ideally we would
          ;; sample them lazily

          pi (sample (dirichlet (repeat K alpha)))
          lambda (into [] (map (fn [x] (sample x))
                               (repeat K (wishart nu lambda-0))))
          mu (into [] (map
                        (fn [k]
                          (sample (mvn mu-0
                                       (invert kappa
                                               (get lambda k)))))
                        (range K)))
          sigma (into [] (map invert lambda))]
      ;; for each data point, sample z[n] and `observe`
      (loop [n 0
             z []]
        (if (= n N)
          z
          (let [row (get-row data n)
                k (sample (discrete pi))]
            (observe (mvn (get mu k) (diagonal-matrix [0.00005 0.00005])) row)
            (recur (inc n) (conj z k))))))))

(shape some-circles-mat)

(defn draw-samples [n-samples]
  "simple helper to draw samples"
  (take n-samples
        (doquery :smc gmm [some-circles-mat
                           {:K 3
                            :mu-0 (row-mean some-circles-mat)
                            :kappa 1.0
                            :nu 10.0
                            :lambda-0 (mat/mul (mat/identity-matrix 2) 0.5)
                            :alpha 0.5}] :number-of-particles 10000)))

(def N 20000)
(def samples (draw-samples N))

(def sorted-importance-weighted-posterior-samples
  (->> samples
     (take N)
     (stat/collect-by :result)
     (stat/empirical-distribution)
     (#(into (sorted-map-by (fn [key1 key2]
                         (compare [(get % key2) key2]
                                  [(get % key1) key1]))) %))))


(simple-histogram! "gamma" (comp-histogram (repeatedly 1000 #(sample* (gamma 3 0.15)))))


(defn zip [& args] (apply map vector args))

(def mus [[100 200] [200 100] [200 300]])
(def sigmas (map diagonal-matrix [[20 5] [10 10] [5 20]]))
(def clusters (categorical (zip (map mvn mus sigmas) [1 1 1])))

(def gmm-points
  (repeatedly
   100
   #(sample* (sample* clusters))))

(points-2d! "gmm" gmm-points)

(class (repeatedly 3 #(vector (sample* (discrete [0.5 0.5])) (sample* (discrete [0.5 0.5])))))

(defquery gmm1 [data]
  (let [μ-dist (normal 200 40)
        σ-dist (gamma 1 0.1)
        μ (into [] (repeatedly 3 #(vector (sample μ-dist) (sample μ-dist))))
        σ (into [] (repeatedly 3 #(vector (sample σ-dist) (sample σ-dist))))
        weights (sample (dirichlet [1.0 1.0 1.0]))
        cluster-dist (discrete weights)]
    (map
     (fn [[x y]]
       (let [cluster (sample cluster-dist)
             [x-mu y-mu] (get μ cluster)
             [x-sigma y-sigma] (get σ cluster)]
         (observe (normal x-mu x-sigma) x)
         (observe (normal y-mu y-sigma) y))) data)
    {:μ μ
     :σ σ
     :weights weights}))

(def samples
  (->>
   (doquery :palmh gmm1 [gmm-points])
   (drop 5000)
   (take 10000)
   (doall)))

(let [result (:result (last samples))
      mus' (into {} (zip (range) (zip (sort (:μ result)) (sort mus))))
      sigmas' (into {} (zip (range) (zip (sort  (:σ result)) (sort sigmas))))]
  (prn :μ)
  (pprint mus')
  (prn :σ)
  (pprint sigmas'))


;; :μ
;; {0 [[128.93520581016764 201.42345056659642] [100 200]],
;;  1 [[199.3844406173993 100.01754785388663] [200 100]],
;;  2 [[200.30400178014986 311.54944898680435] [200 300]]}
;; :σ
;; {0 [[2.7661741701380933 12.68440588882472] [5 20]],
;;  1 [[4.320804594103503 2.793681192418604] [10 10]],
;;  2 [[28.81562811920519 2.752365748098351] [20 5]]}

(defn sortmap [m]
  (into (sorted-map-by (fn [key1 key2]
                         (compare [(get m key2) key2]
                                  [(get m key1) key1]))) m))




(defdist dirac [x]
  (sample* [this] x)
  (observe* [this value]
            (if (= value x)
              0
              (- (/ 1.0 0.0)))))


(with-primitive-procedures [dirac]
  (defquery skill []
    (let [np (sample (normal 25 8))
          st (sample (normal 25 8))
          koti (sample (normal 25 8))]
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

(simple-histogram! "gamma" (comp-histogram (repeatedly 1000 #(sample* (gamma 3 0.15)))))



(sample* (dirac true))
(observe* (flip 0.999999) (> 1 0))



(with-primitive-procedures [dirac]
  (defquery some-gaussian []
    (let [np (sample (normal 25 8))]
      ;;(observe (dirac true) (> np -1000))
      np)))

(def sgs
  (->>
   (doquery :rmh some-gaussian []
            :number-of-particles 100 :number-of-nodes 10)
   (drop 5000)
   (take 5000)))



(g/raw-plot!
         [[:set :output (vega/home-png "some-gaussian")]
          [:set :term :png :truecolor :size (g/list 600 600) :fontscale 1]
          [:set :autoscale]
          [:set :style :fill :solid 0.6]
          [:plot (g/list ["-" :using (g/lit "1:2") :smooth :frequency])]]
         [(comp-histogram (map :result sgs))])


(defm sigmoid [x]
  (double (/ 1 (+ 1 (exp (- x))))))

(def matches [["11-Б" "Zimba"] ["11-Б" "PPS"] ["11-Б" "intSq"] ["PPS" "DAF"] ["Zimba" "WOLFs"] ["WOLFs" "Zveri"] ["intSq" "kitt"] ["kitt" "PPS"] ["kitt" "Zveri"] ["Zveri" "Zimba"] ["kitt" "Offic"] ["PPS" "DAF"] ["intSq" "ERTHQ"] ["ERTHQ" "11-Б"] ["intSq" "WOLFs"] ["NP" "On.Vi"] ["NP" "PREDA"] ["-gS" "On.Vi"] ["On.Vi" "PREDA"] ["-gS" "BD"] ["Dev1l" "DA"] ["Dev1l" "ST"] ["Dev1l" "On.Vi"] ["ST" "PREDA"] ["DA" "Zver"] ["DA" "zxazx"] ["Dev1l" "PREDA"] ["Dev1l" "DA"] ["DA" "ST"] ["Dev1l" "NP"] ["PREDA" "TK"] ["TK" "TNGGN"] ["PREDA" "-gS"] ["NL" "RNG"] ["RNG" "benz"] ["benz" "NP"] ["benz" "123"] ["benz" "A16"] ["A16" "john"] ["A16" "PT"] ["john" "dktz"] ["benz" "AWD"] ["benz" "FaBs"] ["AWD" "BD"] ["123" "UA"] ["123" "Xcali"] ["123" "TGeo"] ["Xcali" "NGLN"] ["UA" "EM"] ["UA" "TKP"] ["EM" "TK"] ["NP" "FM"] ["FM" "EG"] ["EG" "AKI"] ["AKI" "TKNEU"] ["EG" "2KG"] ["FM" "wn"] ["FM" "SL"] ["wn" "AP"] ["NP" "H.i.S"] ["H.i.S" "Wild"] ["Wild" "ETeam"] ["H.i.S" "TNGGN"] ["NP" "viamo"] ["NP" "TMT"] ["viamo" "PRSE"] ["RNG" "STFU"] ["RNG" "KRB"] ["RNG" "FLC"] ["RNG" "5cns"] ["5cns" "-CTW-"] ["RNG" "DTeam"] ["FLC" "4erep"] ["KRB" "T.NFS"] ["KRB" "5kage"] ["T.NFS" "RQ"] ["STFU" "W.L"] ["STFU" "SALO"] ["STFU" "Pot"] ["SALO" "CouG"] ["W.L" "DH"] ["DH" "InG"] ["W.L" "LOM"] ["NL" "KOTI"] ["KOTI" "MBGG"] ["MBGG" "Skvaz"] ["MBGG" "T777"] ["MBGG" "GS"] ["T777" "IC"] ["Skvaz" "NG"] ["Skvaz" "XDS"] ["NG" "ST"] ["KOTI" "NC"] ["NC" "VU"] ["VU" "exl"] ["NC" "DA"] ["KOTI" "AesT"] ["KOTI" "TLV"] ["AesT" "YB"] ["NL" "EVO"] ["NL" "Ky"] ["NL" "TBF"] ["TBF" "FP"] ["NL" "VML"] ["Ky" "B-OG"] ["B-OG" "OG"] ["Ky" "LUWE"] ["EVO" "IF"] ["IF" "CMD"] ["CMD" "SS"] ["IF" "Zver"] ["EVO" "ASAP"] ["EVO" "Dev1l"] ["ASAP" "NXG"] ["KOTI" "OP"] ["KOTI" "NC"] ["NC" "OXY"] ["OXY" "NL"] ["OXY" "Skvaz"] ["NL" "TKP"] ["NC" "TDUSH"] ["NC" "exl"] ["TDUSH" "FM"] ["KOTI" "DX"] ["DX" "ASAP"] ["ASAP" "TIWS"] ["DX" "A16"] ["KOTI" "KRB"] ["KOTI" "OFF"] ["KRB" "TKNEU"] ["OP" "-gS"] ["-gS" "EM"] ["-gS" "Ky"] ["-gS" "B-OG"] ["Ky" "BanP"] ["EM" "IC"] ["EM" "john"] ["IC" "DTeam"] ["OP" "EVO"] ["EVO" "H.i.S"] ["EVO" "RNG"] ["H.i.S" "UA"] ["OP" "SALO"] ["SALO" "ST"] ["OP" "4erep"] ["ASAP" "OP"] ["OP" "EVO"] ["EVO" "B-OG"] ["B-OG" "DX"] ["EVO" "NC"] ["OP" "UA"] ["OP" "FM"] ["UA" "BanP"] ["ASAP" "KOTI"] ["KOTI" "GG"] ["KOTI" "BMB"] ["GG" "EM"] ["ASAP" "NP"] ["NP" "IC"] ["ASAP" "H.i.S"] ["OP" "KOTI"] ["KOTI" "Ky"] ["KOTI" "BanP"] ["BanP" "IC"] ["KOTI" "FM"] ["Ky" "Kal"] ["Kal" "EM"] ["Ky" "RNG"] ["OP" "NP"] ["OP" "BD"] ["BD" "UA"] ["OP" "B-OG"] ["NP" "john"] ["NP" "H.i.S"] ["KOTI" "OP"] ["KOTI" "H.i.S"] ["H.i.S" "UA"] ["UA" "BD"] ["H.i.S" "DH"] ["KOTI" "B-OG"] ["KOTI" "DX"] ["B-OG" "Ky"] ["OP" "FM"] ["FM" "STFU"] ["OP" "EG"] ["ASAP" "KOTI"] ["ASAP" "OP"] ["ASAP" "FM"] ["ASAP" "EM"] ["FM" "BRain"] ["OP" "Ky"] ["OP" "UA"] ["KOTI" "B-OG"] ["B-OG" "EG"] ["KOTI" "H.i.S"] ["ASAP" "KOTI"] ["KOTI" "H.i.S"] ["KOTI" "FM"] ["FM" "FLC"] ["FM" "UA"] ["FM" "B-OG"] ["UA" "FGP"] ["FLC" "Wild"] ["Wild" "john"] ["FLC" "TLV"] ["KOTI" "EVO"] ["EVO" "IC"] ["EVO" "AP"] ["IC" "ETeam"] ["KOTI" "BD"] ["KOTI" "W.L"] ["BD" "NC"] ["H.i.S" "SELO"] ["SELO" "SC"] ["SELO" "OFF"] ["OFF" "Ky"] ["SELO" "Xcali"] ["SC" "viamo"] ["H.i.S" "DX"] ["H.i.S" "BanP"] ["DX" "OXY"] ["ASAP" "DH"] ["ASAP" "SL"] ["ASAP" "EG"] ["ASAP" "KRB"] ["EG" "STFU"] ["SL" "NG"] ["SL" "SS"] ["NG" "A16"] ["DH" "4erep"] ["DH" "ST"] ["DH" "NXG"] ["ST" "NP"]])

(def teams (distinct (reduce concat matches)))

(defn dump-csv [name data]
  (with-open [writer (io/writer name)]
    (csv/write-csv writer data)
    data))

(defquery dotaskill [[matches teams]]
  (let [skills (zipmap teams (repeatedly (count teams) #(sample (normal 0 1))))]
    (reduce
     (fn [_ [winner loser]]
       ;;(observe (flip (sigmoid (- (get skills winner) (get skills loser)))) true)
       (observe (normal 1 0.001) (if (> (get skills winner) (get skills loser)) 1 0)) nil
       ) nil matches)
    (into [] (map #(get skills %) ["NP" "ST" "ASAP" "KOTI"]))))

(->> (doquery :palmh dotaskill [[matches teams]]) (take 1))

(def samples
  (->>
   (doquery :palmh dotaskill [[matches teams]])
   (drop 1000)
   (take 10000)
   (map :result)
   (dump-csv "/Users/proger/np-st-asap-koti.csv")))

(take 100 samples)
(g/raw-plot!
 [[:set :output (vega/home-png "NP-vs-ST")]
  [:set :term :png :truecolor :size (g/list 600 600) :fontscale 1]
  [:set :autoscale]
  [:set :style :fill :solid 0.3]
  ;; [:set :yrange (g/range 0 1.2)]
  [:plot (g/list ["-" :using (g/lit "1:2") :smooth :frequency :lw 3 :with :boxes :title "skill density NP"]
                 ["-" :using (g/lit "1:2") :smooth :frequency :lw 3 :with :boxes :title "skill density ST"]
                 ["-" :using (g/lit "1:2") :smooth :frequency :lw 3 :with :boxes :title "skill density ASAP"]
                 ["-" :using (g/lit "1:2") :smooth :frequency :lw 3 :with :boxes :title "skill density KOTI"])]]
 [(comp-histogram (map #(nth % 0) samples))
  (comp-histogram (map #(nth % 1) samples))
  (comp-histogram (map #(nth % 2) samples))
  (comp-histogram (map #(nth % 3) samples))])

(defquery showdown []
  (let [a (sample (normal 1000 200))
        b (sample (normal 1200 200))]
    (> b a)))

(->> (doquery :rmh showdown []) (take 100000) (stat/collect-results) (stat/empirical-distribution) prn)

(observe* (normal 1 0.111) 1)

(simple-histogram! "showdown" (->> (doquery :rmh showdown []) (take 500) (map :result) (comp-histogram)))

(def samples
  (->>
   (doquery :smc dotaskill [[(map #(map keyword %) matches) (map keyword teams)]])
   (take 1)
   (prn)))


(defquery coin []
  (sample (bernoulli 0.5)))



(->>
 (doquery :importance coin [])
 (take 10000)
 (stat/collect-results)
 (stat/empirical-distribution)
 ((fn [x] (simple-histogram! "coin" x
                             :size (g/list 600 600)
                             :yrange (g/range 0 1)))))


(defquery many-coins [N]
  (reduce
   +
   (map
    (fn [_]
      (if (sample (flip 0.5))
        1
        0))
    (range coins))))

; = (binomial N 0.5)

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
 ((fn [x] (simple-histogram! "many-coins" x :size (g/list 600 600))))
 )

(doquery (* (binomial 10 0.5)))

(defquery random-coins [N]
  (let [coins
        (sample (uniform-discrete 1 N))]
    (reduce
     +
     (map
      (fn [_]
        (if (sample (flip 0.5))
          1
          0))
      (range coins)))))

(->>
 (doquery :importance random-coins [10])
 (take 10000)
 (stat/collect-results)
 (stat/empirical-distribution)
 (sort-by first)
 ((fn [x] (simple-histogram! "random-coins" x :xrange (g/range 0 10) :size (g/list 844 844)))))

(sample* (poisson 2))

(sample* (categorical {:regular 1
                       :angry 0.5
                       :fsat 0.6}))

(defquery queue []
  (let
    [arrivals (sample (poisson 20))
     customers
     (categorical
      {;; regular
       (normal 1 0.1) 10
       ;; angry
       (normal 5 1) 3
       ;; fast
       (normal 0.3 0.02) 6})]
    (if (> arrivals 0)
      (reduce +
       (map (fn [_] (sample
                     (sample customers)))
            (range arrivals)))
      0)))

(->>
 (doquery :importance queue [])
 (take 1000)
 (stat/collect-results)
 (stat/empirical-distribution)
 (group-by (fn [[k v]] (Math/round k)))
 (map (fn [[k vs]] [k (reduce + (map second vs))]))
 (sort-by first)
 ;;((fn [samples] (comp-histogram samples :normalize :probability)))
 ((fn [x] (simple-histogram! "queue" x :size (g/list 844 844))))
 )


(defquery queue-param []
  (let [hyper (sample (uniform-discrete 1 100))
        arrivals (sample (poisson hyper))
        customers (categorical
                   {;; regular
                    (normal 1 0.1) 10
                    ;; angry
                    (normal 5 1) 3
                    ;; fast
                    (normal 0.3 0.02) 6})
        times (if (> arrivals 0)
                (reduce +
                        (map (fn [_] (sample
                                      (sample customers)))
                             (range arrivals)))
                0)]
    (observe (normal 60 0.001) times)
    hyper))

(->>
 (doquery :importance queue-param [])
 (take 100000)
 (stat/collect-results)
 (stat/empirical-distribution)
 (sort-by first)
 ((fn [x] (simple-histogram! "customers" x :size (g/list 844 844)))))




(def cisco
  ;; https://www.percona.com/docs/wiki/benchmark_cisco_scale_start.html
  ;; :threads :read-only :read-write
  [[1 955.16 562.62]
   [2 1878.91 1258.05]
   [3 2688.01 1890.37]
   [4 3548.68 2503.93]
   [5 4315.54 3068.93]
   [6 5130.43 3533.94]
   [7 5931.37 4038.42]
   [8 6531.08 4446.48]
   [9 7219.8 4830.21]
   [10 7867.61 5181.95]
   [11 8278.71 5460.3]
   [12 8646.7 5746.34]
   [13 9047.84 5996.51]
   [14 9426.55 6119.23]
   [15 9645.37 6261.42]
   [16 9897.24 6391.45]
   [17 10097.6 6532.84]
   [18 10240.5 6645.45]
   [19 10532.39 6795.81]
   [20 10798.52 7013.94]
   [21 11151.43 7177.58]
   [22 11518.63 7205.59]
   [23 11806 7531.35]
   [24 12089.37 7537.07]
   [25 12075.41 7494.07]
   [26 12177.29 7370.29]
   [27 12211.41 7366.76]
   [28 12158.93 7429.72]
   [29 12155.27 7421.76]
   [30 12118.04 7396.23]
   [31 12140.4 7403.5]
   [32 12074.39 7337.66]])


(defn usl [[λ σ κ x]]
  (/ (* λ x)
     (+ 1
        (* σ   (dec x))
        (* κ x (dec x)))))

(sample* (normal 1000 1000))


(with-primitive-procedures [usl]
  (defquery usl-parameters [dataset]
    (let [λ (sample (normal 1000 1000))
          σ (sample (gamma 1 50))
          κ (sample (gamma 1 1500))]
      (reduce (fn [_ [x tput _]]
             (observe (normal (usl [λ σ κ x]) 100) tput))
           dataset)
      (predict :λ λ)
      (predict :σ σ)
      (predict :κ κ)
      (usl [λ σ κ 33]))))

;; ;; q.core> (usl [995.6486 0.02671591 0.0007690945 10])
;; 7602.334280686128
;; (usl [995.6486 0.02671591 0.0007690945 33])

(sample* (normal 0.0008 0.0001))

(assoc {} :x 1)
(->> (range 10)
     (def things)
     (quote)
     (prn))

(->>
   (doquery :palmh usl-parameters [cisco])
   (take 10))

(def usl-samples
  (->>
   (doquery :ipmcmc usl-parameters [cisco])
   (drop 5000)
   ;;(map prn)
   (take 50000)
   (doall)
   ;; (stat/collect-predicts)
   ;;(stat/)
   ;; (stat/empirical-distribution)
              ;;(sort-by first)
              ;;(points-2d! "cisco-33")
              ))

(let [λ (->> (stat/collect-predicts :λ usl-samples)
             (stat/empirical-distribution))
      σ (->> (stat/collect-predicts :σ usl-samples)
             (stat/empirical-distribution))
      κ (->> (stat/collect-predicts :κ usl-samples)
             (stat/empirical-distribution))]
  (pprint (sort-by second λ))
  )


(defn const [x & xs] x)
;; {:log-weight -0.6931471805599453, :result 12692.52046011564, :predicts [[:λ 993.3455307973285] [:σ 0.026833914191818367] [:κ 6.855774919826476E-4]]}
(->>
   (doquery :siman usl-parameters [cisco])
   (map prn)
   (take 1000)
   (doall)
   (const :ok))

(def usl-estimated
  nil)

(usl (concat usl-estimated [10]))



(defn plot-world! [name world & {:keys [size]
                                 :or {size (g/list 1000 1000)}}]
  (let [iw (->> world :initial-world)
        sensor-range (->> iw :sensor-range)
        init-pose (->> iw :robot)
        ;; poses (->> world :observations (map (comp :robot :world)))
        poses (rest
               (reductions
                (fn [[ox oy] [dx dy]] [(+ ox dx) (+ oy dy)])
                [0 0]
                (->> world :observations (map :motion))))
        landmarks (->> iw :landmarks (map second))

        [x-max y-max] (->> iw :bounds)]
    (g/raw-plot!
     [[:set :output (vega/home-png name)]
      [:set :term :png :truecolor :size size :fontscale 1]
      [:set :autoscale]
      [:set :grid]
      [:set :xtics 10]
      [:set :ytics 10]
      [:set :xrange (g/range 0 x-max)]
      [:set :yrange (g/range 0 y-max)]
      [:plot (g/list
              ["-" :title "true-landmarks" :with :points :lw 8]
              ["-" :title "true-poses" :with :linespoints :ls 4 :lc :rgb "black"]
              ["-" :title "sensing" :with :circles :fs :transparent :solid 0.05 :noborder]
              ["-" :title "robot" :with :circles :lw 2])]]
     [landmarks
      poses
      (map #(conj % sensor-range) poses)
      [(concat init-pose [sensor-range])]])))


(->> world :observations
     (dissoc)
     (take 2)
     pprint)

(def world (w/simulate))
(plot-world! "world" world)



(concat (->> world :initial-world :robot)
        [(->> world :initial-world :sensor-range)])

(def magnitude #(mat/magnitude %))
(def distance mat/distance)
(def mat-add mat/add)

(with-primitive-procedures [mat-add distance]
  (defquery localize [{:keys [initial-world observations]}]
    (let [{:keys [bounds landmarks motion-sigma sensor-sigma]} initial-world
          init (map (fn [max] (sample (uniform-continuous 0 max))) bounds)]
      (reduce
       (fn [cur-pose {:keys [control measurements]}]
         (let [new-pose (mat-add cur-pose (sample (mvn control motion-sigma)))]
           (map
            (fn [{:keys [id dist]}]
              (observe
               (normal
                (distance new-pose (second (nth landmarks id)))
                sensor-sigma)
               dist))
            measurements)
           new-pose))
       init observations)
      init)))

(def landmarks (->> world :initial-world :landmarks))

(first (doquery :smc localize [world] :number-of-particles 50))

(def poses (->> world :observations (map (comp :robot :world))))

(observe* (normal 0 5) 0)

(observe* (normal 0 5) (sample* (normal 0 5)))

(prn (first  (->> world :observations)))

(let [sensor-sigma 0.3
      poses-measurements (zip poses (->>
                                     (->> world :observations)
                                     (map :measurements)))]
  (reduce
   (fn [score [new-pose measurements]]
     (+ score (reduce (fn [prescore {:keys [id dist true-dist]}]
                        (let [guessed-dist (distance new-pose (second (nth landmarks id)))
                              score (observe* (normal guessed-dist 0.3) dist)]
                          (prn {:guess guessed-dist :dist dist :diff (- dist guessed-dist) :score score})
                          (+ prescore score))) 0 measurements)))
   0
   poses-measurements))


(->> world
     :observations
     (map :measurements)
     pprint)
(pprint (take 5 ((comp :measurements :observations) world)))

(def samples
  (->>
   (doquery :smc localize [world] :number-of-particles 1)
   (take 25000)
   (partition 100)
   (map-indexed (fn [i batch]
                  (prn :batch i :best (last (sort-by :log-weight samples)))
                  batch))
   (doall)))

(last (sort-by :log-weight samples))

(->> world :initial-world :robot)

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




;; random walk (gaussian noise integrated)

;; https://en.wikipedia.org/wiki/Dead_reckoning

(defn dead-reckon
  "Dead-reckoning: process of integrating the measurements from inertial sensors to obtain position and orientation information"
  []
  (zip (range)
       (reductions + (repeatedly 100 #(sample* (normal 0 1))))))

(let [noises (repeatedly 50 dead-reckon)
      tags (map (fn [_] ["-" :with :lines]) noises)]
  (g/raw-plot!
   [[:set :output (str "/Users/proger/noise.png")]
    [:set :term :png :truecolor :size (g/list 1640 600) :fontscale 1]
    [:set :style :fill :solid 0.6]
    [:set :autoscale]
    [:plot (apply g/list tags)]]
   noises))


(let [noises (repeatedly 50 (comp (partial drop 50) dead-reckon))
      tags (map (fn [_] ["-" :with :lines]) noises)]
  (g/raw-plot!
   [[:set :output (str "/Users/proger/noise-drop50.png")]
    [:set :term :png :truecolor :size (g/list 1640 600) :fontscale 1]
    [:set :style :fill :solid 0.6]
    [:set :autoscale]
    [:plot (apply g/list tags)]]
   noises))


(defn dead-reckon-twice
  []
  (zip (range) (reductions + (reductions + (repeatedly 100 #(sample* (normal 0 1)))))))

(let [noises (repeatedly 50 dead-reckon-twice)
      tags (map (fn [_] ["-" :with :lines]) noises)]
  (g/raw-plot!
   [[:set :output (str "/Users/proger/noise-twice.png")]
    [:set :term :png :truecolor :size (g/list 1200 400) :fontscale 1]
    [:set :style :fill :solid 0.6]
    [:set :autoscale]
    [:plot (apply g/list tags)]]
   noises))


(defquery smooth-guess [xs]
  (sa/reductions
   (fn [old new]
     (observe (normal 0 1) new)
     (+ old new))
   (first xs) (rest xs)))

(defquery smooth-guess [xs]
  (sa/reductions
   (fn [old new]
     (observe (normal 0 1) new)
     (+ old new))
   (first xs) (rest xs)))

(->
 (doquery :smc smooth-guess [(repeatedly 100 #(sample* (normal 0 1)))])
 first)



(repeatedly 50 #(sample* (normal 0 1)))



(def digit '(char 0 1 2 3 4 5 6 7 8 9))
(def dot '(char "."))
(def byte '())

(defn parse-int [number-string]
  (try (Integer/parseInt number-string)
    (catch Exception e nil)))


(defn ip-address? [s]
  (->>
   (string/split s #"\.")
   (map parse-int)
   (every? (fn [x] (and (some? x) (< x 256) (>= x 0))))))

(defn propose []
  (let [digit (uniform-discrete 0 1000)
      ipa (string/join "." (map (fn [_] (str (sample* digit))) (range 4)))]
  ipa))

(get (group-by ip-address? (repeatedly 100000 propose)) true)

(defn uniform [coll]
  (categorical (into {} (map (fn [x] [x 1]) coll))))


(def t '(cat (alt 1 2) (cat 4 (meh 5 6))))

(defn bft
  "breadth-first traversal of a tree"
  [tree]
  (loop [traversed []
         queue (into [] tree)]
    (if-let [[el & queue'] (seq queue)]
      (if (seq? el)
        (let [[root & children] el]
          (recur (conj traversed root)
                 (concat queue' children)))
        (recur (conj traversed el)
               queue'))
      traversed)))

(bft t)

(pop (conj [1 2 3] 4))

(seq? (second  (first (rest t))))

(defn sort-by-second [xs] (sort-by second xs))

(with-primitive-procedures [sort-by-second]
  (defm make-charset [alphabet]
    (let [total (count alphabet)
          ;; sample alphabet without replacement
          n-chars (sample :n-chars (uniform-discrete 1 total))
          sample-probs (map
                        (fn make-sample [i]
                          [i (sample (uniform-continuous 0 1))])
                        (range total))
          sorted (sort-by-second sample-probs)
          chars (map
                 (fn [[i _]] (nth alphabet i))
                 (take n-chars sorted))]
      (list :charset (set chars)))))

(comment
  (charset (make-charset alphabet))
  (charset '(:x ok)))

(defm charset
  [[node & args]]
  (if (= :charset node) (first args) nil))

(defm charset? [[node & _]] (= :charset node))

;; (defn set-union [x y]
;;   (s/union x y))

(def set-union s/union)

(with-primitive-procedures [set-union]
  (defm charset-union [x y]
    (list :charset (set-union (charset x) (charset y)))))


(def alphabet "0123456789.")
(def rules-dist (categorical {:alt 10
                              :concat 30
                              :charset 30
                              :empty 1}))

(defn make-regex-1 [depth]
  (if (> depth 10)
    nil
    (let [rule (sample* rules-dist)]
      (case rule
        :empty nil
        :alt (if-let [a (make-regex (inc depth))]
               (if-let [b (make-regex (inc depth))]
                 (if (and (charset? a) (charset? b))
                   (charset-union a b)
                   (list :alt a b))
                 a)
               nil)
        :concat (if-let [a (make-regex (inc depth))]
                  (if-let [b (make-regex (inc depth))]
                    (list :concat a b)
                    a)
                  nil)
        :charset (make-charset alphabet)))))

(defm make-regex [depth]
  (if (> depth 10)
    nil
    (let [rule (sample rules-dist)]
      (case rule
        :empty nil
        :alt (let [a (make-regex (inc depth))]
               (if a
                 (let [b (make-regex (inc depth))]
                   (if b
                     (if (and (charset? a) (charset? b))
                       (charset-union a b)
                       (list :alt a b))
                     a))
                 nil))
        :concat (let [a (make-regex (inc depth))]
                  (if a
                    (let [b (make-regex (inc depth))]
                      (if b
                        (list :concat a b)
                        a))
                    nil))
        :charset (make-charset alphabet)))))


(def sjoin string/join)
(def sreplace string/replace)

(with-primitive-procedures [sjoin]
  (defm build-regex [[node & args]]
    (case node
      :alt (str "(" (sjoin "|" (map build-regex args)) ")")
      :concat (sjoin (map build-regex args))
      :charset (let [cs (first args)]
                 (if (= (count cs) 1)
                   (apply str cs)
                   (str "[" (apply str cs) "]"))))))

(def mk-re-pattern re-pattern)

(with-primitive-procedures [sreplace mk-re-pattern]
  (defm build-pattern [re-ast]
    (if re-ast
      (mk-re-pattern (sreplace (build-regex re-ast) "." "\\."))
      nil)))

;; making sure all functions work after defm conversion
(take 1 (doquery :importance
                 (query
                  (let [x (make-charset alphabet)
                        y (make-charset alphabet)
                        _ (assert (charset? x))
                        union (charset-union x y)
                        regex (make-regex 0)
                        s (build-pattern regex)]
                    s))
                 nil))

(defn regex-matches? [p x]
  (some? (re-matches p x)))

(with-primitive-procedures [regex-matches?]
  (defquery icanhazipaddress [example]
    (let [re-ast (make-regex 0)
          pattern (build-pattern re-ast)
          ;;_ (prn pattern example (regex-matches? pattern example))
          ]
      (if pattern
        (observe (flip 0.99) (regex-matches? pattern example))
        (observe (flip 1) false))
      pattern)))


(last
 (take 10000
       (doquery :importance
                icanhazipaddress
                ["127.0.0.1"])))
