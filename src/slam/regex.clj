

(ns slam.regex
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
            [gorilla-plot.core :as plot]))

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
        :alt (if-let [a (make-regex-1 (inc depth))]
               (if-let [b (make-regex-1 (inc depth))]
                 (if (and (charset? a) (charset? b))
                   (charset-union a b)
                   (list :alt a b))
                 a)
               nil)
        :concat (if-let [a (make-regex-1 (inc depth))]
                  (if-let [b (make-regex-1 (inc depth))]
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



(defn match-substring [string sub]
  (let [i-string (partial nth string)
        j-sub (partial nth sub)
        string-len (count string)
        sub-len (count sub)]
    (loop [i 0 j 0 loc 0]
      (cond
        (or (>= i string-len) (>= j sub-len)) (if (>= loc string-len) -1 loc)
        (= (i-string i) (j-sub j)) (recur (inc i) (inc j) (if (> j 0) loc i))
        :backtrack (recur (inc loc) 0 (inc loc))))))

(match-substring "aaaaaaab" "aab")

(lcs "aababaaaaaab" "aab")

(def true-strings
  (-> (slurp "/Users/proger/Desktop/url-classes/true.txt")
      (string/split-lines)))

(def false-strings
  (-> (slurp "/Users/proger/Desktop/url-classes/false.txt")
      (string/split-lines)))

(lcs (first true-strings) (second true-strings))

(defn longest [xs ys]
  (if (> (count xs) (count ys)) xs ys))

(def lcs
  (memoize
   (fn [[x & xs] [y & ys]]
     (cond
      (or (= x nil) (= y nil)) nil
      (= x y) (cons x (lcs xs ys))
      :else (longest (lcs (cons x xs) ys)
                     (lcs xs (cons y ys)))))))





(defn url-tokenize [s] (drop 1 (string/split s #"[/\-_\.]")))

(defn add-to-trie [trie x]
  (assoc-in trie x (merge (get-in trie x) {:val x})))

(defn build-trie [coll]
  "Builds a trie over the values in the specified seq coll."
  (reduce add-to-trie {} coll))

(def brillen-false
  (->> (slurp "/Users/proger/Desktop/url-classes/brillen-false.txt")
       (string/split-lines)
       (map url-tokenize)))

(def brillen-true
  (->> (slurp "/Users/proger/Desktop/url-classes/brillen-true.txt")
       (string/split-lines)
       (map url-tokenize)))


(def bftrie (build-trie brillen-false))

(def fkeys
  (-> bftrie
      (get "brillen")
      (keys)))

(def tkeys
  (-> (build-trie brillen-true)
      (get "brillen")
      (keys)))

(s/intersection (set tkeys) (set fkeys))

(->> (get-in (build-trie brillen-true) ["brillen" "longchamp" "lo" "2605"])
     (tree-seq map? vals)
     (keep :val)
)

(->> (get-in (build-trie brillen-false) ["brillen" "longchamp" "lo" "2605"])
     (tree-seq map? vals)
     (keep :val)
)


(get bftrie "brillen")


(defn zip [& args] (apply map vector args))

(defn sum [xs] (reduce + xs))

(defn mean [xs]
  (/ (sum xs) (float (count xs))))

(defn sqr [x] (* x x))

(defn mean-ssd-count
  "welford's algo: numerically stable sample mean, ssd (sum (map #(- % (mean xs)) xs)), count"
  ;; variance = (/ ssd (dec count))
  ;; http://jonisalonen.com/2013/deriving-welfords-method-for-computing-variance/
  [xs]
  (reduce
   (fn [[m s i] x]
     (let [k (inc i)
           m' (+ m (/ (- x m) k))
           s' (+ s (* (- x m) (- x m')))]
       [m' s' k]))
   [0 0 0]
   xs))

   ;; meanx = meany = C = n = 0
   ;; for x, y in zip(data1, data2):
   ;;     n += 1
   ;;     dx = x - meanx
   ;;     meanx += dx / n
   ;;     meany += (y - meany) / n
   ;;     C += dx * (y - meany)
   ;;  sample_covar = C / (n - 1)

(defn covariance
  "online sum of multiplied differences: (sum (map (fn [x y] (* (- x (mean xs)) (- y (mean ys)))) xs ys)).
  returns [x-mean y-mean smd count], covariance is (/ smd (dec count))."
  [xs ys]
  (reduce
   (fn [[xm ym cov i] [x y]]
     (let [n (inc i)
           cx (- x xm)
           cy (- y ym)
           xm' (+ xm (/ cx n))
           ym' (+ ym (/ cy n))
           cov' (+ cov (* cx cy))]
       [xm' ym' cov' n]))
   [0 0 0 0]
   (map vector xs ys)))

(defn covariance' [xs ys]
  (/ (sum (map (fn [x y] (* (- x (mean xs)) (- y (mean ys)))) xs ys))
     (dec (count xs))))

(let [xs [1 2 3 4 5 6]
      cov (covariance' xs xs)]
  (/ cov
     (Math/sqrt (* cov cov))))

(defn correlation [xs ys]
  (let [[_ x-ssd _] (mean-ssd-count xs)
        [_ y-ssd _] (mean-ssd-count ys)
        [_ _ smd n] (covariance xs ys)]
    (/ smd
       (Math/sqrt (* x-ssd y-ssd)))))

(mean-ssd-count (repeatedly 1000 #(sample* (normal 0 1000))))

(let [xs (repeatedly 100 #(sample* (normal 0 1000)))
      ys (repeatedly 100 #(sample* (normal 0 1000)))]
  (correlation xs ys))

(let [xs [1 2 3 4 5 6]
      ys [6 5 4 3 2 1]]
  (correlation xs ys))


(let [c [0.1 0.2 0.1 0.1 0.1 0.1]
      b [0.1 0.2 0.3 0.4 0.5 0.6]
      a [1 2 3 4 5 6]]
  {:dot (mat/dot a a)
   :cos (/ (mat/dot a a) (* (Math/sqrt (mat/dot a a)) (Math/sqrt (mat/dot a a))))
   :cos1 (/ (mat/dot a c) (Math/sqrt (* (mat/dot a a) (mat/dot c c))))
   })
