(ns slam.anglican
  (:refer-clojure :exclude [reductions])
  (:require [anglican.emit :as ae :refer [defquery defm with-primitive-procedures]]))

(defm $reductions
  [f init lst]
  (cons init (let [s (seq lst)]
               (if (some? s)
                 ($reductions f (f init (first s)) (rest s))
                 nil))))

(def reductions $reductions)
