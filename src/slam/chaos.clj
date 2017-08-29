;; origin: http://www.robots.ox.ac.uk/~fwood/anglican/examples/viewer/?source=github&user=probprog&repo=bopp&path=worksheets/chaos.clj
(ns slam.chaos
  (:require [anglican.core :as ac :refer [doquery]]
            [anglican.runtime :as ar :refer [uniform-continuous mvn normal]]
            [anglican.emit :as ae :refer [defquery defm with-primitive-procedures]]
            [anglican.stat :as stat]
            ;;[bopp.core :refer :all]
            [bopp.helper-functions :refer [argmax]]
            [clojure.core.matrix :as mat]

            ))
