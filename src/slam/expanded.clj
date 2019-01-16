(query
 (let [x (sample (normal 0 10))] (observe (normal 1 1) x) (predict x)))

(let
 [map
  anglican.emit/$map
  reduce
  anglican.emit/$reduce
  filter
  anglican.emit/$filter
  some
  anglican.emit/$some
  repeatedly
  anglican.emit/$repeatedly
  comp
  anglican.emit/$comp
  partial
  anglican.emit/$partial
  conditional
  anglican.emit/$conditional]
 (clojure.core/with-meta
  (fn
   query27283
   [$value $state]
   (let
    [C27284 #function[anglican.trap/result-cont]]
    (anglican.trap/->sample
     'S27288
     (normal 0 10)
     (fn
      var27287
      [x $state]
      (anglican.trap/->observe
       'O27286
       (normal 1 1)
       x
       (fn
        do27285
        [_ $state]
        (fn [] (C27284 nil (anglican.state/add-predict $state 'x x))))
       $state))
     $state)))
  {:source
   '(query
     (let
      [x (sample (normal 0 10))]
      (observe (normal 1 1) x)
      (predict x)))}))
