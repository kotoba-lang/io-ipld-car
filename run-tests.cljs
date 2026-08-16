(ns run-tests
  "The CAR suite on nbb — the runtime a Cloudflare Worker's SCI path resembles.

   Run it with the sibling checkouts on the classpath:

     nbb --classpath \"$(clojure -Spath)\" run-tests.cljs"
  (:require [cljs.test :as t]
            [ipld.car-test]
            [ipld.car.trustless-test]))

(defmethod t/report [:cljs.test/default :end-run-tests] [m]
  (println (str "\nnbb: " (:test m) " tests, " (:pass m) " passed, "
                (:fail m) " failed, " (:error m) " errors"))
  (when (pos? (+ (or (:fail m) 0) (or (:error m) 0)))
    (set! (.-exitCode js/process) 1)))

(t/run-tests 'ipld.car-test 'ipld.car.trustless-test)
