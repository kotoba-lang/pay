(ns run-tests
  "The suite under ClojureScript.

  pay carries the x402 challenge and payment-requirements codec that the Worker answers 402 with.

  This repo had no ClojureScript entry, so the murakumo fleet could only
  gate its JVM half. Counts were measured to match before this was added --
  that measurement, not the `.cljc` extension, is what earns a second gate.
  Measured 2026-08-17 on datom-source: a portable suite can be green on the
  JVM and red under nbb for reasons production does not have (SCI deftype
  behaviour), so `.cljc` alone is not grounds.

      npx nbb --classpath src:test run-tests.cljs"
  (:require [cljs.test :as t]
            [pay.core-test]
            [pay.facilitator-test]
            [pay.x402-test]))

(defmethod t/report [::t/default :end-run-tests] [m]
  (when-not (t/successful? m)
    (js/process.exit 1)))

;; A pattern, not a second list of namespaces to run: a runner that repeats
;; the list can fall behind the suite and report a subset as a pass.
(t/run-all-tests #"^pay\..*-test$")
