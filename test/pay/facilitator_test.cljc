(ns pay.facilitator-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [pay.facilitator :as fac]
            [pay.x402 :as x402]))

(def now 1783000000)

(def rules
  [{:seller "shinshi" :method "GET" :path-prefix "/premium/"
    :usd "0.50" :pay-to "0xShinshiTreasury" :scheme "transaction" :chain "base"}
   {:seller "murakumo" :method "POST" :path-prefix "/v1/messages"
    :usd "0.01" :pay-to "0xMurakumoTreasury" :scheme "transaction" :chain "base"}
   {:seller "kotobase" :method "GET" :path-prefix "/ipfs/"
    :usd "0.001" :pay-to "0xKotobaseTreasury" :scheme "transaction" :chain "base"}])

(deftest rule-validation
  (is (every? fac/valid-rule? rules))
  (is (some #{:facilitator/missing-pay-to}
            (fac/rule-errors {:path-prefix "/x" :usd "1"}))))

(deftest rules-engine
  (testing "first structural match wins, per seller + method + path"
    (is (= "0xShinshiTreasury"
           (:pay-to (fac/match-rule rules {:seller "shinshi" :method "GET" :path "/premium/scene-1"}))))
    (is (= "0xMurakumoTreasury"
           (:pay-to (fac/match-rule rules {:seller "murakumo" :method "POST" :path "/v1/messages"}))))
    (is (nil? (fac/match-rule rules {:seller "shinshi" :method "GET" :path "/free/x"})))
    (is (nil? (fac/match-rule rules {:seller "shinshi" :method "POST" :path "/premium/x"}))))
  (testing "rule → x402 requirements"
    (let [r (fac/rule->requirements (first rules) "/premium/scene-1")]
      (is (= "500000" (:maxAmountRequired r)))
      (is (= "0xShinshiTreasury" (:payTo r)))
      (is (= "base" (:network r))))))

(def payment
  {:x402Version 1 :scheme "transaction" :network "base"
   :payload {:txHash "0xabc" :from "0xAgent"}})
(def reqs (fac/rule->requirements (first rules) "/premium/scene-1"))

(deftest verify-decision
  (testing "valid + settled → isValid"
    (let [v (fac/verify payment reqs {:included true :payer "0xAgent"} now)]
      (is (:isValid v))
      (is (= "0xAgent" (:payer v)))))
  (testing "unsettled → invalid with reason"
    (let [v (fac/verify payment reqs {:included false :reason :tx-not-found} now)]
      (is (not (:isValid v)))
      (is (str/starts-with? (:invalidReason v) "unsettled"))))
  (testing "malformed payment → invalid before any chain call"
    (let [v (fac/verify (assoc payment :network "ethereum") reqs {:included true} now)]
      (is (not (:isValid v)))
      (is (= "x402/network-mismatch" (:invalidReason v))))))

(deftest gate-decisions
  (testing "no matching rule → pass (ungated)"
    (is (= :pass (:decision (fac/gate rules {:seller "shinshi" :method "GET" :path "/free/x"} nil nil now)))))
  (testing "matched + no payment → challenge 402"
    (let [g (fac/gate rules {:seller "shinshi" :method "GET" :path "/premium/scene-1"} nil nil now)]
      (is (= :challenge (:decision g)))
      (is (= 402 (:status g)))
      (is (= "500000" (:maxAmountRequired (:requirements g))))))
  (testing "matched + verified payment → serve"
    (let [g (fac/gate rules {:seller "shinshi" :method "GET" :path "/premium/scene-1"}
                      payment {:included true :tx "0xabc" :payer "0xAgent"} now)]
      (is (= :serve (:decision g)))
      (is (= "0xabc" (get-in g [:settlement :transaction])))))
  (testing "matched + unverified payment → hold 402"
    (let [g (fac/gate rules {:seller "murakumo" :method "POST" :path "/v1/messages"}
                      (assoc payment :scheme "transaction")
                      {:included false :reason :insufficient-confirmations} now)]
      (is (= :hold (:decision g)))
      (is (= 402 (:status g))))))

(deftest discovery-doc
  (let [d (fac/discovery {:verify-url "https://nexus.gftd.ai/verify"
                          :settle-url "https://nexus.gftd.ai/settle"})]
    (is (= 1 (:x402Version d)))
    (is (= "https://nexus.gftd.ai/verify" (get-in d [:facilitator :verify])))
    (is (= x402/usdc-base (get-in d [:asset :address])))
    (is (some #{"transaction"} (:schemes d)))))
