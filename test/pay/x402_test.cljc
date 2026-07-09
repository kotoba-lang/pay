(ns pay.x402-test
  (:require [clojure.test :refer [deftest is testing]]
            [pay.x402 :as x402]))

(def now 1783000000) ;; fixed epoch seconds for deterministic expiry tests

(def reqs
  (x402/payment-requirements
   {:pay-to "0xTreasurySafe" :usd "0.01"
    :resource "https://kotobase.net/api/premium/x"
    :description "premium API call"}))

(deftest requirements-shape
  (is (= "exact" (:scheme reqs)))
  (is (= "base" (:network reqs)))
  (is (= "10000" (:maxAmountRequired reqs)))       ; $0.01 = 10000 micros
  (is (= "0xTreasurySafe" (:payTo reqs)))
  (is (= x402/usdc-base (:asset reqs))))

(deftest challenge-body
  (let [c (x402/challenge reqs)]
    (is (= 1 (:x402Version c)))
    (is (= 1 (count (:accepts c))))
    (is (= "X-PAYMENT header is required" (:error c))))
  (testing "multiple accepted options"
    (is (= 2 (count (:accepts (x402/challenge [reqs reqs])))))))

(deftest header-codec-roundtrip
  (testing "base64 UTF-8 survives round trip (incl. multibyte)"
    (doseq [s ["{\"x402Version\":1}" "日本語 price ¥" "0xdeadBEEF"]]
      (is (= s (x402/decode-header (x402/encode-header s)))))))

(def exact-payment
  {:x402Version 1 :scheme "exact" :network "base"
   :payload {:signature "0xsig"
             :authorization {:from "0xPayer" :to "0xTreasurySafe"
                             :value "10000"
                             :validAfter "0" :validBefore (str (+ now 60))
                             :nonce "0x01"}}})

(deftest exact-scheme-validation
  (testing "a well-formed exact payment passes"
    (is (x402/acceptable? exact-payment reqs now)))
  (testing "wrong recipient rejected"
    (is (some #{:x402/wrong-recipient}
              (x402/payload-errors
               (assoc-in exact-payment [:payload :authorization :to] "0xElse")
               reqs now))))
  (testing "underpaid rejected"
    (is (some #{:x402/underpaid}
              (x402/payload-errors
               (assoc-in exact-payment [:payload :authorization :value] "9999")
               reqs now))))
  (testing "expired authorization rejected"
    (is (some #{:x402/authorization-expired}
              (x402/payload-errors
               (assoc-in exact-payment [:payload :authorization :validBefore] (str (- now 1)))
               reqs now))))
  (testing "not-yet-valid authorization rejected"
    (is (some #{:x402/authorization-not-yet-valid}
              (x402/payload-errors
               (assoc-in exact-payment [:payload :authorization :validAfter] (str (+ now 100)))
               reqs now))))
  (testing "missing signature rejected"
    (is (some #{:x402/missing-signature}
              (x402/payload-errors (update exact-payment :payload dissoc :signature)
                                   reqs now))))
  (testing "scheme / network mismatch"
    (is (some #{:x402/scheme-mismatch}
              (x402/payload-errors (assoc exact-payment :scheme "transaction") reqs now)))
    (is (some #{:x402/network-mismatch}
              (x402/payload-errors (assoc exact-payment :network "ethereum") reqs now)))))

(def tx-reqs (x402/payment-requirements
              {:pay-to "0xTreasurySafe" :usd "0.01" :resource "r" :scheme "transaction"}))

(def tx-payment
  {:x402Version 1 :scheme "transaction" :network "base"
   :payload {:txHash "0xabc" :from "0xPayer"}})

(deftest transaction-scheme-validation
  (is (x402/acceptable? tx-payment tx-reqs now))
  (is (some #{:x402/missing-tx-hash}
            (x402/payload-errors (update tx-payment :payload dissoc :txHash) tx-reqs now))))

(deftest authorize-decision
  (testing "authorized only when host confirms settlement"
    (let [ok (x402/authorize exact-payment reqs
                             {:included true :tx "0xsettled"} now)]
      (is (:authorized? ok))
      (is (= true (get-in ok [:settlement :success])))
      (is (= "0xsettled" (get-in ok [:settlement :transaction])))
      (is (= "0xPayer" (get-in ok [:settlement :payer])))))
  (testing "unverified settlement holds at 402"
    (let [r (x402/authorize exact-payment reqs {:included false :reason "not-anchored"} now)]
      (is (not (:authorized? r)))
      (is (= 402 (:status r)))
      (is (= :x402/settlement-unverified (:reason r)))))
  (testing "invalid payload holds at 402 before any settlement"
    (let [r (x402/authorize (assoc-in exact-payment [:payload :authorization :value] "1")
                            reqs {:included true} now)]
      (is (not (:authorized? r)))
      (is (= :x402/invalid-payment (:reason r)))
      (is (some #{:x402/underpaid} (:errors r))))))
