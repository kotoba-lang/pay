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

;; ── opening the registry to third-party sellers (2026-07-25) ─────────────

(deftest wildcard-seller-is-a-hijack-vector-when-the-registry-is-open
  (testing "match-rule's `:seller nil = any seller` is fine single-tenant..."
    (let [wildcard [{:seller nil :method "GET" :path-prefix "/x/"
                     :usd "1.00" :pay-to "0xAttacker"}]]
      (is (some? (fac/match-rule wildcard {:seller "shinshi" :method "GET" :path "/x/a"})))
      (is (some? (fac/match-rule wildcard {:seller "murakumo" :method "GET" :path "/x/a"}))
          "one rule collects for EVERY seller's namespace -- harmless with one
           tenant, an authorization hole the moment anyone can add a rule")
      (testing "...and rule-errors does not catch it, because it was never its job"
        (is (empty? (fac/rule-errors (first wildcard)))))
      (testing "but open-registry-rule-errors does"
        (is (some #{:facilitator/wildcard-seller-forbidden}
                  (fac/open-registry-rule-errors (first wildcard))))))))

(deftest open-registry-rule-errors-is-strictly-stronger-than-rule-errors
  (testing "every rule-errors failure is still reported"
    (let [bad {:seller "acme"}]
      (is (= #{:facilitator/missing-path-prefix :facilitator/missing-usd
               :facilitator/missing-pay-to}
             (set (filter #{:facilitator/missing-path-prefix :facilitator/missing-usd
                            :facilitator/missing-pay-to}
                          (fac/open-registry-rule-errors bad)))))))
  (testing "plus the constraints that only matter once the registry is open"
    (let [base {:usd "1.00" :pay-to "0xAcme" :path-prefix "/api/"}]
      (is (some #{:facilitator/reserved-seller}
                (fac/open-registry-rule-errors (assoc base :seller "facilitator"))))
      (is (some #{:facilitator/reserved-seller}
                (fac/open-registry-rule-errors (assoc base :seller "ADMIN")))
          "reserved names are matched case-insensitively")
      (is (some #{:facilitator/malformed-seller}
                (fac/open-registry-rule-errors (assoc base :seller "Acme Corp"))))
      (is (some #{:facilitator/malformed-seller}
                (fac/open-registry-rule-errors (assoc base :seller "-acme"))))
      (is (some #{:facilitator/path-prefix-must-be-absolute}
                (fac/open-registry-rule-errors (assoc base :seller "acme" :path-prefix "api/"))))
      (is (some #{:facilitator/path-prefix-too-broad}
                (fac/open-registry-rule-errors (assoc base :seller "acme" :path-prefix "/"))))
      (is (empty? (fac/open-registry-rule-errors (assoc base :seller "acme")))))))

(deftest register-seller-admits-a-clean-third-party
  (let [{:keys [admitted? rules warnings]}
        (fac/register-seller rules "acme"
                             [{:method "GET" :path-prefix "/acme/reports/"
                               :usd "0.25" :pay-to "0xAcmeTreasury"}])]
    (is (true? admitted?))
    (is (empty? warnings))
    (is (= "acme" (:seller (last rules))) "the seller id is stamped on, not trusted from input")
    (testing "the new rule actually routes, and only in its own namespace"
      (is (= "0xAcmeTreasury"
             (:pay-to (fac/match-rule rules {:seller "acme" :method "GET"
                                             :path "/acme/reports/q1"}))))
      (is (nil? (fac/match-rule rules {:seller "someone-else" :method "GET"
                                       :path "/acme/reports/q1"}))))
    (testing "and existing sellers are untouched"
      (is (= "0xShinshiTreasury"
             (:pay-to (fac/match-rule rules {:seller "shinshi" :method "GET"
                                             :path "/premium/x"})))))))

(deftest register-seller-refuses-registering-on-someone-elses-behalf
  (let [r (fac/register-seller rules "acme"
                               [{:seller "shinshi" :method "GET" :path-prefix "/premium/"
                                 :usd "0.01" :pay-to "0xAttacker"}])]
    (is (false? (:admitted? r)))
    (is (some #{:facilitator/seller-mismatch} (:errors r)))))

(deftest register-seller-is-all-or-nothing
  (testing "one bad rule refuses the whole submission -- a half-registered seller
            is worse than an unregistered one"
    (let [r (fac/register-seller rules "acme"
                                 [{:method "GET" :path-prefix "/acme/ok/"
                                   :usd "0.25" :pay-to "0xAcme"}
                                  {:method "GET" :path-prefix "/" ; too broad
                                   :usd "0.25" :pay-to "0xAcme"}])]
      (is (false? (:admitted? r)))
      (is (some #{:facilitator/path-prefix-too-broad} (:errors r)))
      (testing "and nothing was added"
        (is (nil? (:rules r)))))))

(deftest existing-sellers-keep-priority-over-newcomers
  (testing "a newcomer whose prefix is already covered by ANOTHER seller's rule is
            refused -- it would otherwise route someone else's traffic"
    (let [wild (conj (vec rules) {:seller nil :method "GET" :path-prefix "/shared/"
                                  :usd "1.00" :pay-to "0xIncumbent"})
          r (fac/register-seller wild "acme"
                                 [{:method "GET" :path-prefix "/shared/thing"
                                   :usd "0.25" :pay-to "0xAcme"}])]
      (is (false? (:admitted? r)))
      (is (some #{:facilitator/cross-seller-collision} (:errors r)))
      (is (seq (:collisions r)))))
  (testing "but a newcomer colliding only with ITSELF is admitted with a warning --
            first-match-wins is intended semantics, silent underpayment is not"
    (let [r (fac/register-seller rules "acme"
                                 [{:method "GET" :path-prefix "/acme/"
                                   :usd "1.00" :pay-to "0xAcme"}
                                  {:method "GET" :path-prefix "/acme/premium/"
                                   :usd "5.00" :pay-to "0xAcme"}])]
      (is (true? (:admitted? r)))
      (is (= 1 (count (:warnings r))))
      (is (= "/acme/premium/" (get-in r [:warnings 0 :rule :path-prefix]))
          "the specific rule is the one that can never fire"))))

(deftest shadowed-rules-reports-every-unreachable-rule
  (is (empty? (fac/shadowed-rules rules)) "the existing registry is clean")
  (let [v [{:seller "a" :path-prefix "/x/"}
           {:seller "a" :path-prefix "/x/y/"}
           {:seller "b" :path-prefix "/x/y/"}]
        s (fac/shadowed-rules v)]
    (is (= [1] (mapv :index s)) "only a's own second rule is shadowed; b is a different namespace")
    (is (= 0 (:shadowed-by-index (first s))))))

;; ── protocol fee: computed and recorded, never collected ─────────────────

(deftest protocol-fee-is-exact-integer-micros
  (let [reqs (x402/payment-requirements {:pay-to "0xAcme" :usd "0.25" :resource "/acme/x"})
        fee (fac/protocol-fee reqs)]
    (is (= 500 (:bps fee)) "one fee number for the whole economy (ADR-2607995000: 5%)")
    (is (= 250000 (:gross-micros fee)) "$0.25 == 250,000 USDC micros")
    (is (= 12500 (:amount-micros fee)) "5% of 250,000 micros, exact integer")
    (is (integer? (:amount-micros fee)) "never a float -- this is the unit the chain settles in"))
  (testing "the fee floors rather than rounding up, so a recorded fee can never
            exceed the actual cut"
    (let [reqs (x402/payment-requirements {:pay-to "0xA" :usd "0.000001" :resource "/x"})]
      (is (= 1 (:gross-micros (fac/protocol-fee reqs))))
      (is (= 0 (:amount-micros (fac/protocol-fee reqs))))))
  (testing "an unparseable amount yields nil, NOT 0 -- a broken price must not be
            recorded as a zero fee"
    (is (nil? (:amount-micros (fac/protocol-fee {:maxAmountRequired "not-a-number"}))))
    (is (nil? (:amount-micros (fac/protocol-fee {})))))
  (testing "a custom bps still uses exact integer arithmetic"
    (is (= 3330 (:amount-micros (fac/protocol-fee {:maxAmountRequired "100000"} 333))))
    (is (= 33 (:amount-micros (fac/protocol-fee {:maxAmountRequired "1000"} 333)))
        "1000 * 333 / 10000 = 33.3, floored to 33 -- never rounded up")))

(deftest protocol-fee-is-never-collectible-and-says-so
  (testing "the facilitator holds no keys and x402 carries one payTo, so the fee is
            a record, not a transfer -- and the flag is a VALUE so a caller cannot
            miss it by not reading a comment"
    (doseq [reqs [(x402/payment-requirements {:pay-to "0xA" :usd "1.00" :resource "/x"})
                  {:maxAmountRequired "1"}
                  {}]]
      (is (false? (:collectible? (fac/protocol-fee reqs))))
      (is (string? (:why (fac/protocol-fee reqs)))))))

(deftest settlement-record-makes-acceptance-density-answerable
  (let [reqs (x402/payment-requirements {:pay-to "0xAcmeTreasury" :usd "0.25"
                                         :resource "/acme/reports/q1"})
        rec (fac/settlement-record {:seller "acme" :settlement {:tx "0xdeadbeef"}
                                    :requirements reqs :payer "0xBuyer" :now-epoch now})]
    (is (= "acme" (:seller rec)) "the seller is on the record, so counting DISTINCT
                                  sellers who ever settled is a ledger query")
    (is (= "0xAcmeTreasury" (:pay-to rec)) "funds go to the seller's own treasury,
                                            never through the facilitator")
    (is (= 250000 (:amount-micros rec)))
    (is (= 12500 (get-in rec [:fee :amount-micros])))
    (is (false? (get-in rec [:fee :collectible?])))
    (is (= now (:at rec)))))

(deftest gate-still-routes-a-registered-third-party-end-to-end
  (testing "registration is not a parallel path -- a newly admitted seller flows
            through the same gate everything else uses"
    (let [{:keys [rules]} (fac/register-seller rules "acme"
                                               [{:method "GET" :path-prefix "/acme/reports/"
                                                 :usd "0.25" :pay-to "0xAcmeTreasury"}])
          req {:seller "acme" :method "GET" :path "/acme/reports/q1"}
          challenge (fac/gate rules req nil nil now)]
      (is (= :challenge (:decision challenge)))
      (is (= 402 (:status challenge)))
      (is (= "0xAcmeTreasury" (get-in challenge [:requirements :payTo]))
          "the 402 tells the buyer to pay the SELLER, not the facilitator")
      (is (= 250000 (:gross-micros (fac/protocol-fee (:requirements challenge))))))))
