(ns pay.x402-buyer-test
  (:require [clojure.test :refer [deftest is testing]]
            [pay.x402-buyer :as buyer]))

(def usdc "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913")
(def treasury "0xA00366234D29d4F882088048c0B2fa0dB7302D4E")

(def policy {:max-amount 1000 :schemes #{"exact" "transaction"}
             :networks #{"base"} :assets #{usdc} :pay-tos #{treasury}})

(defn- offer [& {:as m}]
  (merge {:scheme "exact" :network "base" :asset usdc :payTo treasury
          :maxAmountRequired "1000"} m))

(defn- challenge [& os] {:x402Version 1 :accepts (vec os)})

;; ── the one that matters most ───────────────────────────────────────────────

(deftest no-policy-is-not-permission
  (testing "支出権限を渡されていない agent は、何も支出してよくない。
            ここで沈黙を自由と読むのが一番危険な既定値"
    (let [r (buyer/plan (challenge (offer)) nil)]
      (is (not (buyer/payable? r)))
      (is (= :buyer/no-policy (:refuse r))))))

(deftest an-empty-allowlist-allows-nothing
  (testing "「無い」と「空」は別物。空の allowlist は何も許さない、という意味を
            表現できなくなったら policy は policy でなくなる"
    (let [r (buyer/plan (challenge (offer)) (assoc policy :assets #{}))]
      (is (not (buyer/payable? r)))
      (is (= :buyer/asset-not-allowed (:refuse r))))))

(deftest a-cap-is-a-cap
  (let [r (buyer/plan (challenge (offer :maxAmountRequired "1001")) policy)]
    (is (not (buyer/payable? r)))
    (is (= :buyer/over-cap (:refuse r))))
  (testing "ちょうど上限は通る"
    (is (buyer/payable? (buyer/plan (challenge (offer :maxAmountRequired "1000")) policy)))))

;; ── choosing, rather than taking whatever came first ─────────────────────────

(deftest the-cheapest-acceptable-offer-is-taken-not-the-first
  (testing "accepts の順序は seller の選択であって buyer の判断ではない"
    (let [r (buyer/plan (challenge (offer :maxAmountRequired "900")
                                   (offer :maxAmountRequired "100")
                                   (offer :maxAmountRequired "500"))
                        policy)]
      (is (buyer/payable? r))
      (is (= 100 (get-in r [:pay :amount]))))))

(deftest the-choice-does-not-depend-on-the-order-it-arrived-in
  (let [a (offer :maxAmountRequired "100") b (offer :maxAmountRequired "500")]
    (is (= (get-in (buyer/plan (challenge a b) policy) [:pay :amount])
           (get-in (buyer/plan (challenge b a) policy) [:pay :amount])))))

;; ── both protocol versions ───────────────────────────────────────────────────

(deftest v1-and-v2-name-the-amount-differently-and-both-are-read
  (testing "片方しか読まないと、もう片方の version の seller は
            『何も売っていない』のと同じ顔になる"
    (is (= 250 (get-in (buyer/plan (challenge (dissoc (offer :amount "250")
                                                      :maxAmountRequired))
                                   policy) [:pay :amount])))
    (is (= 250 (get-in (buyer/plan (challenge (offer :maxAmountRequired "250"))
                                   policy) [:pay :amount])))))

;; ── refusals stay distinguishable ────────────────────────────────────────────

(deftest each-constraint-refuses-in-its-own-name
  (testing "spend limit が asset address の打ち間違いと同じ顔をしたら、
            どちらも直せない"
    (doseq [[expected o]
            [[:buyer/scheme-not-allowed  (offer :scheme "made-up")]
             [:buyer/network-not-allowed (offer :network "ethereum")]
             [:buyer/asset-not-allowed   (offer :asset "0xdead")]
             [:buyer/payee-not-allowed   (offer :payTo "0xbad")]
             [:buyer/over-cap            (offer :maxAmountRequired "99999")]
             [:buyer/unreadable-amount   (offer :maxAmountRequired "not-a-number")]
             [:buyer/offer-has-no-scheme (dissoc (offer) :scheme)]]]
      (let [r (buyer/plan (challenge o) policy)]
        (is (not (buyer/payable? r)) (pr-str o))
        (is (= expected (:refuse r)) (pr-str o))))))

(deftest an-empty-challenge-is-no-offer-not-a-free-one
  (doseq [c [{} {:accepts []} {:accepts nil}]]
    (let [r (buyer/plan c policy)]
      (is (not (buyer/payable? r)))
      (is (= :buyer/no-offers (:refuse r)) (pr-str c)))))

(deftest a-zero-amount-is-read-but-a-malformed-one-is-not
  (testing "0 は本物の申し出でありうる。読めなかったものを 0 にすると
            壊れた challenge が一番安い申し出になってしまう"
    (is (= 0 (get-in (buyer/plan (challenge (offer :maxAmountRequired "0")) policy)
                     [:pay :amount])))
    (is (= :buyer/unreadable-amount
           (:refuse (buyer/plan (challenge (offer :maxAmountRequired "-5")) policy))))))

;; ── the rejected list is evidence, not debris ────────────────────────────────

(deftest every-offer-not-taken-is-reported-even-on-success
  (testing "5 つのうち唯一の受諾可能な申し出を取ったのか、5 つのうち 1 つを
            取ったのかは、別の事実"
    (let [r (buyer/plan (challenge (offer) (offer :network "ethereum")
                                   (offer :asset "0xdead"))
                        policy)]
      (is (buyer/payable? r))
      (is (= 2 (count (:rejected r))))
      (is (= #{:buyer/network-not-allowed :buyer/asset-not-allowed}
             (set (map :reason (:rejected r))))))))

(deftest nothing-is-signed-here
  (testing "戻り値に鍵・署名・送信の痕跡が無いこと —— この名前空間の契約"
    (let [r (buyer/plan (challenge (offer)) policy)]
      (is (nil? (:signature (:pay r))))
      (is (nil? (:private-key (:pay r))))
      (is (map? (:pay r))))))
