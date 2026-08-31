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

;; ── the credits rail: a second unit, quoted in the same field ───────────────
;;
;; x402.nexus announces murakumo credits at /.well-known/x402 and
;; `pay.facilitator/credits-requirements` builds the offer. Its `asset` is a
;; MAP, not an address string, because a ledger unit has no contract to name.
;; These are the shape as that function emits it.

(def credits-asset
  {:symbol "CREDITS" :network "murakumo" :redeemable false
   :note "murakumo memory x time credits: labor-issued, NON-redeemable."})

(defn- credits-offer [& {:as m}]
  (merge {:scheme "credits" :network "murakumo" :asset credits-asset
          :payTo "murakumo" :maxAmountRequired "1"} m))

(def two-rail-policy
  {:schemes #{"exact" "transaction" "credits"}
   :networks #{"base" "murakumo"}
   :assets #{usdc "CREDITS"}
   :pay-tos #{treasury "murakumo"}
   :caps {["murakumo" "CREDITS"] 1 ["base" usdc] 1000}})

(deftest a-credits-offer-can-be-read-at-all
  (testing "asset が map で来る offer を、address 文字列と同じ set / sort に
            かけると JVM は throw し cljs は黙って別物を選ぶ。
            比較する前に 1 つの文字列に畳む"
    (is (= "CREDITS" (buyer/asset-id credits-asset)))
    (is (= usdc (buyer/asset-id usdc)))
    (is (nil? (buyer/asset-id 42)))
    (let [r (buyer/plan (challenge (credits-offer))
                        (assoc two-rail-policy :prefer [["murakumo" "CREDITS"]]))]
      (is (buyer/payable? r))
      (is (= 1 (get-in r [:pay :amount])))
      (is (= "CREDITS" (get-in r [:pay :asset]))))))

(deftest an-allowlist-that-names-credits-still-refuses-what-it-does-not-name
  (testing "畳んだ結果が allowlist に無ければ拒否される。
            畳むことは通すことではない"
    (let [r (buyer/plan (challenge (credits-offer))
                        (assoc two-rail-policy :assets #{usdc}))]
      (is (not (buyer/payable? r)))
      (is (= :buyer/asset-not-allowed (:refuse r))))))

;; ── two rails cannot be compared on price ───────────────────────────────────

(deftest offers-on-two-rails-are-not-ranked-by-their-numbers
  (testing "10000 base units の USDC は $0.01、10000 credits は $100。
            どちらも同じ field に同じ文字列で届くので、
            数の大小で選んだ時点で単位を捨てている"
    (let [r (buyer/plan (challenge (offer :maxAmountRequired "1000")
                                   (credits-offer :maxAmountRequired "1"))
                        two-rail-policy)]
      (is (not (buyer/payable? r)))
      (is (= :buyer/incomparable-rails (:refuse r)))
      (is (= #{["base" usdc] ["murakumo" "CREDITS"]} (set (:rails r)))))))

(deftest a-stated-preference-decides-which-rail-and-nothing-else-does
  (let [ch (challenge (offer :maxAmountRequired "1000")
                      (credits-offer :maxAmountRequired "1"))]
    (is (= ["murakumo" "CREDITS"]
           (-> (buyer/plan ch (assoc two-rail-policy
                                     :prefer [["murakumo" "CREDITS"] ["base" usdc]]))
               :pay buyer/rail)))
    (testing "順序を逆にすれば逆の rail が選ばれる —— 金額は動かしていない"
      (is (= ["base" usdc]
             (-> (buyer/plan ch (assoc two-rail-policy
                                       :prefer [["base" usdc] ["murakumo" "CREDITS"]]))
                 :pay buyer/rail))))
    (testing "prefer が実在しない rail しか挙げていなければ、選ばずに拒否する"
      (let [r (buyer/plan ch (assoc two-rail-policy :prefer [["solana" "USDC"]]))]
        (is (= :buyer/incomparable-rails (:refuse r)))))))

(deftest one-rail-still-needs-no-preference
  (testing "rail が 1 本しか残らないなら prefer は要らない。
            この規則が縛るのは選択であって支払いではない"
    (let [r (buyer/plan (challenge (offer :maxAmountRequired "900")
                                   (offer :maxAmountRequired "100"))
                        two-rail-policy)]
      (is (buyer/payable? r))
      (is (= 100 (get-in r [:pay :amount]))))))

;; ── a cap is a quantity in a unit ───────────────────────────────────────────

(deftest a-cap-belongs-to-the-rail-it-caps
  (testing "1 credit の上限を micro-USDC の上限で代用すると、
            $0.01 の cap が 10000 credits = $100 を通す"
    (let [r (buyer/plan (challenge (credits-offer :maxAmountRequired "2"))
                        (assoc two-rail-policy :prefer [["murakumo" "CREDITS"]]))]
      (is (not (buyer/payable? r)))
      (is (= :buyer/over-cap (:refuse r))))
    (testing "rail ごとの cap が無いときだけ :max-amount に落ちる"
      (let [p (-> two-rail-policy (dissoc :caps) (assoc :max-amount 1))
            r (buyer/plan (challenge (credits-offer :maxAmountRequired "2")) p)]
        (is (= :buyer/over-cap (:refuse r)))))))

;; ── building the envelope ───────────────────────────────────────────────────

(deftest a-credits-envelope-is-not-built-for-a-chain-offer
  (testing "scheme が settle の仕方を決める。別の validator が読む形の
            envelope を組めてしまうと、両方を受ける validator が 1 つできる"
    (let [chain (:pay (buyer/plan (challenge (offer)) two-rail-policy))]
      (is (= :buyer/not-a-credits-offer
             (:refuse (buyer/credits-payment chain {:payer "bot" :cacao "c"})))))))

(deftest an-envelope-without-an-authorization-is-not-a-payment
  (let [cr (:pay (buyer/plan (challenge (credits-offer))
                             (assoc two-rail-policy :prefer [["murakumo" "CREDITS"]])))]
    (is (= :buyer/no-authorization (:refuse (buyer/credits-payment cr {:payer "bot"}))))
    (is (= :buyer/no-payer-account (:refuse (buyer/credits-payment cr {:cacao "c"}))))
    (testing "拒否は header にならない —— 払っていない request が
              払ったように見える形を作らない"
      (is (nil? (buyer/payment-header (buyer/credits-payment cr {:payer "bot"})))))
    (testing "揃っていれば、facilitator が読む flat な形になる"
      (let [p (buyer/credits-payment cr {:payer "did:key:zBot" :cacao "eyJ…"})]
        (is (= "credits" (:scheme p)))
        (is (= "murakumo" (:network p)))
        (is (= {:payer "did:key:zBot" :amount 1 :to "murakumo" :cacao "eyJ…"}
               (:payload p)))
        (is (string? (buyer/payment-header p)))))))
