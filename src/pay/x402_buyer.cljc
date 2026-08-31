(ns pay.x402-buyer
  "What a paying agent decides BEFORE it signs anything.

  Everything else in this library is the seller's half: build a challenge,
  validate a payload, decide whether to serve. This is the other side, and it
  is the half where an autonomous agent can do damage, so it is the half that
  is pure.

  ## Zero keys, zero I/O -- deliberately, and this is where it matters most

  `plan` returns what WOULD be signed and paid. It does not sign, does not
  fetch, and does not hold a key. The host supplies a signer (in this
  workspace, `wallet.signer`'s protocol, whose production implementation keeps
  the seed in a kagi compartment) and performs the retry.

  Keeping the decision here means the question *may this agent spend this
  money* is answerable by reading a pure function and running it on a table of
  cases, rather than by reasoning about a network client.

  ## No policy is not permission

  `plan` REFUSES when it is given no spend policy. An agent that has not been
  told what it may spend has not been authorised to spend anything, and the
  most dangerous possible default here is the one that treats silence as
  freedom.

  This is the same shape as the rest of this workspace: a check that could not
  measure must not return what a check that measured and approved returns.

  ## It never widens what it was given

  Every constraint narrows. `plan` cannot pick a network, asset, payee, scheme
  or amount outside the policy, and when several offers are acceptable it takes
  the cheapest with a deterministic tie-break -- never the first that happens to
  parse, because parse order is not a decision anybody made.

  ## Refusals are distinct on purpose

  A buyer that refuses everything must be able to say which constraint bit.
  Collapsing `over-cap` into a generic `not-acceptable` is how a spend limit
  becomes indistinguishable from a typo in an asset address."
  (:require [clojure.string :as str]))

(def ^:private amount-keys
  "x402 v1 says `maxAmountRequired`; v2 says `amount`. Both are base units as a
  string. Reading only one silently refuses every challenge of the other
  version, which looks exactly like a seller offering nothing."
  [:amount :maxAmountRequired])

(defn- blank? [s] (or (nil? s) (and (string? s) (str/blank? s))))

(defn- parse-amount
  "Base units as a non-negative integer, or nil.

  nil rather than 0: an unreadable amount is not a free offer, and returning 0
  would make a malformed challenge the cheapest one on the table."
  [v]
  (cond
    (integer? v) (when-not (neg? v) v)
    (string? v) (when (re-matches #"\d+" (str/trim v))
                  #?(:clj (Long/parseLong (str/trim v))
                     :cljs (js/parseInt (str/trim v) 10)))
    :else nil))

(defn- amount-of [req]
  (some parse-amount (map #(get req %) amount-keys)))

(defn- allowed?
  "A policy set that is absent does not constrain; a policy set that is present
  constrains. Absent is not the same as empty -- an empty allowlist allows
  nothing, and that must stay expressible."
  [s v]
  (or (nil? s) (contains? s v)))

(defn- offers
  "The `accepts` list of a v1 or v2 challenge."
  [challenge]
  (let [a (or (:accepts challenge) (get challenge "accepts"))]
    (when (sequential? a) (vec a))))

(defn- normalize [req]
  {:scheme (or (:scheme req) (get req "scheme"))
   :network (or (:network req) (get req "network"))
   :asset (or (:asset req) (get req "asset"))
   :pay-to (or (:payTo req) (get req "payTo"))
   :amount (amount-of req)
   :resource (or (:resource req) (get req "resource"))
   :raw req})

(defn- rejection
  "Why this one offer cannot be taken, or nil when it can. Order is fixed so a
  reader can predict which reason surfaces when several apply."
  [{:keys [scheme network asset pay-to amount]}
   {:keys [schemes networks assets pay-tos max-amount]}]
  (cond
    (blank? scheme) :buyer/offer-has-no-scheme
    (not (allowed? schemes scheme)) :buyer/scheme-not-allowed
    (blank? network) :buyer/offer-has-no-network
    (not (allowed? networks network)) :buyer/network-not-allowed
    (not (allowed? assets asset)) :buyer/asset-not-allowed
    (not (allowed? pay-tos pay-to)) :buyer/payee-not-allowed
    (nil? amount) :buyer/unreadable-amount
    (and max-amount (> amount max-amount)) :buyer/over-cap
    :else nil))

(defn plan
  "-> `{:pay {...} :rejected [...]}` or `{:refuse reason :rejected [...]}`.

  `challenge` is the decoded 402 body. `policy` is what this agent may spend:

      {:max-amount 1000            ; base units, per call. nil = no cap, and a
                                   ; policy with no cap must say so by omitting
                                   ; the key rather than by being absent
       :schemes  #{\"exact\"}
       :networks #{\"base\"}
       :assets   #{\"0x8335…\"}     ; nil = unconstrained, #{} = nothing allowed
       :pay-tos  #{\"0xA003…\"}}

  `:pay` carries the chosen offer and the amount, which is what the host then
  signs. `:rejected` always lists every offer that was not taken with its
  reason -- including on success, because knowing you took the only acceptable
  offer out of five is different from knowing you took one of five."
  [challenge policy]
  (let [os (offers challenge)]
    (cond
      (nil? policy)
      {:refuse :buyer/no-policy :rejected []}

      (empty? os)
      {:refuse :buyer/no-offers :rejected []}

      :else
      (let [scored (for [o os
                         :let [n (normalize o)]]
                     (assoc n :rejected-because (rejection n policy)))
            ok (remove :rejected-because scored)
            no (filterv :rejected-because scored)
            reasons (mapv (fn [r] {:reason (:rejected-because r)
                                   :scheme (:scheme r) :network (:network r)
                                   :amount (:amount r)})
                          no)]
        (if (empty? ok)
          {:refuse (or (:reason (first reasons)) :buyer/nothing-acceptable)
           :rejected reasons}
          ;; Cheapest, then a stable tie-break. Never "first that parsed":
          ;; the order of `accepts` is the seller's choice, not the buyer's.
          (let [chosen (first (sort-by (juxt :amount :network :asset :scheme) ok))]
            {:pay (dissoc chosen :rejected-because)
             :rejected reasons}))))))

(defn payable? [r] (some? (:pay r)))
