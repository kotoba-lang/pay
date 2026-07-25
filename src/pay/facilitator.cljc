(ns pay.facilitator
  "x402 facilitator core — the payment-GATEWAY logic as pure Clojure/
  ClojureScript. This is the reusable brain of gftdcojp/nexus-x402 (the
  deployable, edge-hosted x402 facilitator service) and of any seller worker
  that delegates verification instead of vendoring the whole gate.

  Cloudflare's Monetization Gateway is a CENTRAL facilitator: sellers register
  pricing rules and the facilitator verifies payments at the edge. nexus-x402
  is our self-hosted equivalent, and this namespace is its rules engine + the
  standard x402 facilitator decisions (`/verify`, `/settle`) — all pure,
  matching kotoba-lang/pay's invariants (zero-dep, zero network I/O, zero key
  custody). The on-chain settlement/verification I/O is injected by the host
  (nexus-x402 backs it with kotoba-lang/treasury + base-l2); this layer is
  unit-testable without a chain.

  Three pieces:
  1. Rules engine — a seller registry mapping (seller, method, path) to a
     price + pay-to treasury + scheme; first match wins. Lets many sellers
     (shinshi / murakumo / kotobase, each with its OWN treasury) share one
     facilitator without the facilitator holding any keys.
  2. `verify` — the x402 facilitator `/verify` decision: is this payment valid
     for these requirements (pure shape/economic checks + the host's on-chain
     verdict)? Returns {:isValid :invalidReason :payer}.
  3. `settle` / `gate` — the `/settle` decision and the end-to-end gateway
     decision (no X-PAYMENT -> 402 challenge; X-PAYMENT -> verify -> serve or
     hold), reusing pay.x402's challenge/authorize/entitle philosophy."
  (:require [clojure.string :as str]
            [pay.x402 :as x402]))

;; ── rules engine / seller registry ──────────────────────────────────
;; A rule:
;;   {:seller "shinshi"            ; nil = matches any seller
;;    :method "GET"                ; nil = any method
;;    :path-prefix "/premium/"     ; required
;;    :usd "0.50"
;;    :pay-to "0x…"                ; the seller's OWN treasury (no key custody)
;;    :scheme "transaction"        ; or "exact"
;;    :chain "base"
;;    :description "…"}            ; optional
;; Rules are an ordered vector; the first structural match wins.

(defn rule-errors
  "Validation errors for one rule, [] when usable."
  [{:keys [path-prefix usd pay-to]}]
  (cond-> []
    (not (string? path-prefix)) (conj :facilitator/missing-path-prefix)
    (not (string? usd))         (conj :facilitator/missing-usd)
    (not (string? pay-to))      (conj :facilitator/missing-pay-to)))

(defn valid-rule? [rule] (empty? (rule-errors rule)))

(defn match-rule
  "First rule in `rules` matching the request {:seller :method :path}, or nil."
  [rules {:keys [seller method path]}]
  (some (fn [r]
          (when (and (or (nil? (:seller r)) (= (:seller r) seller))
                     (or (nil? (:method r))
                         (= (str/upper-case (:method r))
                            (str/upper-case (or method "GET"))))
                     (string? (:path-prefix r))
                     (str/starts-with? (or path "") (:path-prefix r)))
            r))
        rules))

(defn rule->requirements
  "Turn a matched rule + the concrete resource path into x402 payment
  requirements (pay.x402/payment-requirements)."
  [rule resource]
  (x402/payment-requirements
   {:pay-to (:pay-to rule)
    :usd (:usd rule)
    :scheme (:scheme rule "transaction")
    :network (:chain rule "base")
    :resource resource
    :description (:description rule (str (:seller rule) " " resource))}))

;; ── opening the registry to third-party sellers ─────────────────────
;;
;; Everything above works today with ONE seller: the operator. Opening the same
;; registry to third parties is what turns the operator from the counterparty
;; on every flow into the market those flows run through -- the structural
;; change a system-dynamics pass over kotoba-lang/dynamics' archetype catalog
;; ranked third of six for the three-sphere economy (com-junkawasaki/root
;; adr-ledger seq 66, 2026-07-25), grounded in the observation that Visa earns
;; on $17T of OTHER PEOPLE'S transactions and that ERC-20's real leverage was
;; making every subsequent token project free demand surface for Ethereum.
;;
;; Two things have to be true before that is safe, and one of them is a live
;; hijack vector in the code above:
;;
;; 1. `match-rule` treats `:seller nil` as "matches ANY seller". That is a
;;    convenience in a single-tenant registry and an authorization hole the
;;    moment anyone else can add a rule: nexus-x402 routes
;;    `/gateway/<seller>/<path>`, so one wildcard rule would collect payment
;;    for every seller's namespace. `open-registry-errors` rejects it.
;; 2. Within a seller's own namespace, two rules whose prefixes shadow each
;;    other silently make the second unreachable (first match wins). That is a
;;    seller's own problem, but it should be reported at registration rather
;;    than discovered as missing revenue.
;;
;; What this section does NOT do, stated plainly: it does not collect a
;; protocol fee on-chain. It cannot. This facilitator holds zero keys by
;; design, payment goes buyer -> seller's own treasury directly, and x402's
;; requirements carry ONE `pay-to`. Splitting a payment atomically would need a
;; splitter contract that is not deployed and whose deployment is a separate,
;; higher-stakes decision. So the fee is COMPUTED AND RECORDED here, and
;; collection is left explicitly unbuilt -- the same record / custody /
;; governance separation ADR-2607995000 §2 already applies to the treasury
;; (the ledger records, the custodian holds, governance authorizes; no layer
;; does another layer's job).

(def reserved-seller-names
  "Seller ids a third party may not claim: they either name the operator's own
  surfaces or would read as one in a catalog."
  #{"operator" "facilitator" "nexus" "admin" "system" "x402" "well-known"})

(defn- seller-id-errors [seller]
  (cond-> []
    (not (string? seller))                    (conj :facilitator/missing-seller)
    (and (string? seller) (str/blank? seller)) (conj :facilitator/blank-seller)
    (and (string? seller)
         (contains? reserved-seller-names (str/lower-case seller)))
    (conj :facilitator/reserved-seller)
    (and (string? seller) (not (re-matches #"[a-z0-9][a-z0-9-]{0,62}" seller)))
    (conj :facilitator/malformed-seller)))

(defn open-registry-rule-errors
  "Validation errors for a rule submitted by a THIRD PARTY, [] when usable.

  Strictly stronger than `rule-errors`: everything that function checks, plus
  the constraints that only matter once the registry is open. Existing
  single-tenant callers keep using `rule-errors` and are unaffected."
  [{:keys [seller path-prefix] :as rule}]
  (into (rule-errors rule)
        (cond-> (seller-id-errors seller)
          ;; the hijack vector: a nil seller matches every namespace
          (nil? seller)
          (conj :facilitator/wildcard-seller-forbidden)

          (and (string? path-prefix) (not (str/starts-with? path-prefix "/")))
          (conj :facilitator/path-prefix-must-be-absolute)

          ;; "/" would swallow the seller's whole namespace including any future
          ;; rule; almost always a mistake, never necessary
          (= path-prefix "/")
          (conj :facilitator/path-prefix-too-broad))))

(defn shadowed-rules
  "Rules in `rules` that can never match because an EARLIER rule for the same
  seller (or a wildcard-seller rule) already covers their prefix and method.
  Returns [{:index i :rule r :shadowed-by-index j}], empty when clean.

  First-match-wins is the intended semantics, not a bug -- but a seller who
  registers a specific price after a general one gets silently underpaid, and
  that is worth reporting at registration time rather than at reconciliation."
  [rules]
  (let [v (vec rules)
        covers? (fn [a b]
                  (and (or (nil? (:seller a)) (= (:seller a) (:seller b)))
                       (or (nil? (:method a))
                           (= (str/upper-case (or (:method a) ""))
                              (str/upper-case (or (:method b) "GET"))))
                       (string? (:path-prefix a)) (string? (:path-prefix b))
                       (str/starts-with? (:path-prefix b) (:path-prefix a))))]
    (vec (for [j (range (count v))
               :let [earlier (first (keep-indexed
                                     (fn [i a] (when (and (< i j) (covers? a (nth v j))) i))
                                     v))]
               :when earlier]
           {:index j :rule (nth v j) :shadowed-by-index earlier}))))

(defn register-seller
  "Admit `new-rules` from one third-party seller into `rules`, or refuse.

  Returns {:admitted? true  :rules <new registry> :warnings [...]}
       or {:admitted? false :errors [...]}.

  Refusal (hard, never partial -- a half-registered seller is worse than an
  unregistered one):
    - any rule fails `open-registry-rule-errors`
    - a rule names a seller other than `seller` (no registering on someone
      else's behalf)
    - a rule's prefix is already covered by an EXISTING rule belonging to a
      DIFFERENT seller (cross-seller collision -- would route this seller's
      traffic to someone else's treasury, or vice versa)

  Warnings (admitted, but reported): rules shadowed within this seller's own
  set. Appended at the END of the registry: an existing seller's rules always
  keep priority over a newcomer's, so registration can never re-route traffic
  that already had a home."
  [rules seller new-rules]
  (let [errs (into [] (mapcat #(open-registry-rule-errors (assoc % :seller (:seller % seller)))) new-rules)
        wrong-seller (remove #(= seller (:seller % seller)) new-rules)
        combined (into (vec rules) (map #(assoc % :seller (:seller % seller))) new-rules)
        n-existing (count rules)
        cross (->> (shadowed-rules combined)
                   (filter (fn [{:keys [index shadowed-by-index]}]
                             (and (>= index n-existing)
                                  (< shadowed-by-index n-existing)
                                  (not= seller (:seller (nth combined shadowed-by-index))))))
                   vec)
        all-errors (cond-> (vec (distinct errs))
                     (seq wrong-seller) (conj :facilitator/seller-mismatch)
                     (seq cross) (conj :facilitator/cross-seller-collision))]
    (if (seq all-errors)
      {:admitted? false :errors all-errors :collisions cross}
      {:admitted? true
       :rules combined
       :warnings (->> (shadowed-rules combined)
                      (filter #(>= (:index %) n-existing))
                      vec)})))

;; ── protocol fee: computed and recorded, NOT collected ───────────────

(def default-protocol-fee-bps
  "Basis points the facilitator records against a third-party settlement.
  500 bps = 5%, matching the cut ADR-2607995000's membrane table already
  applies to fiat/USDC -> credits mint, so the economy has ONE fee number
  rather than a second one invented here."
  500)

(defn- parse-micros
  "`:maxAmountRequired` is USDC micros as a decimal STRING (pay.x402's
  bigint-safe wire convention). Parsed as an integer -- never a float, so fee
  arithmetic stays exact at the unit the chain actually settles in."
  [raw]
  (when (and (string? raw) (re-matches #"\d+" raw))
    #?(:clj (Long/parseLong raw) :cljs (js/parseInt raw 10))))

(defn protocol-fee
  "Fee accounting for one settled payment, in USDC MICROS -- exact integer
  arithmetic on `:maxAmountRequired`, floored, never floating point. Returns
  :amount-micros nil (not 0) when the requirements carry no parseable amount,
  so an unparseable price can never be recorded as a zero fee.

  `:collectible?` is ALWAYS false and carries its reason in-band. This
  facilitator holds no keys, x402 requirements carry exactly one `payTo`, and
  no splitter contract is deployed -- so this is a RECORD of what is owed, not
  a transfer. Treating a non-nil :amount-micros as revenue received would be
  wrong, which is why the flag is a value rather than a comment."
  ([requirements] (protocol-fee requirements default-protocol-fee-bps))
  ([requirements bps]
   (let [micros (parse-micros (:maxAmountRequired requirements))]
     {:bps bps
      :gross-micros micros
      :amount-micros (when micros (quot (* micros bps) 10000))
      :collectible? false
      :why "facilitator holds no keys and x402 requirements carry a single
            payTo; collection needs a splitter contract that is not deployed.
            Recorded per ADR-2607995000 §2's record/custody/governance split --
            this is the record layer only."})))

(defn settlement-record
  "What a host appends to its ledger for one settled third-party payment.
  Carries the seller deliberately: it makes ACCEPTANCE DENSITY -- how many
  distinct sellers have ever settled -- answerable from the ledger instead of
  by assertion. That count is 1 today (the operator), and the three-sphere
  growth analysis identified it as the binding constraint on the whole
  economy."
  [{:keys [seller settlement requirements payer now-epoch]}]
  {:seller seller
   :payer payer
   :settlement settlement
   :resource (:resource requirements)
   :amount-micros (parse-micros (:maxAmountRequired requirements))
   :pay-to (:payTo requirements)
   :fee (protocol-fee requirements)
   :at now-epoch})

;; ── /verify (x402 facilitator interface) ────────────────────────────
;; `onchain-verdict` is host-supplied: {:included bool :reason … :payer …} —
;; nexus backs it with treasury/verify-payment (transaction scheme) or an
;; EIP-3009 submission result (exact scheme). Pure decision here.

(defn verify
  "The x402 `/verify` response for a decoded payment against requirements at
  `now-epoch`, given the host's on-chain verdict. Shape/economic checks run
  first (no chain call needed to reject a malformed/underpaid payment)."
  [payment requirements onchain-verdict now-epoch]
  (let [errs (x402/payload-errors payment requirements now-epoch)]
    (cond
      (seq errs)
      {:isValid false :invalidReason (subs (str (first errs)) 1) :errors errs}

      (not (:included onchain-verdict))
      {:isValid false
       :invalidReason (str "unsettled: " (some-> (:reason onchain-verdict) name))}

      :else
      {:isValid true
       :payer (or (:payer onchain-verdict)
                  (get-in payment [:payload :from])
                  (get-in payment [:payload :authorization :from]))})))

;; ── /settle (x402 facilitator interface) ────────────────────────────

(defn settle
  "The x402 `/settle` decision — reuses pay.x402/authorize. Returns the
  authorize map ({:authorized? …} with :settlement or :status 402)."
  [payment requirements onchain-verdict now-epoch]
  (x402/authorize payment requirements onchain-verdict now-epoch))

;; ── gate (end-to-end gateway decision) ──────────────────────────────
;; The full facilitator/gateway step, independent of transport. The host:
;;  - resolves the seller + path,
;;  - if there is a matching rule and NO decoded payment -> emit :challenge,
;;  - if a payment is present -> the host must have run its on-chain verdict,
;;    then this returns :serve (authorized) or :hold (402), or :pass (no rule).

(defn gate
  "Resolve a gateway decision. Args:
    rules   — the seller registry (ordered vector)
    req     — {:seller :method :path}
    payment — decoded X-PAYMENT payload map, or nil
    onchain-verdict — host verdict map, or nil when payment is nil
    now-epoch
  Returns one of:
    {:decision :pass}                                  ; no rule → not gated
    {:decision :challenge :status 402 :requirements r} ; needs payment
    {:decision :serve :settlement s :requirements r}   ; paid & verified
    {:decision :hold :status 402 :reason kw :requirements r}"
  [rules {:keys [path] :as req} payment onchain-verdict now-epoch]
  (if-let [rule (match-rule rules req)]
    (let [reqs (rule->requirements rule path)]
      (if (nil? payment)
        {:decision :challenge :status 402 :requirements reqs}
        (let [d (settle payment reqs onchain-verdict now-epoch)]
          (if (:authorized? d)
            {:decision :serve :settlement (:settlement d) :requirements reqs}
            {:decision :hold :status 402 :reason (:reason d) :requirements reqs}))))
    {:decision :pass}))

;; ── facilitator discovery (/.well-known/x402) ───────────────────────

(defn discovery
  "A facilitator discovery document listing the schemes/networks this
  facilitator supports and its endpoints. Served at /.well-known/x402."
  [{:keys [verify-url settle-url]}]
  {:x402Version x402/x402-version
   :facilitator {:verify verify-url :settle settle-url}
   :schemes ["transaction" "exact"]
   :networks ["base"]
   :asset {:symbol "USDC" :address x402/usdc-base :network "base" :decimals 6}})
