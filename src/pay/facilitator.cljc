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
