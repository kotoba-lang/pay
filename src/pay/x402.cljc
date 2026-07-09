(ns pay.x402
  "x402 (HTTP 402 Payment Required) protocol codec — pure Clojure/ClojureScript.

  x402 is the open, in-band micropayment protocol Cloudflare's Monetization
  Gateway standardizes (blog.cloudflare.com/monetization-gateway): a gated
  resource answers `402` with its price + accepted asset + pay-to address; the
  buyer (a human wallet OR an autonomous agent) pays in a stablecoin and
  re-requests with an `X-PAYMENT` header carrying proof; a facilitator verifies
  and the resource is served, with an `X-PAYMENT-RESPONSE` settlement receipt —
  no redirect, no checkout page, no seller onboarding.

  This namespace is the PROTOCOL layer only, matching kotoba-lang/pay's
  invariants: pure .cljc, zero network I/O, zero key custody, zero deps. It
  builds the 402 challenge, validates a decoded payment payload against the
  requirements (scheme / network / recipient / amount / expiry — pure), and
  builds the settlement receipt. The on-chain settle/verify itself is delegated
  to the host — we are our OWN facilitator via kotoba-lang/treasury
  (`verify-payment`) instead of a closed vendor service, and the resource-grant
  decision reuses pay.core's verify-before-honor `entitle` philosophy.

  Two schemes are supported:
  - `\"exact\"` — the canonical x402 EVM scheme: the payload carries an EIP-3009
    `transferWithAuthorization` (gasless USDC transfer) the facilitator submits.
    We validate the authorization's recipient/amount/expiry (pure); the host
    submits + confirms it on-chain.
  - `\"transaction\"` — a fallback where the buyer already broadcast the tx and
    the payload carries its hash. This maps 1:1 onto our existing
    treasury/verify-payment + pay.core/entitle path (club-shinshi's claim flow),
    so an agent and a human wallet share one rail."
  (:require [clojure.string :as str]
            [pay.core :as pay]))

(def x402-version 1)

;; USDC on Base L2 (Coinbase Bridged), 6 decimals — same asset kotoba-lang/
;; treasury's `base` chain uses; the default settlement asset.
(def usdc-base "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913")

(defn- parse-int
  [s]
  #?(:clj  (try (Long/parseLong (str s)) (catch Exception _ 0))
     :cljs (let [n (js/parseInt (str s) 10)] (if (js/isNaN n) 0 n))))

(defn- lc [s] (some-> s str str/lower-case))

;; ─── base64 header codec (UTF-8, portable) ──────────────────────────
;; The X-PAYMENT / X-PAYMENT-RESPONSE header values are base64 of a JSON
;; string. JSON (de)serialization stays with the host (JSON.stringify/parse in
;; a cljs Worker; the JVM test injects a string) so the library keeps its
;; zero-dep invariant — this only does the base64 envelope, portably across
;; JVM / browser / Cloudflare Workers / nbb.

(defn encode-header
  "UTF-8 string → base64 (for an already-JSON-serialized payload)."
  [s]
  #?(:clj  (.encodeToString (java.util.Base64/getEncoder)
                            (.getBytes ^String s "UTF-8"))
     :cljs (js/btoa (js/unescape (js/encodeURIComponent s)))))

(defn decode-header
  "base64 → UTF-8 string (the host then JSON-parses it)."
  [b64]
  #?(:clj  (String. (.decode (java.util.Base64/getDecoder) ^String b64) "UTF-8")
     :cljs (js/decodeURIComponent (js/escape (js/atob b64)))))

;; ─── 402 challenge (seller side) ────────────────────────────────────

(defn payment-requirements
  "One accepted payment option for the 402 body. `usd` is priced through
  pay.core/parse-usdc so maxAmountRequired is USDC micros as a string
  (bigint-safe on the wire, matching pay.core's receipt convention)."
  [{:keys [pay-to usd resource description mime-type network asset
           max-timeout-seconds scheme]
    :or {network "base" mime-type "application/json"
         max-timeout-seconds 60 scheme "exact"}}]
  {:scheme scheme
   :network network
   :maxAmountRequired (str (pay/parse-usdc (str usd)))
   :resource resource
   :description (or description "")
   :mimeType mime-type
   :payTo pay-to
   :maxTimeoutSeconds max-timeout-seconds
   :asset (or asset usdc-base)
   :extra {:name "USD Coin" :version "2"}})

(defn challenge
  "The 402 response body: {:x402Version :accepts [reqs…] :error}. Pass one
  requirement map or a seq of them (multiple accepted assets/networks)."
  ([requirements] (challenge requirements "X-PAYMENT header is required"))
  ([requirements error]
   {:x402Version x402-version
    :accepts (if (sequential? requirements) (vec requirements) [requirements])
    :error error}))

;; ─── payment payload validation (facilitator side, pure) ────────────
;; A decoded X-PAYMENT payload:
;;   {:x402Version 1 :scheme "exact" :network "base"
;;    :payload {:signature "0x…"
;;              :authorization {:from :to :value :validAfter :validBefore :nonce}}}
;; or, for the "transaction" scheme:
;;   {:x402Version 1 :scheme "transaction" :network "base"
;;    :payload {:txHash "0x…" :from "0x…"}}

(defn payload-errors
  "Pure structural + economic validation of a decoded payment payload against
  the chosen requirements, at `now-epoch-seconds` (int). [] when acceptable —
  the host then settles/verifies on-chain. Never grants on shape alone."
  [{:keys [scheme network payload] :as _payment} requirements now-epoch-seconds]
  (let [{req-scheme :scheme req-net :network req-pay-to :payTo
         req-amount :maxAmountRequired} requirements
        auth (:authorization payload)]
    (cond-> []
      (not= scheme req-scheme)   (conj :x402/scheme-mismatch)
      (not= network req-net)     (conj :x402/network-mismatch)

      (= scheme "exact")
      (as-> errs
        (cond-> errs
          (not= (lc (:to auth)) (lc req-pay-to))
          (conj :x402/wrong-recipient)

          (< (parse-int (:value auth)) (parse-int req-amount))
          (conj :x402/underpaid)

          (and (:validBefore auth)
               (<= (parse-int (:validBefore auth)) now-epoch-seconds))
          (conj :x402/authorization-expired)

          (and (:validAfter auth)
               (> (parse-int (:validAfter auth)) now-epoch-seconds))
          (conj :x402/authorization-not-yet-valid)

          (not (string? (:signature payload)))
          (conj :x402/missing-signature)))

      (= scheme "transaction")
      (as-> errs
        (cond-> errs
          (not (string? (:txHash payload))) (conj :x402/missing-tx-hash))))))

(defn acceptable?
  [payment requirements now-epoch-seconds]
  (empty? (payload-errors payment requirements now-epoch-seconds)))

;; ─── settlement receipt (X-PAYMENT-RESPONSE) ────────────────────────

(defn settlement-response
  "The X-PAYMENT-RESPONSE body the seller returns once settled."
  [{:keys [success tx network payer]}]
  {:success (boolean success)
   :transaction tx
   :network network
   :payer payer})

;; ─── authorize: the verify-before-serve decision ────────────────────

(defn authorize
  "Tie the pure protocol checks to a host-supplied on-chain `verification`
  ({:included bool :reason … :tx … :payer …}) — for the exact scheme the host
  submits the EIP-3009 authorization and reports inclusion; for the
  transaction scheme the host runs treasury/verify-payment (bridge it with
  pay.core/verification<-treasury). Returns either
    {:authorized? true  :settlement <X-PAYMENT-RESPONSE map>}
  or
    {:authorized? false :status 402 :reason kw :errors [...]}.
  Never serves the resource unless the payment settled — the x402 form of
  pay.core/entitle."
  [payment requirements verification now-epoch-seconds]
  (let [errors (payload-errors payment requirements now-epoch-seconds)]
    (cond
      (seq errors)
      {:authorized? false :status 402 :reason :x402/invalid-payment :errors errors}

      (not (:included verification))
      {:authorized? false :status 402 :reason :x402/settlement-unverified
       :detail (:reason verification)}

      :else
      {:authorized? true
       :settlement (settlement-response
                    {:success true
                     :tx (:tx verification)
                     :network (:network requirements)
                     :payer (or (:payer verification)
                                (get-in payment [:payload :from])
                                (get-in payment [:payload :authorization :from]))})})))
