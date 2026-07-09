# pay

**On-chain creator-payment rail primitives (USDC on Base L2) — pure
Clojure/ClojureScript (`.cljc`), zero network I/O, zero key custody.**

Re-homed from `@etzhayyim/sdk`'s payment surface (`pay.ts`: `pay()` /
`payStream()` / `splitDistribute()` / escrow) as a **vendor-neutral
kotoba-lang common library** — superproject ADR-2607092700. The move
resolves a structural mismatch: the etzhayyim substrate's payment-purpose
doctrine prohibits external `tip` / `subscription` / `purchase`, which is
exactly what vendor creator platforms (jk-luxury `club-shinshi`,
`net-babiniku`) need to settle. Vendor apps now consume this library under
their own operating entity (JK株式会社 / "Shinshi Inc."); etzhayyim keeps
its donation-only rail on its own side of the substrate axis.

## What it provides (pure, tested)

- **USDC units** — `parse-usdc` / `format-usdc` (6-decimal base units,
  truncating parse matching the original `parseUsdc`).
- **Streaming flow rates** — `flow-rate-per-second "10.00 / month"`
  (Superfluid-shaped, adapter-agnostic).
- **Revenue splits** — `split-allocations` / `creator-platform-split`
  (the 80/20 creator take; rounding dust goes to the creator; totals are
  invariant).
- **Monetization capability** — the signed / scoped / expiring / revocable
  payment-authorization shape from ADR-2607071000
  (`net.babiniku.monetization.capability`), generalized: `capability-errors`,
  `valid-capability?`, `expired?`.
- **Receipts + entitlement** — `->receipt` (bigint-safe string amounts on
  the wire) and `entitle`, the **verify-before-honor** decision: an
  entitlement is granted only when a host-supplied on-chain verification
  confirms the settlement. Never grant on an unverified claim — the exact
  gap (`verify()` unimplemented) that kept the original SDK's monetization
  HARD-held.
- **`PayRail` protocol** — the injected seam for every on-chain effect,
  with `unprovisioned-rail` as the honest default (every operation HOLDs
  with `:pay/unprovisioned-capability`; never fake a settlement).

## What it deliberately does NOT do

- No chain RPC, no PDS writes, no HTTP — hosts back `PayRail` with real
  adapters: [kotoba-lang/base-l2](https://github.com/kotoba-lang/base-l2)
  (JSON-RPC + ERC-4337 sponsored writes),
  [kotoba-lang/treasury](https://github.com/kotoba-lang/treasury)
  (on-chain USDC verification + append-only ledger),
  [kotoba-lang/wallet](https://github.com/kotoba-lang/wallet)
  (non-custodial signing). Superfluid / 0xSplits / Safe-escrow adapters are
  follow-ups; until then those rail methods HOLD honestly.
- No private keys. Signing stays on the payer's device (passkey smart
  wallet or non-custodial wallet) — a platform-held signing key is
  prohibited by design, same as the original SDK's rule.
- No fiat. Fiat rails (ISO 8583 / ISO 20022) live in
  [kotoba-lang/kessai](https://github.com/kotoba-lang/kessai) — a sibling,
  not a dependency. A consuming app can run crypto + fiat dual-rail by
  composing both.

## x402 (HTTP 402) protocol — `pay.x402`

The `pay.x402` namespace implements the **x402** open micropayment protocol
that Cloudflare's [Monetization Gateway](https://blog.cloudflare.com/monetization-gateway/)
standardizes — in-band `402 Payment Required`, price + asset + pay-to in the
response, buyer pays a stablecoin and re-requests with an `X-PAYMENT` header,
facilitator verifies, resource served with an `X-PAYMENT-RESPONSE` receipt. No
redirect, no checkout, no seller onboarding — and it works for autonomous
**agent** buyers, not just human wallets.

We are our **own facilitator** (via kotoba-lang/treasury's on-chain verify)
instead of a closed vendor service, so this is not gated on any waitlist and
settles on Base L2 / USDC we already control. Pure protocol codec, same
zero-dep / zero-I/O / zero-custody invariants as the rest of the lib:

- `payment-requirements` / `challenge` — build the 402 body (price via
  `parse-usdc`, micros on the wire).
- `encode-header` / `decode-header` — portable UTF-8 base64 for the
  `X-PAYMENT` / `X-PAYMENT-RESPONSE` envelopes (JSON stays with the host to
  keep zero-dep).
- `payload-errors` / `acceptable?` — pure validation of a decoded payment
  against the requirements (scheme / network / recipient / amount / expiry) for
  both the canonical `exact` EIP-3009 scheme and a `transaction` (tx-hash)
  scheme that maps 1:1 onto the existing treasury/verify path.
- `authorize` — the verify-before-serve decision (the x402 form of `entitle`):
  serve only when the host confirms on-chain settlement, else hold at 402.

Design: superproject ADR-2607093100.

## Facilitator gateway — `pay.facilitator`

`pay.facilitator` is the pure brain of a central x402 **facilitator/gateway**
service (deployed as `gftdcojp/nexus-x402`, superproject ADR-2607093300). Where
`pay.x402` is the wire codec, this is the multi-seller **rules engine** + the
standard x402 facilitator decisions:

- **Rules engine** — a seller registry (`{:seller :method :path-prefix :usd
  :pay-to :chain :scheme}`, first structural match wins). Many sellers
  (shinshi / murakumo / kotobase, each with its OWN treasury) share one
  facilitator that holds no keys: `match-rule`, `rule->requirements`.
- **`verify`** — the x402 `/verify` decision (`{:isValid :invalidReason
  :payer}`): pure shape/economic checks + the host's on-chain verdict.
- **`settle`** — the x402 `/settle` decision (reuses `authorize`).
- **`gate`** — the end-to-end gateway decision (`:pass` / `:challenge` /
  `:serve` / `:hold`), transport-independent.
- **`discovery`** — the `/.well-known/x402` facilitator discovery document.

All pure — the host (nexus-x402) injects on-chain verify/settle via
kotoba-lang/treasury + base-l2. This lets sellers delegate verification to one
edge-hosted facilitator instead of each vendoring the whole gate.

## Consumers

- `jk-luxury/club-shinshi` — creator subscription / PPV / tip (the
  `:hyp/club-shinshi-creator-take` gate: creator GMV > ExoClick ad
  revenue).
- `jk-luxury/net-babiniku` — character subscribe / tip / PPV
  (ADR-2607062200's proposal shapes map 1:1 onto the capability here).

## Dev

```bash
clojure -M:test    # cognitect test-runner
clojure -M:lint    # clj-kondo, errors fail
```

Apache-2.0.
