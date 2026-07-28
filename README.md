# cloud-itonami-translation

Open Business Blueprint (implemented actor): **translation sold as a procured
service — where the hard part is buying inference well, not calling a model.**

**TranslationAdvisor ⊣ TranslationGovernor.** Every other `cloud-itonami-*`
actor sells something it produces. This one's cost of goods is inference it
does not own, bought from whichever seller an [x402](https://x402.nexus)
catalog offers. That makes *procurement* the thing it has to be good at.

Design record:
[ADR-2607289100](https://github.com/com-junkawasaki/root/blob/main/90-docs/adr/2607289100-translation-as-a-procured-service.edn).

## Why this exists

Between 2026-07-27 and 2026-07-28 an operator translated one product's UI into
84 locales with ad-hoc scripts. It worked, eventually. It also produced, in
order:

- a run reported as successful that had delivered **76 of 1,259 keys** for 65
  of 83 locales;
- **seven Arabic-script locales rendering left-to-right**, because the
  direction check was `(= :ur …)` from when Urdu was the only RTL language;
- **28 cells across 19 locales** where a concatenated fragment lost its edge
  space and rendered as `2B메시지…`;
- a near-miss where routing the job through x402 would have cost **~$160 for
  zero extra throughput**, because the seller on offer fronts the same fleet
  the operator already reaches for free.

None of those are translation problems. They are **procurement and acceptance**
problems, and they are what this actor is made of.

## What it does

| namespace | role |
|---|---|
| `translation.order` | orders, delivery tiers, the reach-ordered schedule and its coverage curve |
| `translation.procurement` | x402 `/catalog` → a plan; refuses to buy capacity the operator already has |
| `translation.qa` | acceptance checks and the small set of mechanical repairs |
| `translation.governor` | spend / quality / publication gates; decides what settles |
| `translation.advisor` | the contained intelligence node — proposes, never writes |
| `translation.phase` | 0→3 rollout; spending never auto-commits at any phase |
| `translation.operation` | one request = one supervised StateGraph run |
| `translation.store` | SSoT + append-only ledger, and the two reconciliation queries |
| `translation.edge.worker` | the XRPC/D1 host half — I/O only, no rules |

### Containment

The advisor proposes; it cannot act. Three things it structurally cannot do,
each because the alternative already cost something:

- **name a price** — plans are priced from the catalog offer, and no code path
  reads a price out of a request;
- **accept its own work** — the verdict comes from `translation.qa`, computed
  by the governor;
- **declare capacity** — that is backend-deduplication in procurement, not an
  opinion.

`:plan-procurement` is absent from every phase's auto-commit set **including
phase 3**. Recording an outcome may be automatic; deciding to spend may not.
A rejected delivery is still *recorded* — a supplier who delivered garbage must
be distinguishable from one who was never used — it is simply not payable.

### The API is not new

[`i18n.etzhayyim.com`](https://i18n.etzhayyim.com) already defines the
translation-service contract — `RegisterProject` / `TranslateBatch` /
`WidgetApprove` / `GetLanguageRegistry` — and
[`kotoba-lang/i18n`](https://github.com/kotoba-lang/i18n) already ships
`i18n.tm-import` as the client bridge to it. This actor implements **that**
contract. It does not invent a second one.

## The three things a buyer actually needs

**1. Capacity that is real.** Two sellers only add throughput if they resolve
to different backends. `procurement/distinct-capacity` groups by backend, and
`plan` returns `:no-independent-supply` when a catalog offers nothing but the
operator's own fleet at retail — which is the honest verdict for x402.nexus as
it stands today.

```clojure
(proc/plan catalog {:free-backends #{"murakumo.cloud"} :units 16000})
;; => {:verdict :no-independent-supply :added-capacity 0 :cost-usd 0.0
;;     :self-backends ["murakumo.cloud"]}
```

**2. Delivery that front-loads value.** A 1,259-key × 83-locale job runs for
hours, and under alphabetical ordering the operator has nothing until it
finishes. `:tier/reach` delivers by speaker count, so Spanish, Arabic,
Portuguese, Indonesian and Russian — 1.9 billion speakers — land first. The
total time is identical; the job simply stops being all-or-nothing.

**3. Acceptance that refuses to pay for bad work.** A supplier returning 40%
source-language text has not delivered translations at any price.
`governor/review-batch` marks such a batch `:payable? false`, which is what an
[`engi`](https://github.com/gftdcojp/engi)-countersigned settlement should
decline to honour.

## Governor invariants

- A plan that adds no capacity is **rejected**, however cheap.
- Spend over the auto-approve ceiling **holds for a human** (`interrupt-before`).
- A batch below the accept-rate floor is **not payable**.
- A short catalog is publishable as partial but **is not a fulfilled order** —
  reporting 76 of 1,259 keys as done is the specific mistake this prevents.

## Status

**Implemented**: advisor, governor, phase gate, StateGraph operation,
store + append-only ledger, and the edge worker (XRPC surface over D1).
32 tests / 85 assertions (`clojure -M:test`), clean `clojure -M:lint`,
`npm run build` produces the Worker bundle.

**Not done, and not claimed**:

- **no operational history.** Nothing has run in production. `:maturity
  :implemented` in `blueprint.edn` means the parts exist and are tested — it
  has never meant "has been used".
- **no x402 payment execution.** Procurement decides *whether and from whom*
  to buy; performing the 402 challenge → USDC transfer → settle → receipt
  match is not wired. `record-payment!` exists so the ledger can hold a
  receipt; nothing produces one yet.
- **no dispatcher.** `TranslateBatch` records what a supplier delivered. The
  component that actually calls N providers in parallel is the next increment,
  and it is deliberately separate from acceptance: the thing that judges the
  work must not be the thing that produced it.
- **not deployed.** `wrangler.jsonc` ships `PHASE: "0"` (read-only) and no D1
  binding, so a careless deploy writes nothing.
- **RAD identity not registered.**

## License

AGPL-3.0-or-later.
