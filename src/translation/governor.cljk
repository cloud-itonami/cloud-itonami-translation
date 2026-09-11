(ns translation.governor
  "TranslationGovernor — the independent check on everything the advisor
  proposes and every batch a supplier delivers. Pure.

  The actor invariant, in this domain: **the governor never lets a batch be
  paid for, or a catalog be published, that it has not accepted.** An advisor
  (LLM) may propose any procurement plan it likes; the governor decides
  whether it settles.

  Three gates, and each one exists because skipping it cost something real
  during the 84-locale shinshi.club rollout on 2026-07-27/28:

  1. **Spend.** The plan must stay under the order's budget AND must add
     capacity. A plan that buys back the operator's own fleet at $0.01/call is
     refused even when it is affordable — ~$160 for zero extra throughput was
     the concrete near-miss.
  2. **Quality.** A delivered batch is judged by translation.qa. Below the
     accept-rate floor the batch is refused and NOT paid for, because a
     supplier that returns 40% Japanese has not delivered translations at any
     price. The first automated run produced exactly this: 65 of 83 locales
     came back with only the keys they started with, and it was reported as
     success.
  3. **Publication.** A catalog is only publishable when its accepted strings
     cover the keys the app asks for. A partially-filled catalog is not an
     error — it degrades to source language — but it must not be recorded as
     a completed order.

  Every decision returns a reason. `:hold` is the human-in-the-loop state
  (interrupt-before in the StateGraph), not a silent pass."
  (:require [translation.procurement :as proc]
            [translation.qa :as qa]))

(def default-policy
  {:accept-rate-floor 0.95
   ;; backends the operator already reaches directly; buying these adds no
   ;; capacity no matter what the catalog charges
   :free-backends #{"murakumo.cloud" "infer.murakumo.cloud" "api.murakumo.cloud"}
   ;; a plan above this needs a human, however good the arithmetic looks
   :auto-approve-usd 25.0})

(defn review-plan
  "Decide whether a procurement plan may execute.
  → {:decision :commit|:hold|:reject :reason kw :plan plan}"
  [plan {:keys [budget-usd] :as _order} & [policy]]
  (let [{:keys [auto-approve-usd]} (merge default-policy policy)
        cost (:cost-usd plan)]
    (cond
      (= :no-independent-supply (:verdict plan))
      {:decision :reject :reason :no-added-capacity :plan plan}

      (and budget-usd (> cost budget-usd))
      {:decision :reject :reason :over-budget :plan plan}

      (> cost auto-approve-usd)
      {:decision :hold :reason :spend-needs-human :plan plan}

      :else
      {:decision :commit :reason :within-policy :plan plan})))

(defn review-batch
  "Judge a delivered batch before it settles.
  → {:decision :commit|:reject :reason kw :accept-rate double
     :accepted [[k v]…] :rejected [...]}

  A rejected batch is explicitly NOT payable: the receipt this produces is
  what `engi`-countersigned settlement should refuse to honour."
  [locale pairs & [policy]]
  (let [{:keys [accept-rate-floor]} (merge default-policy policy)
        {:keys [accepted rejected accept-rate]} (qa/batch-verdict locale pairs)]
    (if (< accept-rate accept-rate-floor)
      {:decision :reject :reason :below-quality-floor
       :accept-rate accept-rate :accepted accepted :rejected rejected
       :payable? false}
      {:decision :commit :reason :quality-ok
       :accept-rate accept-rate :accepted accepted :rejected rejected
       :payable? true})))

(defn review-publication
  "May this locale's catalog be published as a completed order?
  A short catalog is publishable ONLY as partial — it must not be recorded as
  fulfilled, which is precisely the mistake of reporting a run that produced
  76 of 1,259 keys as done."
  [{:keys [required-keys catalog]}]
  (let [have (set (keys catalog))
        missing (remove have required-keys)]
    (cond
      (empty? missing) {:decision :commit :reason :complete :missing 0}
      (< (count missing) (* 0.05 (count required-keys)))
      {:decision :commit :reason :complete-enough :missing (count missing)}
      :else
      {:decision :hold :reason :incomplete-catalog :missing (count missing)})))

(defn plan-for-order
  "Convenience: catalog body + order → reviewed plan, in one call."
  [catalog-body order & [policy]]
  (let [{:keys [free-backends]} (merge default-policy policy)
        p (proc/plan catalog-body {:free-backends free-backends
                                   :budget-usd (:order/budget-usd order)
                                   :units (:order/units order)})]
    (review-plan p {:budget-usd (:order/budget-usd order)} policy)))
