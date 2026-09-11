(ns translation.advisor
  "TranslationAdvisor — the contained intelligence node.

  Four ops from a closed allowlist: registering an order, planning its
  procurement, recording a supplier delivery, and flagging a supply concern.

  CRITICAL: every proposal's `:effect` is always `:propose`. Nothing here
  writes, pays, or publishes — `translation.governor` censors every output
  first.

  What this advisor structurally CANNOT do, and why each one matters:

  - **It cannot name a price.** `:plan-procurement` passes the catalog to
    `translation.procurement/plan`, which reads prices from the CATALOG. A
    price in the request payload is ignored — there is no code path that
    reads one — so neither a supplier nor a confused model can quote itself.
  - **It cannot decide a batch is acceptable.** `:record-delivery` carries the
    raw supplier output; the verdict comes from `translation.qa`, computed by
    the governor. An advisor that could mark its own work accepted is an
    advisor that can spend money on nothing.
  - **It cannot declare capacity.** Whether a plan adds throughput is decided
    by backend-deduplication in procurement, not by the advisor's opinion —
    the $160-for-zero-throughput near-miss was exactly a plausible-sounding
    plan that no one checked.

  Deterministic mock so the actor graph runs offline; in production this calls
  a real LLM that must return the same proposal shape."
  (:require [translation.order :as order]
            [translation.procurement :as proc]))

(def ops #{:register-order :plan-procurement :record-delivery :flag-supply-concern})

(defprotocol Advisor
  (-advise [a store request] "request -> proposal map. NEVER writes."))

(defn- propose [op value & [extra]]
  (merge {:effect :propose :op op :value value} extra))

(defrecord MockAdvisor []
  Advisor
  (-advise [_ _store {:keys [op] :as request}]
    (case op
      :register-order
      (propose :register-order (order/order (:order request))
               {:rationale "normalise the order and schedule it by reach"})

      :plan-procurement
      (let [o (:order request)
            plan (proc/plan (:catalog request)
                            {:free-backends (:free-backends request #{})
                             :budget-usd (:order/budget-usd o)
                             :units (:order/units o)})]
        (propose :plan-procurement plan
                 {:rationale (case (:verdict plan)
                               :no-independent-supply
                               "every offer resolves to a backend the operator already reaches; buying adds no throughput"
                               :over-budget
                               "the only independent supply costs more than the order's budget"
                               "independent backends are available within budget")}))

      :record-delivery
      (propose :record-delivery
               {:locale (:locale request)
                :pairs (:pairs request)
                :seller (:seller request)}
               {:rationale "hand the supplier's raw output to acceptance"})

      :flag-supply-concern
      (propose :flag-supply-concern
               {:locale (:locale request) :note (:note request)}
               {:rationale "a supply problem a human should look at"})

      ;; An op outside the allowlist is not an error to argue about — it is a
      ;; proposal the governor will reject, which keeps the failure on the
      ;; audited path instead of throwing somewhere unlogged.
      (propose :flag-supply-concern
               {:locale nil :note (str "unknown op: " (pr-str op))}
               {:rationale "op outside the advisor allowlist"}))))

(defn mock-advisor [] (->MockAdvisor))
