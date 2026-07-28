(ns translation.operation
  "OperationActor — one translation request = one supervised actor run, as a
  langgraph-clj StateGraph.

  The advisor is sealed into a single node (`:advise`); its proposal is ALWAYS
  routed through the TranslationGovernor (`:govern`) and the rollout phase gate
  (`:decide`) before anything reaches the SSoT.

  Everything is injected, so each is a swap rather than a rewrite:
    - Store    (MemStore | D1-backed)  — `store` arg
    - Advisor  (mock | real LLM)       — `:advisor` opt
    - Phase    (0→3 rollout)           — `:phase` in ctx

  One graph run = one request (intake → advise → govern → decide →
  commit | hold | approval). No unbounded inner loop: a translation JOB is
  many runs, one per locale batch, each independently auditable and
  checkpointed. That is deliberate — the 9-hour catalog job that failed
  silently did so because it was one opaque process, not 83 auditable ones.

  Human-in-the-loop is a real workflow: `interrupt-before #{:request-approval}`
  pauses and hands the decision to a reviewer, who resumes with
  `{:approval {:status :approved :by \"…\"}}`."
  (:require [langgraph.graph :as g]
            [translation.advisor :as advisor]
            [translation.governor :as gov]
            [translation.phase :as phase]
            [translation.store :as store]))

(defn- govern
  "Censor one proposal. Returns {:decision :reason …} — the ONLY place a
  proposal becomes permitted."
  [_st {:keys [op value] :as _proposal} {:keys [policy] :as _ctx}]
  (case op
    :register-order
    (if (seq (:order/keys value))
      {:decision :commit :reason :order-well-formed}
      {:decision :reject :reason :empty-order})

    :plan-procurement
    (gov/review-plan value {:budget-usd (:budget-usd value)} policy)

    ;; A delivery is ALWAYS recorded, even when it fails acceptance. The
    ;; governor's refusal here means "not payable", not "do not write it
    ;; down" — a rejected batch that leaves no trace makes a supplier who
    ;; delivered garbage indistinguishable from one who was never used, and
    ;; the accept-rate history is the only thing that makes a supplier's real
    ;; price (per USABLE string) visible. Same principle phase.cljc states
    ;; from the other side: recording an outcome is not committing money.
    :record-delivery
    (let [{:keys [locale pairs]} value
          v (gov/review-batch locale pairs policy)]
      {:decision :commit
       :reason (:reason v)
       :payable? (:payable? v)
       :verdict v})

    :flag-supply-concern
    {:decision :hold :reason :human-attention}

    {:decision :reject :reason :op-not-allowed}))

(defn- commit!
  "Apply a permitted proposal to the store. Every branch appends to the ledger
  via the store — there is no write path that skips it."
  [st {:keys [op value]} verdict order-id]
  (case op
    :register-order (store/register-order! st value)
    :plan-procurement (store/record-plan! st order-id value)
    :record-delivery (store/record-delivery! st order-id (:locale value) verdict)
    :flag-supply-concern (do (store/append-ledger!
                              st {:fact :flagged :order order-id
                                  :locale (:locale value) :note (:note value)})
                             value)
    nil))

(defn build
  "Compile the actor graph. `store` is the SSoT; opts inject advisor/checkpointer."
  [store & [{:keys [advisor checkpointer] :or {advisor (advisor/mock-advisor)}}]]
  (-> (g/state-graph
       {:channels
        {:request     {:default nil}
         :context     {:default nil}
         :proposal    {:default nil}
         :verdict     {:default nil}
         :disposition {:default nil}
         :record      {:default nil}
         :approval    {:default nil}
         :audit       {:reducer into :default []}}})

      (g/add-node :intake (fn [s] s))

      (g/add-node :advise
        (fn [{:keys [request] :as s}]
          (assoc s :proposal (advisor/-advise advisor store request))))

      (g/add-node :govern
        (fn [{:keys [proposal context] :as s}]
          (assoc s :verdict (govern store proposal context))))

      (g/add-node :decide
        (fn [{:keys [proposal verdict context] :as s}]
          (let [op (:op proposal)
                ph (:phase context 0)
                d (cond
                    ;; the governor's refusal is final — phase can only be
                    ;; MORE restrictive, never less
                    (= :reject (:decision verdict)) :hold
                    (= :hold (:decision verdict)) :approval
                    (not (phase/write-allowed? ph op)) :hold
                    (phase/auto-allowed? ph op) :commit
                    :else :approval)]
            (assoc s :disposition d))))

      (g/add-node :request-approval
        (fn [{:keys [approval] :as s}]
          (assoc s :disposition (if (= :approved (:status approval)) :commit :hold))))

      (g/add-node :commit
        (fn [{:keys [proposal verdict request] :as s}]
          (let [rec (commit! store proposal (:verdict verdict) (:order-id request))]
            (-> s
                (assoc :record rec)
                (update :audit conj {:op (:op proposal) :committed true
                                     :reason (:reason verdict)})))))

      (g/add-node :hold
        (fn [{:keys [proposal verdict] :as s}]
          (update s :audit conj {:op (:op proposal) :committed false
                                 :reason (:reason verdict)})))

      (g/add-edge :intake :advise)
      (g/add-edge :advise :govern)
      (g/add-edge :govern :decide)
      (g/add-conditional-edges :decide
        (fn [{:keys [disposition]}]
          (case disposition
            :commit :commit
            :approval :request-approval
            :hold)))
      (g/add-conditional-edges :request-approval
        (fn [{:keys [disposition]}] (if (= :commit disposition) :commit :hold)))

      (g/set-entry-point :intake)
      (g/set-finish-point :commit)
      (g/set-finish-point :hold)

      (g/compile-graph
       {:checkpointer checkpointer
        :interrupt-before #{:request-approval}})))

(defn run
  "Convenience for tests and sim: one request through a freshly built graph."
  [store request & [{:keys [phase policy advisor] :or {phase 3}}]]
  (let [app (build store {:advisor (or advisor (advisor/mock-advisor))})]
    (g/invoke app {:request request
                   :context {:phase phase :policy policy}})))
