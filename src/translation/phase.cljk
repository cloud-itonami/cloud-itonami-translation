(ns translation.phase
  "Phase 0→3 staged rollout for the translation actor.

    Phase 0  read-only        — nothing writes. Plans may be computed and
                                shown; no order is registered, no batch is
                                accepted, nothing is paid.
    Phase 1  assisted-intake  — orders may be registered; every write needs
                                human approval.
    Phase 2  assisted-buying  — adds delivery recording, still approval-gated.
    Phase 3  supervised auto  — governor-clean `:register-order` and
                                `:record-delivery` may auto-commit.

  `:plan-procurement` is ABSENT from every phase's `:auto` set, including
  phase 3, and that is the deliberate asymmetry of this actor.

  ## Recording an outcome is not the same as committing money

  `:record-delivery` writes down what a supplier already sent and what
  acceptance made of it. Refusing to record that until a human agrees does not
  un-send the batch; it just makes the ledger lag reality, and a ledger that
  lags is worse than one that records a rejection promptly.

  `:plan-procurement` is the opposite: it is the step that decides to SPEND.
  The failure it guards against is not a wrong number, it is a plausible one —
  a plan to route 16,000 requests through a seller at $0.01 each reads as
  perfectly reasonable right up until you notice the seller fronts the fleet
  you already own. The governor catches that specific case by construction,
  but a class of mistake that costs money on a plausible-looking proposal is
  exactly the class that should never auto-commit.

  `:flag-supply-concern` is likewise never automatic — a flag exists to reach
  a human, so auto-committing it defeats its only purpose."
  (:require [clojure.set :as set]))

(def phases
  {0 {:name :read-only        :writes #{}                                        :auto #{}}
   1 {:name :assisted-intake  :writes #{:register-order :flag-supply-concern}     :auto #{}}
   2 {:name :assisted-buying  :writes #{:register-order :record-delivery
                                        :flag-supply-concern}                     :auto #{}}
   3 {:name :supervised-auto  :writes #{:register-order :record-delivery
                                        :plan-procurement :flag-supply-concern}   :auto #{:register-order
                                                                                          :record-delivery}}})

(defn phase-of [n] (get phases (or n 0) (get phases 0)))

(defn write-allowed?
  "May `op` write at all at this phase?"
  [n op]
  (contains? (:writes (phase-of n)) op))

(defn auto-allowed?
  "May `op` commit WITHOUT human approval at this phase?"
  [n op]
  (contains? (:auto (phase-of n)) op))

(defn spend-ops
  "Ops that move money. Never auto at any phase — asserted by a test so a
  future phase cannot quietly add one."
  []
  #{:plan-procurement})

(defn invariant-holds?
  "No phase may auto-commit a spending op."
  []
  (every? (fn [[_ {:keys [auto]}]] (empty? (set/intersection auto (spend-ops))))
          phases))
