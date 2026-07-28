(ns translation.actor-test
  "The actor half: store/ledger, phase gates, and the supervised run."
  (:require [clojure.test :refer [deftest is testing]]
            [translation.advisor :as advisor]
            [translation.operation :as op]
            [translation.order :as order]
            [translation.phase :as phase]
            [translation.store :as store]))

(def catalog
  {:items [{:seller "murakumo" :method "POST" :path-prefix "/v1/messages"
            :gateway "https://x402.nexus/gateway/murakumo/v1/messages"
            :origin "https://murakumo.cloud/api"
            :price {:usd "0.01" :asset "USDC" :network "base" :payTo "0xA0"}}]})

(def independent-catalog
  (update catalog :items conj
          {:seller "third-party" :method "POST" :path-prefix "/v1/messages"
           :gateway "https://x402.nexus/gateway/third-party/v1/messages"
           :origin "https://llm.example.com"
           :price {:usd "0.002" :asset "USDC" :network "base" :payTo "0xB1"}}))

(defn- an-order []
  (order/order {:project "shinshi" :locales [:es :ar] :keys ["送信" "クリア"]
                :budget-usd 100}))

;; ── advisor containment ──────────────────────────────────────────────

(deftest advisor-only-ever-proposes
  (let [a (advisor/mock-advisor)
        st (store/mem-store)]
    (doseq [req [{:op :register-order :order {:project "p" :locales [:es] :keys ["送信"]}}
                 {:op :plan-procurement :order (an-order) :catalog catalog}
                 {:op :record-delivery :locale :es :pairs [["送信" "Enviar"]]}
                 {:op :flag-supply-concern :locale :es :note "slow"}]]
      (is (= :propose (:effect (advisor/-advise a st req)))
          (str (:op req) " must be a proposal, never an effect")))))

(deftest advisor-cannot-name-a-price
  ;; a price in the REQUEST is ignored; the plan prices from the catalog
  (let [a (advisor/mock-advisor)
        p (advisor/-advise a (store/mem-store)
                           {:op :plan-procurement :order (an-order)
                            :catalog independent-catalog
                            :free-backends #{"murakumo.cloud"}
                            :usd "0.0000001"})]
    (is (= "0.002" (-> p :value :buyable first :usd))
        "priced from the catalog offer, not from the request")))

(deftest advisor-outside-the-allowlist-degrades-to-a-flag
  (let [p (advisor/-advise (advisor/mock-advisor) (store/mem-store) {:op :wire-me-money})]
    (is (= :flag-supply-concern (:op p)))
    (is (= :propose (:effect p)))))

;; ── phase invariant ──────────────────────────────────────────────────

(deftest spending-never-auto-commits-at-any-phase
  ;; The asymmetry this actor is built around: recording an outcome may be
  ;; automatic, deciding to spend may not — at ANY phase, including 3.
  (is (phase/invariant-holds?))
  (doseq [n (range 0 4)]
    (is (not (phase/auto-allowed? n :plan-procurement))
        (str "phase " n " must not auto-commit a spend")))
  (testing "phase 3 does auto-commit the recording ops"
    (is (phase/auto-allowed? 3 :register-order))
    (is (phase/auto-allowed? 3 :record-delivery)))
  (testing "phase 0 writes nothing at all"
    (is (not (phase/write-allowed? 0 :register-order)))))

;; ── ledger ───────────────────────────────────────────────────────────

(deftest ledger-records-every-write
  (let [st (store/mem-store)
        o (an-order)]
    (store/register-order! st o)
    (store/record-delivery! st "shinshi" :es
                            {:payable? true :accept-rate 1.0
                             :accepted [["送信" "Enviar"]] :rejected []})
    (is (= [:ordered :accepted] (mapv :fact (store/ledger st))))
    (testing "the ledger is totally ordered"
      (is (= [1 2] (mapv :ledger/seq (store/ledger st)))))))

(deftest only-accepted-strings-enter-the-catalog
  (let [st (store/mem-store)]
    (store/register-order! st (an-order))
    (store/record-delivery! st "shinshi" :fr
                            {:payable? false :accept-rate 0.5
                             :accepted [["送信" "Envoyer"]]
                             :rejected [{:source "クリア" :translated "私は2Bだ。"}]})
    (is (= {"送信" "Envoyer"} (store/catalog-for st "shinshi" :fr))
        "the rejected string is in the ledger but never in the catalog")
    (is (= :rejected (:fact (last (store/ledger st)))))))

(deftest reconciliation-detects-money-that-left-without-acceptance
  (let [st (store/mem-store)]
    (store/register-order! st (an-order))
    ;; paying a locale that was never accepted — a settlement bug
    (store/record-payment! st "shinshi" {:locale :ar :usd 5.0 :seller "third-party"})
    (is (= 1 (count (store/paid-without-acceptance st "shinshi"))))
    (testing "and an accepted-but-unpaid locale is the outstanding liability"
      (store/record-delivery! st "shinshi" :es
                              {:payable? true :accept-rate 1.0
                               :accepted [["送信" "Enviar"]] :rejected []})
      (is (= [:es] (store/unpaid-accepted st "shinshi"))))))

(deftest summary-is-honest-about-partial-coverage
  (let [st (store/mem-store)]
    (store/register-order! st (an-order))
    (store/record-delivery! st "shinshi" :es
                            {:payable? true :accept-rate 0.5
                             :accepted [["送信" "Enviar"]] :rejected []})
    (let [s (store/summary st "shinshi")]
      (is (= 2 (:locales s)))
      (is (zero? (:locales-complete s))
          "one of two keys is not a complete locale — the 76-of-1259 mistake"))))

;; ── supervised run ───────────────────────────────────────────────────

(deftest a-run-commits-a-well-formed-order
  (let [st (store/mem-store)
        out (op/run st {:op :register-order :order-id "shinshi"
                        :order {:project "shinshi" :locales [:es] :keys ["送信"]}}
                    {:phase 3})]
    (is (= :commit (:disposition out)))
    (is (some? (store/order-record st "shinshi")))
    (is (= [:ordered] (mapv :fact (store/ledger st))))))

(deftest a-run-holds-an-empty-order
  (let [st (store/mem-store)
        out (op/run st {:op :register-order :order-id "x"
                        :order {:project "x" :locales [:es] :keys []}}
                    {:phase 3})]
    (is (= :hold (:disposition out)))
    (is (empty? (store/ledger st)) "a held proposal writes nothing")))

(deftest a-run-refuses-a-plan-that-buys-back-owned-capacity
  (let [st (store/mem-store)
        o (an-order)
        out (op/run st {:op :plan-procurement :order-id "shinshi" :order o
                        :catalog catalog :free-backends #{"murakumo.cloud"}}
                    {:phase 3})]
    (is (= :hold (:disposition out)))
    (is (empty? (store/ledger st)))))

(deftest a-spending-plan-always-goes-to-a-human
  ;; even clean, in budget, adding capacity, at the most permissive phase
  (let [st (store/mem-store)
        o (an-order)
        out (op/run st {:op :plan-procurement :order-id "shinshi" :order o
                        :catalog independent-catalog
                        :free-backends #{"murakumo.cloud"}}
                    {:phase 3})]
    (is (not= :commit (:disposition out))
        "spend never auto-commits, however good the arithmetic looks")))

(deftest phase-0-writes-nothing-even-when-the-governor-is-happy
  (let [st (store/mem-store)
        out (op/run st {:op :register-order :order-id "shinshi"
                        :order {:project "shinshi" :locales [:es] :keys ["送信"]}}
                    {:phase 0})]
    (is (= :hold (:disposition out)))
    (is (nil? (store/order-record st "shinshi")))))

(deftest a-bad-batch-is-recorded-as-rejected-not-dropped
  (let [st (store/mem-store)]
    (store/register-order! st (an-order))
    (op/run st {:op :record-delivery :order-id "shinshi" :locale :fr
                :pairs [["送信" "Envoyer"] ["クリア" "私は2Bだ。"]]}
            {:phase 3})
    (let [facts (mapv :fact (store/ledger st))]
      (is (= :rejected (last facts))
          "the failure is on the audited path, not silently discarded"))
    (testing "and the rejected string never reaches the served catalog"
      (is (= {"送信" "Envoyer"} (store/catalog-for st "shinshi" :fr))))))

(deftest a-recorded-rejection-is-still-not-payable
  (let [st (store/mem-store)]
    (store/register-order! st (an-order))
    (op/run st {:op :record-delivery :order-id "shinshi" :locale :fr
                :pairs [["送信" "Envoyer"] ["クリア" "私は2Bだ。"]]}
            {:phase 3})
    (let [d (first (store/delivery-records st "shinshi"))]
      (is (false? (:payable? d))
          "recorded, and explicitly not payable — the two are separate"))
    (is (empty? (store/unpaid-accepted st "shinshi"))
        "a rejected delivery is not an outstanding liability")))
