(ns translation.core-test
  "Tests are written against the SHAPES production actually produced on
  2026-07-27/28, not invented ones — every fixture below is a real defect or a
  real catalog response."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [translation.governor :as gov]
            [translation.order :as order]
            [translation.procurement :as proc]
            [translation.qa :as qa]))

;; The live x402.nexus/catalog body, 2026-07-28.
(def live-catalog
  {:x402Version 1 :service "nexus-x402" :count 3
   :items [{:seller "murakumo" :method "POST" :path-prefix "/v1/messages"
            :gateway "https://x402.nexus/gateway/murakumo/v1/messages"
            :origin "https://murakumo.cloud/api"
            :price {:usd "0.01" :micros "10000" :asset "USDC" :network "base"
                    :payTo "0xA00366234D29d4F882088048c0B2fa0dB7302D4E"}}
           {:seller "kotobase" :method "GET" :path-prefix "/ipfs/"
            :gateway "https://x402.nexus/gateway/kotobase/ipfs/"
            :origin "https://kotobase.net"
            :price {:usd "0.001" :asset "USDC" :network "base"
                    :payTo "0xA00366234D29d4F882088048c0B2fa0dB7302D4E"}}
           {:seller "shinshi" :method "GET" :path-prefix "/x402/premium/"
            :gateway "https://x402.nexus/gateway/shinshi/x402/premium/"
            :origin "https://shinshi.club"
            :price {:usd "0.50" :asset "USDC" :network "base"
                    :payTo "0xf1592811063554e2EDfC137964aE181C830967Ac"}}]})

;; ── quality ──────────────────────────────────────────────────────────

(deftest catches-the-dominant-failure
  (testing "Japanese returned for a non-Japanese locale — 4/6 for French under
            a real persona, and invisible to a byte-count check"
    (is (contains? (qa/defects :fr "はじめまして" "私はヨルハ二号B型。")
                   :source-language-leak)))
  (testing "kanji alone is NOT a leak — Chinese output is full of it"
    (is (not (contains? (qa/defects :zh "送信" "我是2B。执行任务。")
                        :source-language-leak))))
  (testing "Japanese output for the Japanese locale is correct, not a defect"
    (is (empty? (qa/defects :ja "送信" "送信")))))

(deftest catches-placeholder-damage
  (testing "a dropped {name} renders a sentence with no name in it"
    (is (contains? (qa/defects :es "{name} を応援" "Apoyar")
                   :placeholder-dropped)))
  (testing "a dropped %s leaves a greeting with no character in it"
    (is (contains? (qa/defects :es "%sだよ。" "Hola.")
                   :printf-placeholder-mismatch))))

(deftest repairs-only-what-is-unambiguous
  (testing "brace spacing — wuu did this on every placeholder key"
    (is (= "支持侬，{name}" (qa/repair "{name} を応援" "支持侬，{ name}"))))
  (testing "a lone %-token standing in for a single {param}"
    (is (= "Ko %s." (qa/repair "%sです。" "Ko %n."))
        "Fula returned %n for a %s key — one stray token, one source token")
    (is (= "Ko {name}." (qa/repair "{name}です。" "Ko %n."))))
  (testing "edge space the source has and the translation lost — ko rendered
            \"2B메시지…\" without this"
    (is (= " 메시지" (qa/repair " にメッセージ" "메시지"))))
  (testing "repair does NOT invent a translation"
    (is (= "" (qa/repair "送信" "")))))

(deftest verdict-accepts-a-repaired-string
  (let [v (qa/verdict :ko " にメッセージ…" "메시지를 입력하세요…")]
    (is (:accepted? v))
    (is (:repaired? v))
    (is (str/starts-with? (:translated v) " "))))

(deftest batch-verdict-prices-by-usable-output
  (let [{:keys [accept-rate rejected]}
        (qa/batch-verdict :fr [["送信" "Envoyer"]
                               ["クリア" "Effacer"]
                               ["再送信" "私は2Bだ。"]])]
    (is (= 1 (count rejected)))
    (is (< accept-rate 0.7) "a third of the batch is Japanese")))

;; ── procurement ──────────────────────────────────────────────────────

(deftest only-inference-offers-can-run-a-batch
  (let [offers (-> live-catalog proc/parse-catalog proc/inference-offers)]
    (is (= 1 (count offers)))
    (is (= "murakumo" (:seller (first offers))))
    (testing "a blob gateway and a premium-content path are in the same
              catalog and are not inference"
      (is (not-any? #{"kotobase" "shinshi"} (map :seller offers))))))

(deftest paying-for-your-own-fleet-adds-no-capacity
  ;; THE defect this actor exists to prevent: murakumo's origin is
  ;; murakumo.cloud/api, the same fleet the operator already reaches for free.
  (let [p (proc/plan live-catalog {:free-backends #{"murakumo.cloud"}
                                   :units 16000 :budget-usd 500})]
    (is (= :no-independent-supply (:verdict p)))
    (is (zero? (:added-capacity p)))
    (is (= ["murakumo.cloud"] (:self-backends p)))
    (is (zero? (:cost-usd p)) "and it costs nothing because nothing is bought")))

(deftest an-independent-seller-is-what-actually-buys-capacity
  (let [catalog (update live-catalog :items conj
                        {:seller "third-party" :method "POST"
                         :path-prefix "/v1/messages"
                         :gateway "https://x402.nexus/gateway/third-party/v1/messages"
                         :origin "https://llm.example.com"
                         :price {:usd "0.002" :asset "USDC" :network "base"
                                 :payTo "0xBEEF"}})
        p (proc/plan catalog {:free-backends #{"murakumo.cloud"}
                              :units 16000 :budget-usd 500})]
    (is (= :adds-capacity (:verdict p)))
    (is (= 1 (:added-capacity p)))
    (is (= 32.0 (:cost-usd p)) "16000 units x $0.002")
    (is (:within-budget? p))))

(deftest budget-is-a-ceiling-not-a-suggestion
  (let [catalog (update live-catalog :items conj
                        {:seller "pricey" :method "POST" :path-prefix "/v1/messages"
                         :gateway "https://x402.nexus/gateway/pricey/v1/messages"
                         :origin "https://pricey.example.com"
                         :price {:usd "0.10" :asset "USDC" :network "base" :payTo "0x1"}})
        p (proc/plan catalog {:free-backends #{"murakumo.cloud"}
                              :units 16000 :budget-usd 100})]
    (is (= :over-budget (:verdict p)))
    (is (not (:within-budget? p)))))

;; ── scheduling ───────────────────────────────────────────────────────

(deftest reach-order-front-loads-value
  (let [ls [:as :es :ar :or :pt :id :ru]
        s (order/schedule ls :tier/reach)]
    (is (= :es (first s)) "561M speakers before Assamese's 10M")
    (is (= [:es :ar :pt :id :ru] (take 5 s))))
  (testing "uniform order is alphabetical and delivers nothing early"
    (is (= :ar (first (order/schedule [:as :es :ar] :tier/uniform)))))
  (testing "an explicit list is honoured verbatim"
    (is (= [:as :es] (order/schedule [:as :es] :tier/listed)))))

(deftest coverage-curve-makes-the-tradeoff-arguable
  (let [s (order/schedule [:es :ar :pt :id :ru :as :or] :tier/reach)
        curve (order/coverage-curve s)]
    (is (= 7 (count curve)))
    (is (= 1.0 (:share (last curve))))
    (testing "the first five locales carry the overwhelming majority"
      (is (> (:share (nth curve 4)) 0.9)))))

(deftest locales-for-share-answers-what-must-finish
  (let [s (order/schedule [:es :ar :pt :id :ru :as :or] :tier/reach)]
    (is (<= (count (order/locales-for-share s 0.8)) 4))))

;; ── governor ─────────────────────────────────────────────────────────

(deftest governor-refuses-to-rebuy-owned-capacity
  (let [o (order/order {:project "shinshi" :locales [:es :ar] :keys ["送信"]
                        :budget-usd 500})
        d (gov/plan-for-order live-catalog o)]
    (is (= :reject (:decision d)))
    (is (= :no-added-capacity (:reason d)))))

(deftest governor-holds-large-spend-for-a-human
  (let [catalog (update live-catalog :items conj
                        {:seller "third-party" :method "POST" :path-prefix "/v1/messages"
                         :gateway "https://x402.nexus/gateway/third-party/v1/messages"
                         :origin "https://llm.example.com"
                         :price {:usd "0.01" :asset "USDC" :network "base" :payTo "0xB"}})
        o (order/order {:project "shinshi" :locales [:es] :keys (repeat 10000 "送信")
                        :budget-usd 500})
        d (gov/plan-for-order catalog o)]
    (is (= :hold (:decision d)))
    (is (= :spend-needs-human (:reason d)))))

(deftest a-bad-batch-is-not-payable
  (let [d (gov/review-batch :fr [["送信" "Envoyer"] ["クリア" "私は2Bだ。"]])]
    (is (= :reject (:decision d)))
    (is (false? (:payable? d)))
    (is (= :below-quality-floor (:reason d)))))

(deftest a-good-batch-settles
  (let [d (gov/review-batch :fr [["送信" "Envoyer"] ["クリア" "Effacer"]])]
    (is (= :commit (:decision d)))
    (is (:payable? d))))

(deftest a-short-catalog-is-not-a-fulfilled-order
  ;; The first automated run produced 76 of 1259 keys and reported success.
  (let [d (gov/review-publication {:required-keys (map str (range 1259))
                                   :catalog (zipmap (map str (range 76)) (repeat "x"))})]
    (is (= :hold (:decision d)))
    (is (= :incomplete-catalog (:reason d))))
  (testing "a nearly-complete catalog may publish"
    (let [d (gov/review-publication {:required-keys (map str (range 100))
                                     :catalog (zipmap (map str (range 98)) (repeat "x"))})]
      (is (= :commit (:decision d))))))
