(ns translation.publish-test
  "Slice d — the publish bridge. Written against what the bridge actually does
  (encode → store → index → pin → lake-admit), verifying BOTH that a
  governor-accepted catalog is published in that order, and that a catalog the
  TranslationGovernor refuses is never stored, pinned, or admitted."
  (:require [clojure.test :refer [deftest is testing]]
            [i18n-cid.core :as ic]
            [translation.publish :as pub]))

;; ── fixtures ─────────────────────────────────────────────────────────

(def ja-catalog
  {:app/title "ようこそ"
   :app/greeting {:select :count :one "1件" :other "%{count} 件"}
   :nav/home "ホーム"})

(defn- recorder
  "Shared event log + a mem block store, so a test can assert both the ORDER
  of the seam calls and that nothing ran on refusal."
  []
  (let [events (atom [])
        store  (atom {})]
    {:events events
     :store  store
     :get-fn (fn [cid] (get @store cid))
     :put-block-fn (fn [cid bytes]
                     (swap! events conj [:store cid])
                     (swap! store assoc cid bytes)
                     bytes)
     :post-fn (fn [path body]
                (swap! events conj [:pin path body])
                {"id" (str "pin-" (subs (:cid body) 0 8))})}))

(defn- ctx
  "A publish ctx with deterministic seams and a complete required-key set, so
  the governor gate passes and the flow runs to lake-admit."
  [r]
  {:project :myapp
   :required-keys (keys ja-catalog)
   :tenant "test-tenant"
   :ingested-at "2026-09-03T00:00:00Z"
   :put-block-fn (:put-block-fn r)
   :post-fn (:post-fn r)})

;; ── governor-passing catalog publishes ───────────────────────────────

(deftest accepts-a-governor-passed-catalog-in-flow-order
  (let [r (recorder)
        out (pub/publish-catalog! (ctx r) :ja ja-catalog)]
    (testing "result says published with CID + pin + lake claim"
      (is (:published? out))
      (is (= :commit (:decision out)))
      (is (re-find #"bafy" (:cid out)))
      (is (re-find #"bafy" (:index-cid out)))
      (is (:pinned? (:pin out)))
      (is (:admitted? (:lake out)))
      (testing "the catalog was admitted to the lake (datom-plane queryable)"
        (is (pos? (count (:quads (:lake out)))))))
    (testing "flow order is catalog->block → store → pin → lake-claim"
      (is (= [:catalog-block :store :locale-index :pin :lake-claim :index-lake-claim]
             (:steps out)))
      (testing "the store and pin seams were hit in that order"
        (let [seams (mapv first @(:events r))]
          (is (= [:store :store :pin] (take 3 seams))))))
    (testing "the pin request named the release by project/locale"
      (let [[_ path body] (first (filter #(= :pin (first %)) @(:events r)))]
        (is (= "/pins" path))
        (is (re-find #"^myapp/ja$" (str (:name body))))))))

;; ── governor-refusing catalog never publishes ────────────────────────

(deftest refuses-a-governor-rejected-catalog-with-no-side-effects
  (let [r (recorder)
        ;; required-keys far exceed the catalog → review-publication holds it
        out (pub/publish-catalog! (assoc (ctx r) :required-keys (map str (range 100)))
                                  :ja ja-catalog)]
    (testing "it is refused, not published"
      (is (false? (:published? out)))
      (is (= :hold (:decision out)))
      (is (= :incomplete-catalog (:reason out)))))
  (let [r (recorder)
        out (pub/publish-catalog! (assoc (ctx r) :required-keys (map str (range 100)))
                                  :ja ja-catalog)]
    (testing "no block was stored, no pin requested, nothing admitted"
      (is (empty? @(:events r)))
      (is (empty? @(:store r)))
      (is (nil? (:pin out)))
      (is (nil? (:lake out))))))

;; ── project publish links gated locales, excludes refused ones ───────

(deftest project-index-links-only-gated-locales
  (let [r (recorder)
        out (pub/publish-project! (ctx r)
                                  {:ja ja-catalog
                                   ;; a locale that fails the gate is excluded
                                   :ko {:app/title "ホームだけ"}})]
    (testing "published set has the gated locale, refused set has the short one"
      (is (= 1 (count (:published out))))
      (is (contains? (:published out) :ja))
      (is (contains? (:refused out) :ko))
      (is (= :incomplete-catalog (get-in out [:refused :ko :reason]))))
    (testing "the shared index block links only the published locale CID"
      (let [r2 (recorder)
            single (pub/publish-project! (ctx r2) {:ja ja-catalog})]
        (is (= (:index-cid single) (:index-cid out))
            "same project+same ja block → same index CID, ko never enters")))))

;; ── round-trip: the published block is a real i18n catalog ───────────

(deftest published-block-round-trips-through-the-block-store
  (let [r (recorder)
        out (pub/publish-catalog! (ctx r) :ja ja-catalog)]
    ;; the stored catalog block is a real CID-addressed i18n catalog: fetch it
    ;; back through dag->catalog and it restores the exact register! shape
    (is (= ja-catalog (ic/dag->catalog (:get-fn r) (:cid out))))
    (is (= ja-catalog (ic/dag->catalog (:get-fn r) (get-in out [:pin :cid]))))))