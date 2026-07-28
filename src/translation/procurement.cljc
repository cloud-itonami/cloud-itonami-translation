(ns translation.procurement
  "Turn an x402 `/catalog` into a procurement plan. Pure.

  The supply side of this business. Inference is the cost of goods and this
  actor owns none of it — it buys from whichever sellers a facilitator's
  catalog offers (`GET https://x402.nexus/catalog`, live, x402Version 1).

  THE TRAP THIS NAMESPACE EXISTS TO AVOID (measured 2026-07-28): the catalog
  already lists `murakumo` at $0.01 per POST /v1/messages, so it looks like a
  translation job can simply be paid for and parallelised. It cannot — that
  seller's `origin` is `https://murakumo.cloud/api`, which fronts the SAME
  fleet the operator already reaches directly and for free. Routing ~16,000
  requests through it would have cost ~$160 and delivered ZERO extra
  throughput. `distinct-capacity` is the check: two sellers only add capacity
  if they resolve to different backends, and a plan that ignores this is
  buying its own GPUs back at retail.

  Capacity therefore has exactly two honest sources — a genuinely independent
  seller, or more fleet — and `plan` reports which one a given catalog can
  actually give you."
  (:require [clojure.string :as str]))

(defn parse-catalog
  "x402 `/catalog` body → normalised offers. Pure.

  Accepts the wire shape ({:items [{:seller :method :path-prefix :gateway
  :price {:usd :micros :asset :network :payTo}}]}) and keeps only what a
  procurement decision needs."
  [body]
  (->> (:items body)
       (keep (fn [{:keys [seller method gateway price] :as item}]
               (when (and seller gateway)
                 {:seller (str seller)
                  :method (or method "POST")
                  :gateway gateway
                  :origin (:origin item)
                  :usd (some-> (:usd price) str)
                  :micros (some-> (:micros price) str)
                  :asset (:asset price)
                  :network (:network price)
                  :pay-to (:payTo price)})))
       vec))

(defn inference-offers
  "Offers that can actually run a translation batch — a POST endpoint that
  speaks a messages API. A blob gateway or a premium-content path is in the
  same catalog and is not inference."
  [offers]
  (filterv (fn [{:keys [method gateway]}]
             (and (= "POST" (str/upper-case (str method)))
                  (re-find #"/v1/(messages|chat)" (str gateway))))
           offers))

(defn- ->usd
  "Price string -> double. Portable; a bad price is 0.0, never an exception."
  [s]
  #?(:clj (try (Double/parseDouble (str s)) (catch Exception _ 0.0))
     :cljs (let [n (js/parseFloat (str s))] (if (js/isNaN n) 0.0 n))))

(defn- backend-of
  "The thing an offer ultimately runs on. Two offers sharing a backend share a
  queue, so they are one supplier wearing two names."
  [{:keys [origin gateway]}]
  (let [u (str (or origin gateway))]
    (-> u (str/replace #"^https?://" "") (str/split #"/") first)))

(defn distinct-capacity
  "Offers grouped by backend, cheapest first within each group.
  → [{:backend str :offers [offer …]} …]

  The number of GROUPS is the parallelism a catalog can actually buy. The
  number of OFFERS is not."
  [offers]
  (->> offers
       (group-by backend-of)
       (mapv (fn [[b os]]
               {:backend b
                :offers (vec (sort-by #(->usd (:usd %)) os))}))
       (sort-by :backend)
       vec))

(defn plan
  "Build a procurement plan for `units` requests against `catalog-body`,
  excluding backends the operator already reaches for free.

  `opts`:
    :free-backends  #{host …}  backends the operator owns/reaches directly
    :budget-usd     number     hard ceiling; the governor enforces it too
    :units          int        request count the job needs

  → {:added-capacity int   ; independent backends worth paying for
     :self-backends [str]  ; offers that would re-buy owned capacity
     :buyable [{:seller :backend :usd :units :cost-usd}]
     :cost-usd number
     :within-budget? bool
     :verdict kw}          ; :adds-capacity | :no-independent-supply | :over-budget

  `:no-independent-supply` is the honest answer for the catalog as it stands
  on 2026-07-28, and it is the whole reason this returns a verdict instead of
  just a bill."
  [catalog-body {:keys [free-backends budget-usd units] :or {free-backends #{} units 0}}]
  (let [offers (-> catalog-body parse-catalog inference-offers)
        groups (distinct-capacity offers)
        free? (fn [g] (contains? free-backends (:backend g)))
        self (filterv free? groups)
        independent (remove free? groups)
        per (when (seq independent) (long (/ units (count independent))))
        buyable (vec (for [g independent
                           :let [o (first (:offers g))
                                 price (->usd (:usd o))]]
                       {:seller (:seller o)
                        :backend (:backend g)
                        :usd (:usd o)
                        :gateway (:gateway o)
                        :pay-to (:pay-to o)
                        :units per
                        :cost-usd (* price per)}))
        cost (reduce + 0.0 (map :cost-usd buyable))]
    {:added-capacity (count independent)
     :self-backends (mapv :backend self)
     :buyable buyable
     :cost-usd cost
     :within-budget? (or (nil? budget-usd) (<= cost budget-usd))
     :verdict (cond
                (empty? independent) :no-independent-supply
                (and budget-usd (> cost budget-usd)) :over-budget
                :else :adds-capacity)}))
