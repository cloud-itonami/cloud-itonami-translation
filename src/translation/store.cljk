(ns translation.store
  "SSoT + append-only audit ledger for the translation actor.

  What has to be recoverable from the ledger alone, because this actor spends
  money on someone else's behalf:

  - what was ORDERED (project, locales, keys, tier, budget),
  - what was PLANNED (which backend, at what price, for how many units),
  - what was DELIVERED and what the acceptance verdict was,
  - what was PAID, and against which delivery.

  The pairing of the last two is the point. A procurement receipt that cannot
  be tied back to an acceptance verdict is how you end up paying for a batch
  that came back in the wrong language — which is exactly what happened when
  a 1,259-key run reported success having delivered 76 keys. The ledger makes
  `:paid` without a preceding `:accepted` a *detectable* state rather than an
  invisible one; `unpaid-accepted` and `paid-without-acceptance` are the two
  reconciliation queries that fall out of it.

  MemStore is the test/offline backend. The durable backend is D1 behind the
  edge worker (`translation.edge.worker`), which implements the same protocol
  over the same fact shapes — `durable?` is what tells them apart.")

(defprotocol Store
  (order-record [s order-id] "Registered order, or nil.")
  (all-order-records [s])
  (plan-record [s order-id] "Accepted procurement plan for an order, or nil.")
  (delivery-records [s order-id] "Every delivered batch for an order.")
  (payment-records [s order-id])
  (catalog-for [s order-id locale] "Accepted strings so far, {source translated}.")
  (ledger [s] "Append-only fact log, oldest first.")
  (durable? [s] "False for the test-only memory backend.")
  (register-order! [s order])
  (record-plan! [s order-id plan])
  (record-delivery! [s order-id locale verdict])
  (record-payment! [s order-id receipt])
  (append-ledger! [s fact]))

(defn- now-stamp
  "Monotonic sequence, not wall-clock: the ledger needs a total order, and a
  pure core must not read a clock (it would make every run unreproducible)."
  [a]
  (:seq (swap! a update :seq inc)))

(defrecord MemStore [a]
  Store
  (durable? [_] false)
  (order-record [_ id] (get-in @a [:orders id]))
  (all-order-records [_] (sort-by :order/project (vals (:orders @a))))
  (plan-record [_ id] (get-in @a [:plans id]))
  (delivery-records [_ id] (vec (get-in @a [:deliveries id] [])))
  (payment-records [_ id] (vec (get-in @a [:payments id] [])))
  (catalog-for [_ id locale] (get-in @a [:catalogs [id locale]] {}))
  (ledger [_] (vec (:ledger @a)))

  (append-ledger! [_ fact]
    (let [n (now-stamp a)]
      (swap! a update :ledger conj (assoc fact :ledger/seq n))
      nil))

  (register-order! [s order]
    (swap! a assoc-in [:orders (:order/project order)] order)
    (append-ledger! s {:fact :ordered
                       :order (:order/project order)
                       :locales (count (:order/schedule order))
                       :keys (count (:order/keys order))
                       :units (:order/units order)
                       :budget-usd (:order/budget-usd order)})
    order)

  (record-plan! [s id plan]
    (swap! a assoc-in [:plans id] plan)
    (append-ledger! s {:fact :planned
                       :order id
                       :verdict (:verdict plan)
                       :added-capacity (:added-capacity plan)
                       :cost-usd (:cost-usd plan)})
    plan)

  (record-delivery! [s id locale verdict]
    (swap! a update-in [:deliveries id] (fnil conj []) (assoc verdict :locale locale))
    ;; only ACCEPTED strings enter the catalog; rejected ones are recorded in
    ;; the ledger but never served
    (swap! a update-in [:catalogs [id locale]]
           (fnil into {}) (:accepted verdict))
    (append-ledger! s {:fact (if (:payable? verdict) :accepted :rejected)
                       :order id
                       :locale locale
                       :accept-rate (:accept-rate verdict)
                       :accepted (count (:accepted verdict))
                       :rejected (count (:rejected verdict))})
    verdict)

  (record-payment! [s id receipt]
    (swap! a update-in [:payments id] (fnil conj []) receipt)
    (append-ledger! s {:fact :paid
                       :order id
                       :locale (:locale receipt)
                       :usd (:usd receipt)
                       :seller (:seller receipt)
                       :settlement (:settlement receipt)})
    receipt))

(defn mem-store
  ([] (mem-store {}))
  ([seed]
   (->MemStore (atom (merge {:orders {} :plans {} :deliveries {} :payments {}
                             :catalogs {} :ledger [] :seq 0}
                            seed)))))

;; ── reconciliation ───────────────────────────────────────────────────

(defn facts-of [store kind] (filterv #(= kind (:fact %)) (ledger store)))

(defn paid-without-acceptance
  "Payments for a locale that never produced an :accepted fact BEFORE them.

  This is the query the whole ledger shape exists to make answerable. It should
  always be empty; a non-empty result means money left for work the governor
  refused, and that is a settlement bug, not a translation one."
  [store order-id]
  (let [accepted (into #{} (map :locale) (filter #(and (= :accepted (:fact %))
                                                       (= order-id (:order %)))
                                                 (ledger store)))]
    (vec (for [p (payment-records store order-id)
               :when (not (contains? accepted (:locale p)))]
           p))))

(defn unpaid-accepted
  "Locales accepted but not yet paid — the actor's outstanding liability."
  [store order-id]
  (let [paid (into #{} (map :locale) (payment-records store order-id))]
    (vec (for [d (delivery-records store order-id)
               :when (and (:payable? d) (not (contains? paid (:locale d))))]
           (:locale d)))))

(defn coverage
  "Accepted-key coverage per locale for an order → {locale ratio}."
  [store order-id]
  (let [{:keys [order/keys order/schedule]} (order-record store order-id)
        total (count keys)]
    (into {} (for [l schedule]
               [l (if (pos? total)
                    (double (/ (count (catalog-for store order-id l)) total))
                    0.0)]))))

(defn summary
  "One-line operational state for an order. Used by the edge worker's status
  endpoint and by sim output."
  [store order-id]
  (let [cov (coverage store order-id)
        done (count (filter #(>= % 0.95) (vals cov)))]
    {:order order-id
     :locales (count cov)
     :locales-complete done
     :unpaid-accepted (count (unpaid-accepted store order-id))
     :paid-without-acceptance (count (paid-without-acceptance store order-id))
     :ledger-facts (count (ledger store))}))
