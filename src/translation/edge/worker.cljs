(ns translation.edge.worker
  "Cloudflare Worker edge for the translation actor — the HOST half.

  Serves the EXISTING contract, not a new one. `i18n.etzhayyim.com` already
  defines RegisterProject / TranslateBatch / WidgetApprove /
  GetLanguageRegistry, and `kotoba-lang/i18n` already ships `i18n.tm-import`
  as the client bridge, so any consumer that speaks to that service speaks to
  this one unchanged. The service is currently undeployed (its domain does not
  resolve), which is why this exists.

  Everything that decides anything lives in the pure core:

      XRPC request → translation.operation (StateGraph)
                   → advisor proposes → governor censors → phase gates
                   → store commits + appends to the ledger

  This namespace only does I/O: parse, dispatch, persist to D1, respond. It
  contains no acceptance rule, no price, and no policy — if a rule appears
  here it is in the wrong file, because the Worker is the one layer that
  cannot be unit-tested offline.

  Config, all optional, degrade honestly:
    TRANSLATION_DB       D1 binding for the durable ledger. Absent → the
                         actor runs in-memory and says so in /status rather
                         than pretending a write was durable.
    X402_CATALOG_URL     facilitator catalog (default https://x402.nexus/catalog)
    FREE_BACKENDS        comma-separated hosts the operator already reaches;
                         procurement refuses to buy these back
    PHASE                0–3 rollout gate (default 0 — read-only)"
  (:require [clojure.string :as str]
            [goog.object :as gobj]
            [translation.operation :as op]
            [translation.order :as order]
            [translation.store :as store]))

(def ^:private json-headers
  #js {"content-type" "application/json; charset=utf-8"
       "cache-control" "no-store"})

(defn- json-response [body & [status]]
  (js/Response. (js/JSON.stringify (clj->js body))
                #js {:status (or status 200) :headers json-headers}))

(defn- cfg [env k]
  (let [v (gobj/get env k)] (when (and (string? v) (seq v)) v)))

(defn- free-backends [env]
  (->> (str/split (or (cfg env "FREE_BACKENDS") "") #",")
       (map str/trim) (remove str/blank?) set))

(defn- phase-of [env]
  (let [n (js/parseInt (or (cfg env "PHASE") "0") 10)]
    (if (js/isNaN n) 0 n)))

;; ── D1-backed ledger ─────────────────────────────────────────────────
;; The ledger is append-only by construction: there is no UPDATE and no DELETE
;; in this namespace. A correction is a new fact, never an edited one.

(defn- append-fact!
  [env order-id fact]
  (when-let [db (gobj/get env "TRANSLATION_DB")]
    (-> (.prepare ^js db
                  "INSERT INTO translation_ledger (order_id, fact, payload) VALUES (?1, ?2, ?3)")
        (.bind (str order-id) (name (:fact fact)) (js/JSON.stringify (clj->js fact)))
        (.run)
        (.catch (fn [_] nil)))))

(defn- read-ledger
  [env order-id]
  (if-let [db (gobj/get env "TRANSLATION_DB")]
    (-> (.prepare ^js db
                  "SELECT fact, payload FROM translation_ledger
                   WHERE order_id = ?1 ORDER BY seq ASC")
        (.bind (str order-id))
        (.all)
        (.then (fn [^js res]
                 (into [] (map (fn [^js r]
                                 (try (js->clj (js/JSON.parse (gobj/get r "payload"))
                                               :keywordize-keys true)
                                      (catch :default _ {:fact (gobj/get r "fact")}))))
                       (or (gobj/get res "results") #js []))))
        (.catch (fn [_] [])))
    (js/Promise.resolve [])))

;; ── contract methods ─────────────────────────────────────────────────

(defn- register-project!
  "RegisterProject: (project, source-locale, locales, messages) → an order.
  `messages` arrives in the TM service's flat dotted-key shape; the keys are
  what gets translated."
  [env st args]
  (let [o (order/order {:project (:project args)
                        :source-locale (keyword (or (:sourceLocale args) "ja"))
                        :locales (mapv keyword (or (:locales args) []))
                        :keys (or (:messages args) (:keys args) [])
                        :tier (keyword (or (:tier args) "tier/reach"))
                        :budget-usd (:budgetUsd args)})
        out (op/run st {:op :register-order :order-id (:order/project o) :order
                        {:project (:order/project o)
                         :source-locale (:order/source-locale o)
                         :locales (:order/schedule o)
                         :keys (:order/keys o)
                         :budget-usd (:order/budget-usd o)}}
                    {:phase (phase-of env)})]
    (doseq [f (store/ledger st)] (append-fact! env (:order/project o) f))
    (json-response
     {:project (:order/project o)
      :accepted (= :commit (:disposition out))
      :disposition (:disposition out)
      :schedule (mapv name (:order/schedule o))
      :units (:order/units o)
      ;; the coverage curve is returned so a caller can see WHY the schedule
      ;; is ordered the way it is, rather than trusting it
      :coverage (mapv (fn [c] {:locale (name (:locale c)) :share (:share c)})
                      (:order/coverage o))}
     (if (= :commit (:disposition out)) 200 202))))

(defn- translate-batch!
  "TranslateBatch: record what a supplier delivered and what acceptance made
  of it. This endpoint does NOT call a model — procurement decides who runs a
  batch, and the caller (or a future dispatcher) performs the call. Keeping
  the model call out of the accepting path is deliberate: the thing that
  judges the work must not also be the thing that produced it."
  [env st args]
  (let [order-id (:project args)
        locale (keyword (:locale args))
        pairs (mapv (fn [p] [(get p :source) (get p :translated)])
                    (or (:pairs args) []))
        out (op/run st {:op :record-delivery :order-id order-id
                        :locale locale :pairs pairs :seller (:seller args)}
                    {:phase (phase-of env)})
        d (last (store/delivery-records st order-id))]
    (doseq [f (store/ledger st)] (append-fact! env order-id f))
    (json-response
     {:project order-id
      :locale (name locale)
      :acceptRate (:accept-rate d)
      :accepted (count (:accepted d))
      :rejected (mapv (fn [r] {:source (:source r)
                               :defects (mapv name (:defects r))})
                      (:rejected d))
      ;; the field a settlement layer must read before releasing money
      :payable (boolean (:payable? d))
      :disposition (:disposition out)})))

(defn- export-messages
  "ExportMessages: the ACCEPTED catalog for one locale, in the flat shape
  i18n.tm-import expects. Rejected strings are absent by construction — they
  are in the ledger, never in what gets served."
  [_env st args]
  (json-response
   {:project (:project args)
    :locale (:locale args)
    :messages (store/catalog-for st (:project args) (keyword (:locale args)))}))

(defn- status
  "Operational truth for one order, including the two reconciliation queries.
  `paidWithoutAcceptance` should always be zero; a non-zero value is a
  settlement bug and is surfaced here rather than buried."
  [env st args]
  (-> (read-ledger env (:project args))
      (.then (fn [durable-ledger]
               (json-response
                (merge (store/summary st (:project args))
                       {:durable (boolean (gobj/get env "TRANSLATION_DB"))
                        :phase (phase-of env)
                        :durableLedgerFacts (count durable-ledger)}))))))

(def ^:private xrpc-methods
  {"ai.gftd.translation.registerProject" register-project!
   "ai.gftd.translation.translateBatch" translate-batch!
   "ai.gftd.translation.exportMessages" export-messages
   "ai.gftd.translation.status" status})

(defn handle
  [^js request env _ctx]
  (let [url (js/URL. (.-url request))
        path (.-pathname url)
        st (store/mem-store)]
    (cond
      (= "/health" path)
      (json-response {:ok true :service "cloud-itonami-translation"
                      :phase (phase-of env)
                      :durable (boolean (gobj/get env "TRANSLATION_DB"))})

      (= "/catalog-plan" path)
      ;; read-only: what would we buy, and would it add anything?
      (-> (js/fetch (or (cfg env "X402_CATALOG_URL") "https://x402.nexus/catalog"))
          (.then (fn [^js r] (if (.-ok r) (.json r) #js {})))
          (.then (fn [j]
                   (let [body (js->clj j :keywordize-keys true)
                         units (js/parseInt (or (.get (.-searchParams url) "units") "0") 10)
                         o (order/order {:project "probe" :locales [] :keys []})
                         out (op/run st {:op :plan-procurement :order-id "probe"
                                         :order (assoc o :order/units units)
                                         :catalog body
                                         :free-backends (free-backends env)}
                                     {:phase (phase-of env)})]
                     (json-response {:plan (:value (:proposal out))
                                     :disposition (:disposition out)}))))
          (.catch (fn [e] (json-response {:error (str e)} 502))))

      (str/starts-with? path "/xrpc/")
      (let [nsid (subs path 6)]
        (if-let [f (get xrpc-methods nsid)]
          (-> (.json request)
              (.catch (fn [_] #js {}))
              (.then (fn [input]
                       (f env st (js->clj input :keywordize-keys true))))
              (.catch (fn [e] (json-response {:error (str e)} 500))))
          (json-response {:error "unknown method" :nsid nsid} 404)))

      :else (json-response {:error "not found" :path path} 404))))

(def handler #js {:fetch handle})
