(ns translation.publish
  "Slice d — the publish bridge. Accepted translation artifacts (messages
  catalogs) leave this actor the same way shipped translations should: as a
  content-addressed i18n-cid DAG-CBOR block, pinned to IPFS, and admitted to
  the kotobase-lake catalog plane so the datom plane can query what was
  published.

  Flow ([[publish-catalog!]]):

      governor gate (review-publication)
        → i18n-cid.core/catalog->block      (locale catalog → DAG-CBOR block)
        → block-store put!                   (caller's block store)
        → locale-index->block                (project index for this locale)
        → IPFS PSA pin! (`POST /pins`)       (render-pin seam, post-fn)
        → i18n-cid.core/lake-claim           (admit catalog to kotobase-lake)
        → lake-claim-index                   (admit the locale index)

  The governor gate is first and it is the whole point: **nothing below it
  runs for a catalog the TranslationGovernor refused to publish.** A short
  catalog is publishable only as :hold (it is not a fulfilled order), and a
  refused locale is never stored, pinned, or admitted. That mirrors the core
  invariant from translation.governor — the governor decides what settles.

  Honesty boundary (same as i18n-cid.core): this namespace encodes, pins, and
  admits; it does not do the HTTP itself. The block store and the pin
  transport are injected seams (`:put-block-fn`, `:post-fn`) exactly as
  i18n-cid asks callers to bring their own `put!`/`get-fn`, so every step is
  pure and unit-testable without a socket. `:ingested-at` is supplied by the
  caller because a pure core must not read a clock."
  (:require [i18n-cid.core :as cid]
            [translation.governor :as gov]))

;; ── PSA pin seam (render-pin) ────────────────────────────────────────────
;; i18n-cid.pin (Piece 3) is not merged at the pinned i18n-cid HEAD, so the
;; pin request is constructed here against the same kotobase-protocol-pinning
;; `/pins` PSA surface. Purposely mirrors i18n-cid.pin's shape so it can be
;; deleted wholesale for `cid/pin!` when Piece 3 lands.

(defn pin-request
  "Build the PSA `POST /pins` request body for `cid`. Returns
  `{:path \"/pins\" :body {:cid .. :name ..?}}`. `name` (IPNS name to publish
  under) is optional and only included when supplied."
  [cid {:keys [name]}]
  {:path "/pins"
   :body (cond-> {:cid cid}
           name (assoc :name name))})

(defn- request-id-of
  "The PSA contract returns either `{\"id\" ..}` or `{\"requestid\" ..}`; accept
  both and throw when neither is present, so a non-pin response can never be
  mistaken for a pinned catalog."
  [resp]
  (let [id (or (get resp "id") (get resp "requestid")
               (get resp :id) (get resp :requestid))]
    (when-not id
      (throw (ex-info "pin response carried no request-id" {:response resp})))
    (str id)))

(defn psa-pin!
  "Pin the catalog block at `cid` through the injected `:post-fn`
  (`(fn [path body] -> parsed-JSON-response)`). Returns
  `{:cid .. :request-id .. :pinned? true}`."
  [post-fn cid opts]
  (let [{:keys [path body]} (pin-request cid opts)
        resp (post-fn path body)]
    {:cid cid
     :request-id (request-id-of resp)
     :pinned? true}))

;; ── block-store / lake seams ────────────────────────────────────────────

(def ^:private default-block-put
  "Total silence: when no store is injected, drop the bytes. Used only by the
  pure core when a caller wants to build+inspect blocks without persisting."
  (fn [_cid _bytes] nil))

;; ── the bridge ──────────────────────────────────────────────────────────

(defn publish-catalog!
  "Publish ONE locale's accepted `catalog` as an i18n-cid block, pinned and
  admitted — but only if the TranslationGovernor's publication gate accepts it.

  `ctx` carries everything the caller controls:
    :project       keyword — project name (the index block's `project`)
    :required-keys seq     — keys the app asks for (governor publication gate)
    :tenant        string  — kotobase-lake tenant
    :ingested-at   string  — caller-supplied timestamp (a pure core has no clock)
    :put-block-fn  fn      — `(fn [cid bytes] -> any)`, the block store seam
    :post-fn       fn      — `(fn [path body] -> response)`, the IPFS PSA seam
    :policy        map?    — optional translation.governor policy override

  Refusal (governor says :hold/:reject) returns `{:published? false :decision
  .. :reason .. :missing ..}` and touches NO seam — no block is stored, no pin
  is requested, nothing is admitted. A :commit returns the published artifact:
  the locale catalog CID, the locale index block CID, and the pin + lake
  claims. `:steps` records the exact code path taken, in order."
  [{:keys [project required-keys tenant ingested-at put-block-fn post-fn]}
   locale catalog]
  (let [gate (gov/review-publication {:required-keys required-keys
                                      :catalog catalog})]
    (if (not= :commit (:decision gate))
      (merge {:published? false} (select-keys gate [:decision :reason :missing]))
      (let [put (or put-block-fn default-block-put)
            cat (cid/catalog->block catalog)
            ;; one-locale project index so the release is queryable as a whole
            idx (cid/locale-index->block project {locale (:cid cat)})
            _ (put (:cid cat) (:bytes cat))
            _ (put (:cid idx) (:bytes idx))
            pin (psa-pin! post-fn (:cid cat)
                          {:name (str (name project) "/" (name locale))})
            lake (cid/lake-claim {:cid (:cid cat)
                                  :size (count (:bytes cat))
                                  :tenant tenant
                                  :ingested-at ingested-at
                                  :locale locale})
            ilk (cid/lake-claim-index {:cid (:cid idx)
                                       :size (count (:bytes idx))
                                       :tenant tenant
                                       :ingested-at ingested-at
                                       :project project})]
        {:decision      :commit
         :published?    true
         :steps         [:catalog-block :store :locale-index :pin :lake-claim :index-lake-claim]
         :cid           (:cid cat)
         :bytes         (count (:bytes cat))
         :index-cid     (:cid idx)
         :pin           pin
         :lake          lake
         :index-lake    ilk}))))

(defn publish-project!
  "Publish every locale of `by-locale` ({locale → accepted-catalog}) under one
  project locale-index. Each locale is individually governor-gated (refused
  locales are excluded and never published); the shared project index then
  links every published locale's CID.

  Returns `{:published {locale -> result} :refused {locale -> refusal}
  :index-cid .. :index-pin .. :index-lake ..}`."
  [{:keys [project tenant ingested-at put-block-fn post-fn] :as ctx}
   by-locale]
  ;; single pass: each locale is published exactly once and its result lands in
  ;; one partition — never double-invoke the seams for a locale (that would pin
  ;; and admit the same catalog twice)
  (let [results (into {} (map (fn [[locale catalog]]
                                [locale (publish-catalog! ctx locale catalog)])
                              by-locale))
        published (into {} (filter (fn [[_ r]] (:published? r)) results))
        refused   (into {} (filter (fn [[_ r]] (not (:published? r))) results))
        put       (or put-block-fn default-block-put)
        {:keys [cid bytes]}
        (cid/locale-index->block project (into {} (map (fn [[l r]] [l (:cid r)])) published))
        _ (put cid bytes)
        pin  (psa-pin! post-fn cid {:name (str (name project) "/locales")})
        lake (cid/lake-claim-index {:cid cid
                                    :size (count bytes)
                                    :tenant tenant
                                    :ingested-at ingested-at
                                    :project project})]
    {:published published
     :refused   refused
     :index-cid cid
     :index-pin pin
     :index-lake lake}))