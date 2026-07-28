(ns translation.order
  "Orders, tiers and scheduling. Pure.

  The demand side. A translation order is (project, source-locale, target
  locales, keys) and the only interesting decision is ORDER OF DELIVERY, which
  is a product decision, not a technical one.

  Why that matters enough to be its own namespace (measured 2026-07-28): a
  1,259-key x 83-locale job runs for ~9 hours, and for those 9 hours the
  operator has NOTHING — every locale is partial. Delivering locales in
  reach order instead turns the same 9 hours into a curve: Spanish, Arabic,
  Portuguese, Indonesian and Russian — 1.9 billion speakers between them —
  land in the first ~25 minutes. The total is unchanged. What changes is that
  the job becomes interruptible without being wasted.

  `:tier/reach` is therefore the default, and `:tier/uniform` (the old
  behaviour: alphabetical, all-or-nothing) is kept only so the difference is
  measurable rather than asserted."
  (:require [clojure.string :as str]))

;; Speaker counts (L1+L2, Ethnologue 2026 order of magnitude). Used ONLY for
;; ordering, never displayed as fact — the ranking is stable even where the
;; absolute numbers are contested.
(def reach
  {:en 1493 :zh 1183 :hi 611 :es 561 :ar 335 :fr 334 :bn 274 :pt 269 :id 255
   :ur 246 :ru 210 :de 133 :ja 126 :yue 85 :wuu 83 :mr 99 :vi 97 :te 96
   :sw 95 :ha 94 :tr 94 :tl 87 :ta 86 :ko 82 :jv 69 :fa 65 :it 60 :gu 58
   :bho 53 :my 43 :ps 43 :pl 41 :uk 39 :om 37 :ff 37 :uz 35 :mai 34 :sd 33
   :su 32 :ne 32 :ig 31 :ku 30 :zu 28 :am 26 :ceb 25 :mg 25 :nl 25 :az 24
   :ro 24 :zh-tw 23 :si 22 :so 22 :ms 20 :ln 20 :xh 19 :km 18 :hne 18 :kn 18
   :sr 18 :kk 17 :rw 15 :ny 14 :tn 14 :ml 13 :el 13 :hu 13 :cs 13 :sv 13
   :ht 13 :st 13 :mag 13 :bg 12 :pa 12 :hr 11 :ug 11 :sn 11 :ak 11 :awa 11
   :ti 10 :ks 10 :bal 10 :or 10 :as 10})

(defn- reach-of [locale] (get reach (keyword (name locale)) 0))

(defn schedule
  "Delivery order for `locales` under `tier`.

  :tier/reach    — most speakers first. Default. Value accrues from minute one.
  :tier/uniform  — stable alphabetical. All-or-nothing; kept for comparison.
  :tier/listed   — exactly as the caller listed them (an explicit priority)."
  ([locales] (schedule locales :tier/reach))
  ([locales tier]
   (let [ls (vec locales)]
     (case tier
       :tier/listed ls
       :tier/uniform (vec (sort-by name ls))
       (vec (sort-by (juxt (comp - reach-of) name) ls))))))

(defn coverage-curve
  "Cumulative speaker reach after each locale of a schedule.
  → [{:locale kw :cumulative-millions int :share double} …]

  This is the artifact that makes the scheduling decision arguable instead of
  a matter of taste: it says what fraction of total addressable reach is
  already delivered N locales in."
  [scheduled]
  (let [total (reduce + 0 (map reach-of scheduled))]
    (first
     (reduce (fn [[acc run] l]
               (let [run' (+ run (reach-of l))]
                 [(conj acc {:locale l
                             :cumulative-millions run'
                             :share (if (pos? total) (double (/ run' total)) 0.0)})
                  run']))
             [[] 0] scheduled))))

(defn locales-for-share
  "The shortest prefix of `scheduled` covering `share` (0..1) of total reach.
  Answers 'what do I have to finish to be 80% done in the way that matters?'"
  [scheduled share]
  (let [curve (coverage-curve scheduled)]
    (mapv :locale (take (inc (count (take-while #(< (:share %) share) curve)))
                        curve))))

(defn order
  "Normalise an order. Pure.

  `keys` are the source strings to translate; the catalog is keyed on them, so
  they double as the message keys (a gettext-style source-as-key catalog)."
  [{:keys [project source-locale locales keys tier budget-usd]}]
  (let [tier (or tier :tier/reach)
        scheduled (schedule (or locales []) tier)]
    {:order/project (str project)
     :order/source-locale (or source-locale :ja)
     :order/tier tier
     :order/schedule scheduled
     :order/keys (vec (remove str/blank? (map str (or keys []))))
     :order/units (* (count scheduled) (count (or keys [])))
     :order/budget-usd budget-usd
     :order/coverage (coverage-curve scheduled)}))
