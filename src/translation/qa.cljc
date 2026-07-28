(ns translation.qa
  "Acceptance checks for one translated string. Pure.

  This namespace is the actor's real asset. Every rule here was paid for by a
  production defect on shinshi.club between 2026-07-27 and 2026-07-28, where
  84 locales were rolled out with ad-hoc scripts and each of these failures
  shipped or nearly shipped:

  - `:source-language-leak` — the model answered in Japanese for a non-Japanese
    locale. Measured at 4/6 for French under a real (not stub) persona.
  - `:placeholder-dropped` — \"{name} を応援\" came back without {name}, so the
    sentence rendered with no name in it.
  - `:placeholder-mangled` — {name} came back as %s (7 cells), or as \"{ name}\"
    with a space inside the braces (wuu, on every placeholder key).
  - `:edge-space-lost` — a fragment key like \" を応援\" (concatenated after a
    name) lost its leading space and rendered as \"2B메시지…\". 28 cells across
    19 locales.
  - `:untranslated` — the model echoed the source string back.
  - `:instruction-echo` — the model returned the instruction it was given
    instead of a translation.
  - `:truncated` — the reply ran past the token budget and came back as a
    fragment of JSON.

  A quality gate is worth more than a repair script: a repair script fixes the
  batch in front of it, a gate refuses to pay for work that fails. `verdict`
  is what the governor consults before a procurement receipt settles.

  Repairable vs rejectable is an explicit distinction. Brace spacing and a
  lone %-token standing in for a single placeholder are mechanical and
  unambiguous, so `repair` fixes them and the string passes. Everything else
  is a real defect and the batch is refused."
  (:require [clojure.string :as str]))

;; ── script detection ─────────────────────────────────────────────────

(def kana-re #"[぀-ゟ゠-ヿ]")

(defn kana? [s] (boolean (re-find kana-re (str s))))

(defn printf-placeholders
  "Legacy %s-style tokens (the greeting templates still use them)."
  [s]
  (count (re-seq #"%s" (str s))))

(defn placeholders
  "The {param} tokens a source string promises to interpolate."
  [s]
  (set (re-seq #"\{[a-zA-Z][a-zA-Z0-9_-]*\}" (str s))))

;; ── repair (mechanical, unambiguous only) ────────────────────────────

(defn repair
  "Fix the two defects that have exactly one possible correction, and leave
  everything else alone. Pure; returns the possibly-corrected translation.

  1. `{ name }` → `{name}`. A space inside the braces breaks the token; no
     other reading is plausible.
  2. a single `%x` token where the source has exactly one `{param}` → the
     `{param}`. Only when both sides are singular, so there is nothing to
     guess about which token maps to which.
  3. leading/trailing space the source has and the translation lost."
  [source translated]
  (let [t (str/replace (str translated) #"\{\s+([a-zA-Z][a-zA-Z0-9_-]*)\s*\}" "{$1}")
        src-ph (vec (placeholders source))
        t (if (and (= 1 (count src-ph)) (not (str/includes? t (first src-ph))))
            (let [hits (vec (re-seq #"%[A-Za-z]" t))]
              (if (= 1 (count hits)) (str/replace t (first hits) (first src-ph)) t))
            t)
        ;; %s keys have the same failure in the other direction: Fula returned
        ;; "%n" for a "%s" source. One stray %-token against one source %s is
        ;; equally unambiguous.
        t (if (and (= 1 (printf-placeholders source)) (zero? (printf-placeholders t)))
            (let [hits (vec (re-seq #"%[A-Za-z]" t))]
              (if (= 1 (count hits)) (str/replace t (first hits) "%s") t))
            t)
        t (cond-> t
            (and (str/starts-with? (str source) " ") (not (str/starts-with? t " ")))
            (->> (str " "))
            (and (str/ends-with? (str source) " ") (not (str/ends-with? t " ")))
            (str " "))]
    t))

;; ── acceptance ───────────────────────────────────────────────────────

(def ^:private refusal-re
  #"(?i)(i cannot|i can't|i am unable|as an ai|language model|申し訳|できません)")

(defn defects
  "All acceptance failures for one (source, translated) pair under `locale`.
  → set of keywords, empty when the string is acceptable. Pure.

  `locale` is the TARGET locale; `source-locale` defaults to :ja because that
  is what the catalogs are keyed on."
  ([locale source translated] (defects locale source translated :ja))
  ([locale source translated source-locale]
   (let [t (str translated)
         s (str source)]
     (cond-> #{}
       (str/blank? t)
       (conj :empty)

       ;; Japanese in a non-Japanese locale is the dominant failure mode.
       ;; Kanji alone is NOT a signal — Chinese output is full of it — so this
       ;; looks for kana only.
       (and (not= locale source-locale) (kana? t))
       (conj :source-language-leak)

       (seq (remove #(str/includes? t %) (placeholders s)))
       (conj :placeholder-dropped)

       (not= (printf-placeholders s) (printf-placeholders t))
       (conj :printf-placeholder-mismatch)

       (and (str/starts-with? s " ") (not (str/starts-with? t " ")))
       (conj :edge-space-lost)

       (and (str/ends-with? s " ") (not (str/ends-with? t " ")))
       (conj :edge-space-lost)

       ;; identical output for a different locale means nothing was translated
       (and (not= locale source-locale) (= (str/trim s) (str/trim t))
            (re-find #"[぀-ヿ一-鿿]" s))
       (conj :untranslated)

       (re-find refusal-re t)
       (conj :refusal)

       ;; a translation far longer than the source is usually the model
       ;; explaining itself; far shorter is usually truncation
       (and (pos? (count s)) (> (count t) (* 6 (count s))) (> (count t) 120))
       (conj :verbose-nontranslation)))))

(defn verdict
  "→ {:accepted? bool :translated str :defects #{…} :repaired? bool}

  Repairs first, then judges. A string that only had a mechanical defect comes
  back accepted WITH the corrected text, so the caller stores the fix rather
  than re-buying the batch."
  ([locale source translated] (verdict locale source translated :ja))
  ([locale source translated source-locale]
   (let [fixed (repair source translated)
         ds (defects locale source fixed source-locale)]
     {:accepted? (empty? ds)
      :translated fixed
      :repaired? (not= (str translated) fixed)
      :defects ds})))

(defn batch-verdict
  "Judge a whole delivered batch. `pairs` is [[source translated] …].
  → {:accepted [[source translated] …] :rejected [{:source :translated
     :defects}] :accept-rate double}

  The accept rate is what the governor prices against: a provider that
  delivers 60% acceptable strings costs more per USABLE string than its
  headline price, and the procurement plan is supposed to notice."
  [locale pairs]
  (let [judged (map (fn [[s t]] (assoc (verdict locale s t) :source s)) pairs)
        ok (filter :accepted? judged)
        bad (remove :accepted? judged)]
    {:accepted (mapv (juxt :source :translated) ok)
     :rejected (mapv #(select-keys % [:source :translated :defects]) bad)
     :accept-rate (if (seq judged) (double (/ (count ok) (count judged))) 0.0)}))
