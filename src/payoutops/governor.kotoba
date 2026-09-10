(ns payoutops.governor
  "PayoutGovernor — the independent compliance layer standing between a
  computed payout and a creator's bank account.

  The advisor has no notion of whether the arithmetic it produced actually
  conserves what backers were charged, whether the creator it wants to pay
  has a verified destination, whether the gate a tranche names has really
  been met, or whether its own `:effect` secretly claims to have already
  moved funds. So this MUST be a separate system able to *reject* a
  proposal and fall back to HOLD.

  ## Nothing here moves money

  A payout plan is a COMPUTATION — an auditable statement of who should
  receive what. `crowdfunding.payout` stamps `:payout/custodial? false` on
  every plan to say so, and this governor treats any claim to have
  *already* transferred, paid out or refunded funds as a HARD, permanent
  scope exclusion. A release, once a human approves it, RECORDS that
  authorisation; a rail adapter performs the actual transfer outside this
  actor.

  ## Why crowdfunding payout is not marketplace payout

  A marketplace seller ships and is then paid; a crowdfunding creator is
  paid and then ships, months later, using the money to do it. So the
  gates here are not delivery confirmations on a completed sale — they are
  milestones on a promise, and the ONLY ones that count are the ones this
  actor can derive from fulfilment records it holds. A gate an advisor
  asserts is not a gate.

  Seven HARD checks, ALL permanent, un-overridable by any human approval:

    1. Plan not conserved      -- fees + creator net must equal what
                                  backers were charged, and tranches must
                                  equal the net. Money that does not add up
                                  is never a confidence question, and it is
                                  checked here as well as inside
                                  `crowdfunding.payout` — two layers,
                                  because a rounding leak is silent.
    2. Destination unverified  -- re-derived from the STORE, never from the
                                  proposal's claim. Caught before approval
                                  rather than at execution, where an
                                  unverified destination is a payment
                                  already in flight.
    3. Gate not met            -- the tranche's gate is re-derived from
                                  fulfilment records. `:on-production-start`
                                  is never inferred: production having begun
                                  is not observable from fulfilment states,
                                  and inferring it would invent the very
                                  fact the holdback exists to require.
    4. Out of order            -- releasing a later tranche before an
                                  earlier one. Otherwise the holdback
                                  ordering backers read on the campaign
                                  page was decorative.
    5. Held tranche            -- releasing a `:held` tranche is a HARD
                                  block. The ONLY way out of `:held` is a
                                  named human's decision through
                                  `crowdfunding.payout/resolve-hold`.
    6. Effect not :propose     -- any other value is a claim to directly
                                  actuate outside governance.
    7. Scope exclusion         -- any claim to have moved funds, plus any op
                                  outside the closed allowlist.

  Five ESCALATE (SOFT) gates, plus low confidence:
    `:bind-payout-destination`, `:release-tranche`, `:hold-tranche`,
    `:plan-refund` and `:flag-payout-concern` ALWAYS escalate. Binding
    where a creator's money goes is the money-equivalent of issuing an
    identity; authorising a release is the moment money leaves; a hold
    stops someone's livelihood; a refund plan decides who eats the
    processing fees. `payoutops.phase` keeps all five out of every phase's
    `:auto` set independently — two layers, not one."
  (:require [kotoba.lang.text :as str]
            [crowdfunding.payout :as payout]
            [marketplace.settlement :as settle]
            [payoutops.store :as store]))

(def confidence-floor 0.6)

(def allowed-ops
  "The closed proposal-op allowlist. CRITICAL: no op that directly
  transfers funds is EVER a member — such an op would be a permanent scope
  violation, not merely un-implemented. `:release-tranche` AUTHORISES a
  transfer for a rail to perform; it does not perform one."
  #{:plan-payout :bind-payout-destination :release-tranche :hold-tranche
    :plan-refund :flag-payout-concern})

(def always-escalate-ops
  #{:bind-payout-destination :release-tranche :hold-tranche
    :plan-refund :flag-payout-concern})

(def payable-states
  "Campaign states from which a creator may be paid. `:collecting` is
  absent: money still arriving is money that can still fail to arrive."
  #{:fulfilling :completed})

(def scope-excluded-terms
  "Case-insensitive substrings marking a proposal as claiming to have
  moved money.

  CRITICAL: every term is phrased as the COMPLETED transfer ('paid out the
  creator'), never a bare noun like 'payout' or 'tranche' — a bare noun
  would match inside this actor's own legitimate proposals (whose whole job
  is to talk about payouts) and self-block the happy path. See
  `payoutops.governor-test/default-mock-advisor-proposals-never-self-trip-scope-exclusion`."
  ["paid out the creator" "paid the creator" "have paid out"
   "transferred the funds" "transferred the money" "have transferred"
   "executed the payout" "executed the transfer" "completed the payout"
   "released the funds to" "moved the funds" "disbursed the funds"
   "refunded the backers" "have refunded" "resolved the payment dispute"
   "送金した" "送金を実行した" "支払いを実行した" "払い出した"
   "資金を移動した" "決済を完了した" "返金した" "支払い紛争を解決した"])

;; ----------------------------- checks -----------------------------

(defn- plan-of
  "The plan a proposal is about: the one it carries, or the one this store
  computes for the campaign it names."
  [proposal st]
  (or (get-in proposal [:value :plan])
      (some->> (get-in proposal [:value :campaign-id]) (store/plan-for st))))

(defn- creator-of [st cid]
  (:campaign/creator (store/campaign-of st cid)))

(defn- payable-violations
  [proposal st]
  (when (contains? #{:plan-payout :release-tranche} (:op proposal))
    (let [cid (get-in proposal [:value :campaign-id])
          c   (store/campaign-of st cid)]
      (cond
        (nil? c) [{:rule :campaign-unknown :detail (str (or cid "(id missing)") " は存在しない")}]
        (not (contains? payable-states (:campaign/state c)))
        [{:rule :not-payable
          :detail (str "状態 " (pr-str (:campaign/state c))
                       " では支払わない（回収中の資金はまだ失われうる）")}]))))

(defn- plan-violations
  "Conservation and destination, delegated to
  `crowdfunding.payout/plan-errors` with the destination read FROM THE
  STORE."
  [proposal st]
  (when (contains? #{:plan-payout :release-tranche} (:op proposal))
    (let [cid (get-in proposal [:value :campaign-id])
          p   (plan-of proposal st)
          d   (some->> (creator-of st cid) (store/destination st))]
      (if (nil? p)
        [{:rule :plan-missing :detail "対象の支払計画が特定できない"}]
        (when-let [errs (seq (payout/plan-errors p d))]
          (mapv (fn [e] {:rule   (or (:payout.error/code e) (:settlement.error/code e))
                         :detail (or (:payout.error/detail e)
                                     (:settlement.error/detail e)
                                     (name (or (:payout.error/code e)
                                               (:settlement.error/code e))))})
                errs))))))

(defn- release-violations
  "A release requires a real tranche, a non-held state, a gate that this
  store can DERIVE as met, and every earlier tranche already cleared."
  [proposal st now]
  (when (= :release-tranche (:op proposal))
    (let [cid  (get-in proposal [:value :campaign-id])
          tid  (get-in proposal [:value :tranche-id])
          ts   (store/tranches st cid)
          t    (first (filter #(= tid (:tranche/id %)) ts))
          facts (store/gate-facts st cid now)]
      (cond
        (empty? ts)
        [{:rule :payout-plan-not-committed
          :detail "支払計画が未確定のまま解放はできない"}]

        (nil? t)
        [{:rule :tranche-unknown
          :detail (str (or tid "(tranche-id missing)") " という分割は存在しない")}]

        (= :held (:tranche/state t))
        [{:rule :tranche-held
          :detail "留保中の分割の解放は永久に禁止 -- 人間の裁定のみが :held を抜けられる"}]

        (not= :pending (:tranche/state t))
        [{:rule :tranche-not-pending
          :detail (str "状態 " (pr-str (:tranche/state t)) " の分割は解放できない")}]

        (not (payout/gate-satisfied? t facts))
        [{:rule :gate-not-met
          :detail (str "解放条件 " (pr-str (:tranche/gate t))
                       " は未充足（充足済み: " (pr-str facts) "）")}]

        ;; Every EARLIER tranche must already be settled. Note this is a
        ;; stricter question than `crowdfunding.payout/releasable`, which
        ;; answers "which tranches could be released now" and legitimately
        ;; returns several at once. Releasing a later one while an earlier
        ;; one is still owed is not forbidden by the gates — it is
        ;; forbidden because the ordering is what backers read on the
        ;; campaign page, and an ordering that can be skipped was
        ;; decorative.
        (let [earlier (take-while #(not= tid (:tranche/id %)) ts)]
          (some #(not (contains? #{:released :cancelled} (:tranche/state %))) earlier))
        [{:rule :out-of-order
          :detail (str "先行する分割("
                       (str/join "," (->> (take-while #(not= tid (:tranche/id %)) ts)
                                          (remove #(contains? #{:released :cancelled}
                                                              (:tranche/state %)))
                                          (map :tranche/id)))
                       ") が未解放のまま後続は解放できない")}]))))

(defn- destination-violations
  "For `:bind-payout-destination`: the drafted destination must itself be
  structurally sound. Note this check does NOT accept the proposal's own
  `:payout/verified?` as proof of anything — verification is an
  out-of-band act, and the human approving this binding is the one
  asserting it happened."
  [proposal]
  (when (= :bind-payout-destination (:op proposal))
    (let [d (get-in proposal [:value :destination])]
      (if-not (map? d)
        [{:rule :destination-missing :detail "支払先の草案がない"}]
        (when-let [errs (seq (remove #(= :payout-destination-unverified
                                         (:settlement.error/code %))
                                     (settle/payout-destination-errors d)))]
          (mapv (fn [e] {:rule (:settlement.error/code e)
                         :detail (or (:settlement.error/detail e)
                                     (name (:settlement.error/code e)))})
                errs))))))

(defn- effect-not-propose-violations [proposal]
  (when (not= :propose (:effect proposal))
    [{:rule :effect-not-propose
      :detail (str ":effect は :propose のみ許可されるが " (pr-str (:effect proposal)) " が提案された")}]))

(defn- text-blob [proposal]
  (str/lower (pr-str (select-keys proposal [:op :summary :rationale :cites]))))

(defn- scope-exclusion-violations [proposal]
  (let [op (:op proposal)
        blob (text-blob proposal)]
    (cond
      (not (contains? allowed-ops op))
      [{:rule :op-not-allowed
        :detail (str (pr-str op) " は許可された操作(closed allowlist)に含まれない")}]

      (some #(str/includes? blob %) scope-excluded-terms)
      [{:rule :scope-excluded
        :detail "送金・払い出し・返金・紛争解決を実行済みと主張する提案は永久に禁止"}])))

(defn check
  "Censors a PayoutAdvisor proposal. `context` supplies `:now` (ISO-8601
  UTC) — this governor has no clock of its own.

  Returns {:ok? bool :violations [..] :confidence c :escalate? bool
           :high-stakes? bool :hard? bool}."
  [_request context proposal store]
  (let [now  (:now context)
        hard (into []
                   (concat (payable-violations proposal store)
                           (plan-violations proposal store)
                           (release-violations proposal store now)
                           (destination-violations proposal)
                           (effect-not-propose-violations proposal)
                           (scope-exclusion-violations proposal)))
        conf (:confidence proposal 0.0)
        low? (< conf confidence-floor)
        stakes? (boolean (always-escalate-ops (:op proposal)))
        hard? (boolean (seq hard))]
    {:ok?          (and (not hard?) (not low?) (not stakes?))
     :violations   hard
     :confidence   conf
     :hard?        hard?
     :escalate?    (and (not hard?) (or low? stakes?))
     :high-stakes? stakes?}))

(defn hold-fact [request context verdict]
  {:t           :governor-hold
   :op          (:op request)
   :actor       (:actor-id context)
   :campaign-id (:campaign-id request)
   :disposition :hold
   :basis       (mapv :rule (:violations verdict))
   :violations  (:violations verdict)
   :confidence  (:confidence verdict)})
