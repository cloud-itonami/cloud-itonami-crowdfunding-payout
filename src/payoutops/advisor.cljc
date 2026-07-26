(ns payoutops.advisor
  "PayoutAdvisor — the *contained intelligence node* for the payout actor.

  It drafts exactly six kinds of proposal from a closed allowlist:
  computing a payout plan, binding a creator's payout destination,
  releasing a tranche, holding one, planning a refund, and flagging a
  concern.

  CRITICAL: it is a smart-but-untrusted advisor. Every proposal's
  `:effect` is always `:propose`; nothing here moves funds. Every output
  is censored downstream by `payoutops.governor`.

  Note what the advisor does NOT decide. The ARITHMETIC is not its
  opinion: `plan-payout` calls `crowdfunding.payout/payout-plan`, a pure
  integer allocation over the platform's published fee schedule and the
  tranche schedule backers were shown, and the governor independently
  re-runs the conservation checks. An advisor cannot pay a creator more
  by being persuasive — it can only propose a campaign to run the
  arithmetic over.

  Like every sibling actor's advisor this is a deterministic mock so the
  actor graph runs offline. In production this calls a real LLM with the
  same proposal shape."
  (:require [crowdfunding.payout :as payout]
            [payoutops.store :as store]))

(defprotocol Advisor
  (-advise [advisor store request] "store + request -> proposal map"))

(defn- propose-plan
  [st {:keys [campaign-id]}]
  (let [p (store/plan-for st campaign-id)]
    {:op          :plan-payout
     :campaign-id campaign-id
     :summary     (str campaign-id " の支払計画: 請求総額 " (:payout/charged-minor p)
                       " / 手数料 " (:payout/platform-minor p)
                       " / 創作者受取 " (:payout/creator-net-minor p))
     :rationale   "公開済み手数料表と支援者に示した分割表に基づく配分計算の提示のみ。資金の移動は行わない。"
     :cites       [campaign-id]
     :effect      :propose
     :value       {:campaign-id campaign-id :plan p}
     :confidence  0.94}))

(defn- propose-destination
  "Draft a payout destination binding. ALWAYS escalates — deciding where a
  creator's money goes is the money-equivalent of issuing an identity."
  [_st {:keys [campaign-id patch]}]
  (let [d (payout/payout-destination (select-keys patch [:creator :rail :address :verified?]))]
    {:op          :bind-payout-destination
     :campaign-id campaign-id
     :summary     (str (:payout/seller d) " の支払先の登録を提案 ("
                       (name (or (:payout/rail d) :unknown)) ")")
     :rationale   "支払先候補の提示のみ。実在性・所有者の確認は人間が行い、その確認をもって承認する。"
     :cites       [(:payout/seller d)]
     :effect      :propose
     :value       {:campaign-id campaign-id :destination d}
     :confidence  (or (:confidence patch) 0.8)}))

(defn- propose-release
  "Authorise a tranche release. ALWAYS escalates — this is the moment
  money actually leaves. The rationale describes the AUTHORISATION, never
  a completed transfer, so it never trips `scope-excluded-terms`."
  [_st {:keys [campaign-id patch]}]
  {:op          :release-tranche
   :campaign-id campaign-id
   :summary     (str campaign-id " の分割 " (:tranche-id patch) " の解放承認を提案")
   :rationale   "解放条件の充足を根拠とする承認依頼のみ。実際の振替は承認後に決済レールが行う。"
   :cites       (vec (keep identity [campaign-id (:tranche-id patch)]))
   :effect      :propose
   :value       {:campaign-id campaign-id :tranche-id (:tranche-id patch)}
   :confidence  (or (:confidence patch) 0.87)})

(defn- propose-hold
  [_st {:keys [campaign-id patch]}]
  {:op          :hold-tranche
   :campaign-id campaign-id
   :summary     (str campaign-id " の分割 " (:tranche-id patch) " の留保を提案: "
                     (pr-str (:reason patch :unstated)))
   :rationale   "観察された事実に基づく留保の承認依頼のみ。留保の解除は人間の裁定でしか行えない。"
   :cites       (vec (keep identity [campaign-id (:tranche-id patch)]))
   :effect      :propose
   :value       {:campaign-id campaign-id :tranche-id (:tranche-id patch)
                 :reason (:reason patch) :at (:at patch)}
   :confidence  (or (:confidence patch) 0.82)})

(defn- propose-refund
  "Draft a refund plan. ALWAYS escalates. Its most important field is
  `:refund/creator-owes-minor` — what the creator must find BEYOND the
  funds they still hold, because processing fees generally do not come
  back from the processor."
  [st {:keys [campaign-id patch]}]
  (let [r (payout/refund-plan {:pledges      (store/pledges-for st campaign-id)
                               :rewards      (store/rewards st campaign-id)
                               :fee-schedule (store/fee-schedule st)
                               :reason       (:reason patch)
                               :requested-by (:requested-by patch)
                               :requested-at (:at patch)})]
    {:op          :plan-refund
     :campaign-id campaign-id
     :summary     (str campaign-id " の返金計画: 返金 " (:refund/gross-minor r)
                       " / 創作者の追加負担 " (:refund/creator-owes-minor r))
     :rationale   "返金額と負担の内訳の提示のみ。返金の実行と紛争の裁定は行わない。"
     :cites       [campaign-id]
     :effect      :propose
     :value       {:campaign-id campaign-id :refund r}
     :confidence  0.9}))

(defn- propose-concern
  [_st {:keys [campaign-id patch]}]
  {:op          :flag-payout-concern
   :campaign-id campaign-id
   :summary     (str campaign-id " に関する支払上の懸念フラグ: "
                     (pr-str (:concern patch "unknown")))
   :rationale   "観察された懸念事実の報告のみ。解放・返金・紛争の裁定は行わない。"
   :cites       [campaign-id]
   :effect      :propose
   :value       (merge {:campaign-id campaign-id} patch)
   :confidence  (or (:confidence patch) 0.78)})

(defn infer
  [st {:keys [op out-of-scope?] :as request}]
  (let [proposal (case op
                   :plan-payout             (propose-plan st request)
                   :bind-payout-destination (propose-destination st request)
                   :release-tranche         (propose-release st request)
                   :hold-tranche            (propose-hold st request)
                   :plan-refund             (propose-refund st request)
                   :flag-payout-concern     (propose-concern st request)
                   {})]
    ;; Test hook: inject scope-excluded content to exercise the
    ;; governor's scope-exclusion block end-to-end. Clear before
    ;; production use.
    (if out-of-scope?
      (update proposal :rationale str
              " -- actually paid out the creator and transferred the funds")
      proposal)))

(defn trace [_request proposal]
  {:t           :advisor-proposal
   :op          (:op proposal)
   :campaign-id (:campaign-id proposal)
   :summary     (:summary proposal)
   :confidence  (:confidence proposal)})

(defn mock-advisor []
  (reify Advisor
    (-advise [_ store request]
      (infer store request))))
