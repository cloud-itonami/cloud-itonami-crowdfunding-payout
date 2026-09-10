(ns payoutops.store
  "SSoT for the payout actor — what a creator is owed, where it may be
  sent, and which gates have actually been met.

  Directories, all keyed by STRING ids (never keywords):

    campaigns     campaign id -> campaign record
    pledges       campaign id -> pledges (for the fee computation)
    rewards       campaign id -> reward tiers (to price them)
    destinations  creator id  -> payout destination. `:payout/verified?`
                  on these records is the ONLY thing the governor trusts;
                  a proposal claiming a destination is verified is
                  ignored. Binding one is the money-equivalent of issuing
                  an identity, so it is always human-gated.
    schedules     campaign id -> tranche schedule the backers were shown
    tranches      campaign id -> the tranche records and their states
    fulfillment   campaign id -> fulfilment lines, from which the release
                  gates are DERIVED rather than asserted
    production    campaign id -> creator-supplied production evidence, if
                  any. Absent means the `:on-production-start` gate is
                  simply not met — production having begun is not
                  observable from fulfilment states, and inferring it
                  would invent the fact the holdback exists to require.

  This store holds NO money and no keys. It records intentions and
  outcomes; moving funds is a rail's job, and only ever after a human
  approves a release.

  The ledger stays append-only."
  (:require [crowdfunding.campaign :as cf]
            [crowdfunding.collection :as collection]
            [crowdfunding.fee :as fee]
            [crowdfunding.fulfillment :as ff]
            [crowdfunding.payout :as payout]
            [crowdfunding.pledge :as pledge]
            [crowdfunding.reward :as reward]))

(defprotocol Store
  (campaign-of [s id])
  (pledges-for [s campaign-id])
  (rewards [s campaign-id])
  (destination [s creator] "Verified payout destination, or nil.")
  (all-destinations [s])
  (schedule [s campaign-id] "The tranche schedule backers were shown.")
  (tranches [s campaign-id] "Tranche records with their current states.")
  (fulfillment-lines [s campaign-id])
  (production-evidence [s campaign-id])
  (fee-schedule [s] "The platform's published fee schedule.")
  (platform [s] "The platform's own identity.")
  (ledger [s])
  (payout-log [s])
  (commit-record! [s record])
  (append-ledger! [s fact]))

;; ----------------------------- demo data -----------------------------

(def ^:private risks "Tooling is not finalised; the controller is single-source.")

(defn- a-campaign [id state]
  (assoc (cf/campaign {:id id :creator (str "creator." id) :title (str id)
                       :currency "JPY" :goal-minor 40000 :category :technology
                       :duration-days 45 :launched-at "2026-08-01T00:00:00Z"
                       :deadline "2026-09-15T00:00:00Z"
                       :story "A split keyboard." :risks risks :ships-to #{:jp}})
         :campaign/state state))

(defn- collected [id campaign backer]
  (assoc (pledge/pledge {:id id :campaign campaign :backer backer :amount-minor 24800
                         :reward "standard" :ship-to :jp
                         :placed-at "2026-08-10T00:00:00Z"})
         :pledge/state :collected))

(defn demo-data
  "Fixtures covering the happy path and each hard check.

    cf-shipping  :fulfilling, two collected pledges, one shipped line, a
                 VERIFIED destination -> the first tranche is releasable
    cf-unpaid    :fulfilling, but its creator's destination is present and
                 NOT verified -> refused before approval, not discovered at
                 execution
    cf-early     :collecting -> not yet payable at all"
  []
  {:campaigns {"cf-shipping" (a-campaign "cf-shipping" :fulfilling)
               "cf-unpaid"   (a-campaign "cf-unpaid" :fulfilling)
               "cf-early"    (a-campaign "cf-early" :collecting)}
   :pledges {"cf-shipping" [(collected "s1" "cf-shipping" "backer.one")
                            (collected "s2" "cf-shipping" "backer.two")]
             "cf-unpaid"   [(collected "u1" "cf-unpaid" "backer.three")]
             "cf-early"    [(collected "e1" "cf-early" "backer.four")]}
   :rewards
   (let [tiers [(reward/reward {:id "standard" :title "Standard" :minimum-minor 24800
                                :estimated-delivery "2027-04-01"
                                :shipping {:jp 800} :ships-to #{:jp}})]]
     {"cf-shipping" tiers "cf-unpaid" tiers "cf-early" tiers})
   :destinations
   {"creator.cf-shipping" (payout/payout-destination
                           {:creator "creator.cf-shipping" :rail :bank-transfer
                            :address "JP-BANK-0001" :verified? true})
    "creator.cf-unpaid"   (payout/payout-destination
                           {:creator "creator.cf-unpaid" :rail :bank-transfer
                            :address "JP-BANK-0002" :verified? false})
    "creator.cf-early"    (payout/payout-destination
                           {:creator "creator.cf-early" :rail :bank-transfer
                            :address "JP-BANK-0003" :verified? true})}
   :schedules {"cf-shipping" payout/staged
               "cf-unpaid"   payout/staged
               "cf-early"    payout/staged}
   :tranches {}
   :fulfillment
   {"cf-shipping" [(assoc (ff/line {:pledge "s1" :backer "backer.one"
                                    :campaign "cf-shipping" :reward "standard"
                                    :estimated-delivery "2027-04-01"})
                          :fulfillment/state :shipped)
                   (ff/line {:pledge "s2" :backer "backer.two"
                             :campaign "cf-shipping" :reward "standard"
                             :estimated-delivery "2027-04-01"})]
    "cf-unpaid"   [(ff/line {:pledge "u1" :backer "backer.three"
                             :campaign "cf-unpaid" :reward "standard"
                             :estimated-delivery "2027-04-01"})]
    "cf-early"    []}
   :production {}
   :fee-schedule fee/kickstarter-like
   :platform "platform.itonami"})

;; ----------------------------- MemStore -----------------------------

(defrecord MemStore [a]
  Store
  (campaign-of [_ id] (get-in @a [:campaigns id]))
  (pledges-for [_ id] (get-in @a [:pledges id] []))
  (rewards [_ id] (get-in @a [:rewards id] []))
  (destination [_ creator] (get-in @a [:destinations creator]))
  (all-destinations [_] (sort-by :payout/seller (vals (:destinations @a))))
  (schedule [_ id] (get-in @a [:schedules id] payout/immediate))
  (tranches [_ id] (get-in @a [:tranches id]))
  (fulfillment-lines [_ id] (get-in @a [:fulfillment id] []))
  (production-evidence [_ id] (get-in @a [:production id]))
  (fee-schedule [_] (:fee-schedule @a))
  (platform [_] (:platform @a))
  (ledger [_] (:ledger @a))
  (payout-log [_] (:payout-log @a))
  (commit-record! [_ record]
    (swap! a update :payout-log conj record)
    (let [{:keys [op value payload]} record
          cid (:campaign-id value)]
      (case op
        :bind-payout-destination
        (when-let [d (:destination value)]
          (swap! a assoc-in [:destinations (:payout/seller d)] d))

        :plan-payout
        (when-let [p (:plan value)]
          (swap! a (fn [m] (-> m
                               (assoc-in [:plans cid] p)
                               (assoc-in [:tranches cid] (:payout/tranches p))))))

        ;; A release NEVER moves money here. It records that a human
        ;; authorised one; a rail adapter reads the released tranche and
        ;; performs the transfer outside this actor.
        :release-tranche
        (swap! a update-in [:tranches cid]
               (fn [ts] (mapv (fn [t] (cond-> t
                                        (= (:tranche/id t) (:tranche-id value))
                                        (assoc :tranche/state :released
                                               :tranche/released-by (:approved-by payload))))
                              ts)))

        :hold-tranche
        (swap! a update-in [:tranches cid]
               (fn [ts] (mapv (fn [t] (if (= (:tranche/id t) (:tranche-id value))
                                        (or (payout/hold t {:by (:approved-by payload)
                                                            :at (:at value)
                                                            :reason (:reason value)})
                                            t)
                                        t))
                              ts)))

        :plan-refund
        (swap! a assoc-in [:refunds cid]
               (assoc (:refund value) :refund/authorised-by (:approved-by payload)))
        nil))
    record)
  (append-ledger! [_ fact] (swap! a update :ledger conj fact) fact))

(defn seed-db []
  (->MemStore (atom (assoc (demo-data) :ledger [] :payout-log []
                           :plans {} :refunds {}))))

(defn mem-store [m]
  (->MemStore (atom (merge {:campaigns {} :pledges {} :rewards {} :destinations {}
                            :schedules {} :tranches {} :fulfillment {} :production {}
                            :plans {} :refunds {} :ledger [] :payout-log []
                            :fee-schedule fee/kickstarter-like
                            :platform "platform.itonami"}
                           m))))

;; ----------------------------- derived views -----------------------------

(defn collection-for
  "Recompute the collection result for a campaign from its pledges."
  [s cid]
  (collection/finalize (campaign-of s cid) (pledges-for s cid) (rewards s cid)))

(defn plan-for
  "Compute (not commit) the payout plan, using THIS store's published fee
  schedule, platform identity and the schedule backers were shown."
  [s cid]
  (when-let [c (campaign-of s cid)]
    (payout/payout-plan {:collection   (collection-for s cid)
                         :pledges      (pledges-for s cid)
                         :rewards      (rewards s cid)
                         :fee-schedule (fee-schedule s)
                         :creator      (:campaign/creator c)
                         :platform     (platform s)
                         :tranches     (schedule s cid)})))

(defn gate-facts
  "Which release gates this campaign ACTUALLY satisfies, derived from the
  fulfilment lines and the collection result.

  Derived here rather than accepted from a proposal, so a tranche can
  never be released against a gate nobody checked."
  [s cid now]
  (let [prog (ff/progress (fulfillment-lines s cid) now)
        coll (collection-for s cid)]
    (ff/gate-facts prog {:collection-closed?  (:collection/complete? coll)
                         :production-evidence (production-evidence s cid)})))

(defn plan [s cid] (get-in @(:a s) [:plans cid]))
(defn refund [s cid] (get-in @(:a s) [:refunds cid]))
