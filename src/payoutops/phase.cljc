(ns payoutops.phase
  "Phase 0->3 staged rollout for the payout actor.

    Phase 0  read-only         -- no writes, still governor-gated.
    Phase 1  assisted-planning -- payout plans may be computed and
                                  recorded, every write needs human
                                  approval.
    Phase 2  assisted-holds    -- adds holding a tranche, still
                                  approval-gated.
    Phase 3  supervised auto   -- governor-clean, high-confidence
                                  `:plan-payout` may auto-commit.

  `:bind-payout-destination`, `:release-tranche`, `:hold-tranche`,
  `:plan-refund` and `:flag-payout-concern` are deliberately ABSENT from
  every phase's `:auto` set, INCLUDING phase 3 — a permanent structural
  fact, not a rollout milestone still to come.

  The ones that matter are the ones that decide about money:

    - `:bind-payout-destination` decides WHERE a creator's money goes.
      Getting it wrong sends funds to an account nobody controls, or to an
      attacker who social-engineered the change. It is the money-equivalent
      of issuing an identity, and is human-gated for the same reason.
    - `:release-tranche` decides WHEN money leaves. Computing a plan is
      reversible (recompute it); releasing is not.
    - `:plan-refund` decides who eats the processing fees a processor will
      not return. That number is the difference between a creator who
      refunds knowingly and one who ends up personally short.

  The only auto-committable op is a COMPUTATION. Nothing
  auto-committable moves value. `payoutops.governor`'s own
  `always-escalate-ops` enforces the same invariant independently — two
  layers, not one, agree on this."
  (:require [payoutops.governor :as governor]))

(def read-ops #{})
(def write-ops governor/allowed-ops)

;; NOTE the invariant: the five money-deciding ops are members of
;; `write-ops` (governor-gated like any write) but are NEVER members of
;; any phase's `:auto` set below. Do not add them there.
(def phases
  {0 {:label "read-only"         :writes #{}              :auto #{}}
   1 {:label "assisted-planning" :writes #{:plan-payout}  :auto #{}}
   2 {:label "assisted-holds"    :writes #{:plan-payout :hold-tranche} :auto #{}}
   3 {:label "supervised-auto"   :writes write-ops        :auto #{:plan-payout}}})

(def default-phase 3)

(defn gate
  "Adjust a governor disposition for the rollout phase. Returns
  {:disposition kw :reason kw|nil}."
  [phase {:keys [op]} governor-disposition]
  (let [{:keys [writes auto]} (get phases phase (get phases default-phase))]
    (cond
      (= :hold governor-disposition)       {:disposition :hold :reason nil}
      (contains? read-ops op)              {:disposition governor-disposition :reason nil}
      (not (contains? writes op))          {:disposition :hold :reason :phase-disabled}
      (and (= :commit governor-disposition)
           (not (contains? auto op)))      {:disposition :escalate :reason :phase-approval}
      :else                                {:disposition governor-disposition :reason nil})))

(defn verdict->disposition
  "Map a PayoutGovernor verdict to a base disposition before the phase
  gate."
  [verdict]
  (cond (:hard? verdict) :hold
        (:escalate? verdict) :escalate
        :else :commit))
