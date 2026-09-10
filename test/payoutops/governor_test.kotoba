(ns payoutops.governor-test
  (:require [clojure.test :refer [deftest is testing]]
            [crowdfunding.payout :as payout]
            [payoutops.advisor :as advisor]
            [payoutops.governor :as sut]
            [payoutops.phase :as phase]
            [payoutops.store :as store]))

(def ctx {:actor-id "payout-test" :phase 3 :now "2027-05-01T00:00:00Z"})

(defn- verdict [st request]
  (sut/check request ctx (advisor/infer st request) st))

(defn- rules [v] (set (map :rule (:violations v))))

(defn- planned
  "Seed with the payout plan already committed for cf-shipping, so tranche
  records exist."
  []
  (let [s (store/seed-db)]
    (store/commit-record! s {:op :plan-payout
                             :value {:campaign-id "cf-shipping"
                                     :plan (store/plan-for s "cf-shipping")}})
    s))

;; ───────────────────────── the happy path ─────────────────────────

(deftest a-conserving-plan-with-a-verified-destination-commits
  (let [st (store/seed-db)
        v  (verdict st {:op :plan-payout :campaign-id "cf-shipping"})]
    (is (true? (:ok? v)))
    (is (= [] (:violations v)))
    (is (= :commit (phase/verdict->disposition v)))))

(deftest the-plan-conserves-in-both-directions
  (let [st (store/seed-db)
        p  (store/plan-for st "cf-shipping")]
    (is (true? (:payout/fees-conserved? p)))
    (is (true? (:payout/tranches-conserved? p)))
    (is (= (:payout/charged-minor p)
           (+ (:payout/platform-minor p) (:payout/tithe-minor p)
              (:payout/creator-net-minor p))))
    (is (= (:payout/creator-net-minor p)
           (reduce + 0 (map :tranche/amount-minor (:payout/tranches p)))))
    (is (false? (:payout/custodial? p)))))

(deftest default-mock-advisor-proposals-never-self-trip-scope-exclusion
  (let [st (planned)]
    (doseq [req [{:op :plan-payout :campaign-id "cf-shipping"}
                 {:op :bind-payout-destination :campaign-id "cf-shipping"
                  :patch {:creator "creator.cf-shipping" :rail :bank-transfer
                          :address "JP-BANK-0001"}}
                 {:op :release-tranche :campaign-id "cf-shipping"
                  :patch {:tranche-id "build"}}
                 {:op :hold-tranche :campaign-id "cf-shipping"
                  :patch {:tranche-id "build" :reason :complaints}}
                 {:op :plan-refund :campaign-id "cf-shipping" :patch {:reason :x}}
                 {:op :flag-payout-concern :campaign-id "cf-shipping"}]]
      (is (not (contains? (rules (verdict st req)) :scope-excluded))
          (str (:op req) " must not block itself")))))

;; ───────────────────────── hard checks ─────────────────────────

(deftest an-unverified-destination-is-caught-before-approval
  (testing "an unverified destination found at execution is a payment already in flight"
    (let [st (store/seed-db)
          v  (verdict st {:op :plan-payout :campaign-id "cf-unpaid"})]
      (is (true? (:hard? v)))
      (is (contains? (rules v) :payout-destination-unverified)))))

(deftest verification-is-read-from-the-store-not-from-the-proposal
  (let [st (store/seed-db)
        lying (assoc-in (advisor/infer st {:op :plan-payout :campaign-id "cf-unpaid"})
                        [:value :destination]
                        (payout/payout-destination {:creator "creator.cf-unpaid"
                                                    :rail :bank-transfer
                                                    :address "JP-BANK-0002"
                                                    :verified? true}))]
    (is (contains? (rules (sut/check {} ctx lying st)) :payout-destination-unverified)
        "the governor re-derives verification from the creator's own record")))

(deftest money-still-arriving-is-money-that-can-still-fail-to-arrive
  (let [st (store/seed-db)
        v  (verdict st {:op :plan-payout :campaign-id "cf-early"})]
    (is (contains? (rules v) :not-payable))))

(deftest releasing-before-the-plan-is-committed-is-refused
  (let [st (store/seed-db)
        v  (verdict st {:op :release-tranche :campaign-id "cf-shipping"
                        :patch {:tranche-id "build"}})]
    (is (contains? (rules v) :payout-plan-not-committed))))

(deftest an-unmet-gate-blocks-a-release
  (let [st (planned)]
    (testing "the shipping campaign has a shipped line, so :on-first-shipment is met"
      (is (contains? (store/gate-facts st "cf-shipping" (:now ctx)) :on-first-shipment)))
    (testing "but fulfilment is not complete, so the last tranche's gate is not"
      (is (not (contains? (store/gate-facts st "cf-shipping" (:now ctx))
                          :on-fulfillment-complete)))
      (is (contains? (rules (verdict st {:op :release-tranche :campaign-id "cf-shipping"
                                         :patch {:tranche-id "complete"}}))
                     :gate-not-met)))))

(deftest production-having-begun-is-never-inferred
  (testing "inferring it would invent the very fact the holdback exists to require"
    (let [st (planned)]
      (is (not (contains? (store/gate-facts st "cf-shipping" (:now ctx))
                          :on-production-start)))
      (let [st2 (store/mem-store (assoc (store/demo-data)
                                        :production {"cf-shipping" {:photos 3}}))]
        (is (contains? (store/gate-facts st2 "cf-shipping" (:now ctx))
                       :on-production-start))))))

(deftest a-later-tranche-cannot-jump-an-earlier-one
  (let [st (planned)
        ;; Force :on-fulfillment-complete so ONLY the ordering rule can
        ;; refuse: every line delivered.
        st2 (store/mem-store
             (assoc (store/demo-data)
                    :fulfillment {"cf-shipping"
                                  [{:fulfillment/pledge "s1" :fulfillment/state :delivered}
                                   {:fulfillment/pledge "s2" :fulfillment/state :delivered}]}))]
    (store/commit-record! st2 {:op :plan-payout
                               :value {:campaign-id "cf-shipping"
                                       :plan (store/plan-for st2 "cf-shipping")}})
    (is (contains? (store/gate-facts st2 "cf-shipping" (:now ctx)) :on-fulfillment-complete))
    (is (contains? (rules (verdict st2 {:op :release-tranche :campaign-id "cf-shipping"
                                        :patch {:tranche-id "complete"}}))
                   :out-of-order)
        "otherwise the holdback ordering backers read on the page was decorative")
    (is (= #{} (rules (verdict st {:op :release-tranche :campaign-id "cf-shipping"
                                   :patch {:tranche-id "build"}})))
        "and the first one, whose gate is met, is fine")))

(deftest a-held-tranche-is-a-permanent-block-for-this-actor
  (let [st (planned)]
    (store/commit-record! st {:op :hold-tranche
                              :value {:campaign-id "cf-shipping" :tranche-id "build"
                                      :reason :backer-complaints}
                              :payload {:approved-by "risk-01"}})
    (is (contains? (rules (verdict st {:op :release-tranche :campaign-id "cf-shipping"
                                       :patch {:tranche-id "build"}}))
                   :tranche-held)
        "only a named human's resolve-hold crosses that line")))

(deftest an-unknown-tranche-is-refused
  (let [st (planned)]
    (is (contains? (rules (verdict st {:op :release-tranche :campaign-id "cf-shipping"
                                       :patch {:tranche-id "bonus"}}))
                   :tranche-unknown))))

(deftest a-destination-draft-must-be-structurally-sound
  (let [st (store/seed-db)]
    (is (contains? (rules (verdict st {:op :bind-payout-destination
                                       :campaign-id "cf-shipping"
                                       :patch {:creator "creator.x" :rail :carrier-pigeon
                                               :address "somewhere"}}))
                   :unknown-rail))
    (is (contains? (rules (verdict st {:op :bind-payout-destination
                                       :campaign-id "cf-shipping"
                                       :patch {:creator "creator.x" :rail :bank-transfer}}))
                   :missing-payout-address))
    (testing "but its own :verified? claim proves nothing and is not checked here"
      (is (not (contains? (rules (verdict st {:op :bind-payout-destination
                                              :campaign-id "cf-shipping"
                                              :patch {:creator "creator.x"
                                                      :rail :bank-transfer
                                                      :address "JP-BANK-9"}}))
                          :payout-destination-unverified))))))

(deftest an-effect-other-than-propose-is-a-claim-to-actuate
  (let [st (store/seed-db)
        p  (assoc (advisor/infer st {:op :plan-payout :campaign-id "cf-shipping"})
                  :effect :execute)]
    (is (contains? (rules (sut/check {} ctx p st)) :effect-not-propose))))

(deftest claiming-to-have-moved-funds-is-permanently-blocked
  (let [st (store/seed-db)
        p  (advisor/infer st {:op :plan-payout :campaign-id "cf-shipping"
                              :out-of-scope? true})]
    (is (contains? (rules (sut/check {} ctx p st)) :scope-excluded))))

(deftest no-op-in-the-allowlist-transfers-funds
  (is (= #{:plan-payout :bind-payout-destination :release-tranche :hold-tranche
           :plan-refund :flag-payout-concern}
         sut/allowed-ops))
  (is (contains? (rules (sut/check {} ctx {:op :send-money :effect :propose}
                                   (store/seed-db)))
                 :op-not-allowed)))

;; ───────────────────────── the two-layer invariant ─────────────────────────

(deftest the-money-deciding-ops-are-out-of-every-phase-auto-set
  (doseq [[p {:keys [auto]}] phase/phases
          op sut/always-escalate-ops]
    (is (not (contains? auto op))
        (str op " must never be auto-committable, including phase " p))))

(deftest even-a-clean-release-escalates
  (let [st (planned)
        v  (verdict st {:op :release-tranche :campaign-id "cf-shipping"
                        :patch {:tranche-id "build"}})]
    (is (false? (:hard? v)))
    (is (true? (:high-stakes? v)))
    (is (= :escalate (phase/verdict->disposition v)))))

(deftest a-governor-hold-survives-every-phase
  (doseq [p (keys phase/phases)]
    (is (= :hold (:disposition (phase/gate p {:op :plan-payout} :hold))))))

(deftest early-phases-disable-writes-rather-than-quietly-allowing-them
  (is (= {:disposition :hold :reason :phase-disabled}
         (phase/gate 0 {:op :plan-payout} :commit)))
  (is (= {:disposition :escalate :reason :phase-approval}
         (phase/gate 1 {:op :plan-payout} :commit)))
  (is (= {:disposition :hold :reason :phase-disabled}
         (phase/gate 2 {:op :release-tranche} :commit)))
  (is (= {:disposition :commit :reason nil}
         (phase/gate 3 {:op :plan-payout} :commit))))

(deftest low-confidence-escalates-without-being-a-violation
  (let [st (store/seed-db)
        p  (assoc (advisor/infer st {:op :plan-payout :campaign-id "cf-shipping"})
                  :confidence 0.2)
        v  (sut/check {} ctx p st)]
    (is (false? (:hard? v)))
    (is (true? (:escalate? v)))))
