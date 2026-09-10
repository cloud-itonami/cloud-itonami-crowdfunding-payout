(ns payoutops.operation-test
  (:require [clojure.test :refer [deftest is testing]]
            [langgraph.graph :as g]
            [payoutops.operation :as operation]
            [payoutops.store :as store]))

(def ctx {:actor-id "payout-test" :phase 3 :now "2027-05-01T00:00:00Z"})

(defn- run-req!
  ([actor tid request] (run-req! actor tid request ctx))
  ([actor tid request c] (g/run* actor {:request request :context c} {:thread-id tid})))

(defn- tranche [s cid id]
  (first (filter #(= id (:tranche/id %)) (store/tranches s cid))))

(deftest a-payout-plan-auto-commits-and-moves-nothing
  (let [s (store/seed-db)
        a (operation/build s)
        r (run-req! a "t1" {:op :plan-payout :campaign-id "cf-shipping"})
        p (store/plan s "cf-shipping")]
    (is (= :commit (:disposition (:state r))))
    (is (= 51200 (:payout/charged-minor p)) "2 x (24800 + 800)")
    (is (= 47064 (:payout/creator-net-minor p)))
    (is (= 4136 (:payout/platform-minor p)))
    (is (= [32946 9412 4706] (mapv :tranche/amount-minor (:payout/tranches p)))
        "dust reaches the creator in the EARLIEST tranche, not in a holdback")
    (is (every? #(= :pending (:tranche/state %)) (store/tranches s "cf-shipping"))
        "a committed plan is a computation; nothing is released by computing it")))

(deftest a-release-interrupts-and-only-a-named-human-authorises-it
  (let [s (store/seed-db)
        a (operation/build s)]
    (run-req! a "t2a" {:op :plan-payout :campaign-id "cf-shipping"})
    (let [held (run-req! a "t2b" {:op :release-tranche :campaign-id "cf-shipping"
                                  :patch {:tranche-id "build"}})]
      (is (= :pending (:tranche/state (tranche s "cf-shipping" "build")))
          "nothing released before approval")
      (is (some #(= :approval-requested (:t %)) (:audit (:state held))))
      (let [ok (g/run* a {:approval {:status :approved :by "treasury-01"}}
                       {:thread-id "t2b" :resume? true})
            t  (tranche s "cf-shipping" "build")]
        (is (= :commit (:disposition (:state ok))))
        (is (= :released (:tranche/state t)))
        (is (= "treasury-01" (:tranche/released-by t))
            "reading the approver from :value would record every release as unattributed")))))

(deftest an-unverified-destination-never-reaches-the-interrupt
  (let [s (store/seed-db)
        a (operation/build s)
        r (run-req! a "t3" {:op :plan-payout :campaign-id "cf-unpaid"})]
    (is (= :hold (:disposition (:state r))))
    (is (not-any? #(= :approval-requested (:t %)) (:audit (:state r))))
    (is (nil? (store/plan s "cf-unpaid")))
    (is (contains? (set (:basis (last (store/ledger s)))) :payout-destination-unverified))))

(deftest a-hold-interrupts-and-then-blocks-the-tranches-behind-it
  (let [s (store/seed-db)
        a (operation/build s)]
    (run-req! a "t4a" {:op :plan-payout :campaign-id "cf-shipping"})
    (run-req! a "t4b" {:op :hold-tranche :campaign-id "cf-shipping"
                       :patch {:tranche-id "build" :reason :backer-complaints
                               :at "2027-05-02T00:00:00Z"}})
    (let [ok (g/run* a {:approval {:status :approved :by "risk-01"}}
                     {:thread-id "t4b" :resume? true})]
      (is (= :commit (:disposition (:state ok))))
      (is (= :held (:tranche/state (tranche s "cf-shipping" "build"))))
      (is (= "risk-01" (get-in (tranche s "cf-shipping" "build") [:tranche/hold :hold/by]))))
    (testing "and a release of the held tranche is now a hard block, not a queue"
      (let [r (run-req! a "t4c" {:op :release-tranche :campaign-id "cf-shipping"
                                 :patch {:tranche-id "build"}})]
        (is (= :hold (:disposition (:state r))))
        (is (contains? (set (:basis (last (store/ledger s)))) :tranche-held))))))

(deftest a-rejected-approval-releases-nothing
  (let [s (store/seed-db)
        a (operation/build s)]
    (run-req! a "t5a" {:op :plan-payout :campaign-id "cf-shipping"})
    (run-req! a "t5b" {:op :release-tranche :campaign-id "cf-shipping"
                       :patch {:tranche-id "build"}})
    (let [no (g/run* a {:approval {:status :rejected :by "treasury-01"}}
                     {:thread-id "t5b" :resume? true})]
      (is (= :hold (:disposition (:state no))))
      (is (= :pending (:tranche/state (tranche s "cf-shipping" "build"))))
      (is (some #(= :approval-rejected (:t %)) (store/ledger s))))))

(deftest a-refund-plan-states-what-the-creator-must-find-beyond-what-they-hold
  (let [s (store/seed-db)
        a (operation/build s)]
    (run-req! a "t6" {:op :plan-refund :campaign-id "cf-shipping"
                      :patch {:reason :unable-to-manufacture
                              :requested-by "creator.cf-shipping"
                              :at "2027-06-01T00:00:00Z"}})
    (is (nil? (store/refund s "cf-shipping")) "nothing recorded before approval")
    (let [ok (g/run* a {:approval {:status :approved :by "treasury-01"}}
                     {:thread-id "t6" :resume? true})
          r  (store/refund s "cf-shipping")]
      (is (= :commit (:disposition (:state ok))))
      (is (= 51200 (:refund/gross-minor r)))
      (is (= 1576 (:refund/unrecoverable-fees-minor r))
          "processing fees generally do not come back from the processor")
      (is (= 4136 (:refund/creator-owes-minor r)))
      (is (= "treasury-01" (:refund/authorised-by r))))))

(deftest phase-zero-writes-nothing
  (let [s (store/seed-db)
        a (operation/build s)
        r (run-req! a "t7" {:op :plan-payout :campaign-id "cf-shipping"}
                    (assoc ctx :phase 0))]
    (is (= :hold (:disposition (:state r))))
    (is (nil? (store/plan s "cf-shipping")))))
