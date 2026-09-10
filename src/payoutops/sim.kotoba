(ns payoutops.sim
  "Offline demo: compute a payout, watch an unverified destination and an
  unmet gate block a release, release the first tranche with a human's
  approval, and see what a refund would actually cost.
  `clojure -M:dev:run`."
  (:require [langgraph.graph :as g]
            [payoutops.operation :as operation]
            [payoutops.store :as store]))

(def ^:private ctx {:actor-id "payout-demo" :phase 3 :now "2027-05-01T00:00:00Z"})

(defn- run-req! [actor tid request]
  (g/run* actor {:request request :context ctx} {:thread-id tid}))

(defn -main [& _]
  (let [s (store/seed-db)
        actor (operation/build s)]

    (println "\n=== 1. 支払計画の計算（保存則つき。自動コミット）===")
    (let [r (run-req! actor "sim-1" {:op :plan-payout :campaign-id "cf-shipping"})
          p (store/plan s "cf-shipping")]
      (println "  status     :" (:status r))
      (println "  請求総額    :" (:payout/charged-minor p) (:payout/currency p))
      (println "  手数料      :" (:payout/platform-minor p))
      (println "  創作者受取  :" (:payout/creator-net-minor p))
      (doseq [t (:payout/tranches p)]
        (println "    -" (:tranche/id t) (:tranche/bps t) "bp →"
                 (:tranche/amount-minor t) "gate" (:tranche/gate t)))
      (println "  保存則      :" (:payout/tranches-conserved? p)
               "/" (:payout/fees-conserved? p)
               " 預託しない:" (not (:payout/custodial? p))))

    (println "\n=== 2. 支払先が未検証なら承認にすら回さない ===")
    (let [r (run-req! actor "sim-2" {:op :plan-payout :campaign-id "cf-unpaid"})]
      (println "  status     :" (:status r))
      (println "  violations :" (mapv :rule (:violations (last (store/ledger s))))))

    (println "\n=== 3. 回収中は支払わない（まだ失われうる資金だから）===")
    (let [r (run-req! actor "sim-3" {:op :plan-payout :campaign-id "cf-early"})]
      (println "  status     :" (:status r))
      (println "  violations :" (mapv :rule (:violations (last (store/ledger s))))))

    (println "\n=== 4. 充足済みの gate（記録から再導出）===")
    (println "  充足        :" (store/gate-facts s "cf-shipping" (:now ctx)))

    (println "\n=== 5. 解放は必ず人間の承認を通る ===")
    (let [held (run-req! actor "sim-5" {:op :release-tranche :campaign-id "cf-shipping"
                                        :patch {:tranche-id "build"}})]
      (println "  status     :" (:status held) "（承認前）")
      (let [ok (g/run* actor {:approval {:status :approved :by "treasury-01"}}
                       {:thread-id "sim-5" :resume? true})
            t  (first (filter #(= "build" (:tranche/id %)) (store/tranches s "cf-shipping")))]
        (println "  --- 人間 treasury-01 が承認 ---")
        (println "  status     :" (:status ok))
        (println "  分割 build :" (:tranche/state t) "by" (:tranche/released-by t))))

    (println "\n=== 6. 先行分割が未解放なら後続は解放できない ===")
    (let [r (run-req! actor "sim-6" {:op :release-tranche :campaign-id "cf-shipping"
                                     :patch {:tranche-id "complete"}})]
      (println "  status     :" (:status r))
      (println "  violations :" (mapv :rule (:violations (last (store/ledger s))))))

    (println "\n=== 7. 返金したら創作者はいくら足りなくなるか ===")
    (let [held (run-req! actor "sim-7" {:op :plan-refund :campaign-id "cf-shipping"
                                        :patch {:reason :unable-to-manufacture
                                                :requested-by "creator.cf-shipping"
                                                :at "2027-06-01T00:00:00Z"}})]
      (println "  status     :" (:status held) "（承認前）")
      (let [ok (g/run* actor {:approval {:status :approved :by "treasury-01"}}
                       {:thread-id "sim-7" :resume? true})
            r  (store/refund s "cf-shipping")]
        (println "  status     :" (:status ok))
        (println "  返金総額    :" (:refund/gross-minor r))
        (println "  戻らない手数料:" (:refund/unrecoverable-fees-minor r))
        (println "  創作者の追加負担:" (:refund/creator-owes-minor r))))

    (println "\n=== 監査台帳 ===")
    (doseq [f (store/ledger s)]
      (println " " (:t f) (:op f) (or (:basis f) "")))))
