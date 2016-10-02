(ns day8.re-frame.async-flow-fx.async-flow-fx-test
  (:require [cljs.test :refer-macros [is deftest]]
            [day8.re-frame.async-flow-fx :as core]))

(deftest test-all-events-seen?
  (is (= (core/seen-all-of? [[:a]] {:a true}) true))
  (is (= (core/seen-all-of? [[:a]] {:a true :b true}) true))
  (is (= (core/seen-all-of? [[:a] [:b]] {:a true :b true}) true))
  (is (= (core/seen-all-of? [[:a]] {:b true}) false))
  (is (= (core/seen-all-of? [[:a] [:b]] {:a true :c true}) false))
  (is (= (core/seen-all-of? [[:a]] {:b true :c true}) false))
  (is (= (core/seen-all-of? [[:a]] {}) false)))


(deftest test-any-events-seen?
  (is (= (core/seen-any-of? [[:a]] {:a true}) true))
  (is (= (core/seen-any-of? [[:a] [:b]] {:a true :b true}) true))
  (is (= (core/seen-any-of? [[:a] [:b]] {:a true :c true}) true))
  (is (= (core/seen-any-of? [[:a]] {:b true}) false))
  (is (= (core/seen-any-of? [[:a]] {}) false)))


(deftest test-newly-startable-tasks
  (let [rules [{:id 1 :when core/seen-all-of?  :events [[:a] [:b]]}
               {:id 2 :when core/seen-all-of?  :events [[:a]]}]]
  (is (= (core/startable-rules rules {:c true} #{})
         []))
  (is (= (core/startable-rules rules {:a true} #{2})
         []))
  (is (= (core/startable-rules rules {:a true} #{1})
         [(nth rules 1)]))
  (is (= (core/startable-rules rules {:a true :b true} #{2})
         [(nth rules 0)]))
  (is (= (core/startable-rules rules {:a true} #{})
         [(nth rules 1)]))))

(def test-sequence-rule
	{:when         :seen-sequence?
	 :events       [[:a] [:b] [:c]]
	 :step-success [:async/success]
	 :dispatch-n   [[:foo] [:bar]]
	 :halt?        true})

(deftest test-sequence
	(is (= (core/expand-sequence-rule test-sequence-rule)

				 [{:when     :seen?
					 :event    [:async/success [:a]]
					 :dispatch [:b]}

					{:when     :seen?
					 :event    [:async/success [:b]]
					 :dispatch [:c]}

					{:when       :seen?
					 :event      [:async/success [:c]]
					 :dispatch-n [[:foo] [:bar]]
					 :halt?      true}])))

(deftest test-massage-rules
  (is (= (core/massage-rules [{:when :seen? :event :1 :dispatch [:2]}])
         (list {:id 0 :when core/seen-all-of? :events [[:1]] :halt? false :dispatch-n (list [:2])})))

  (is (= (core/massage-rules [{:when :seen-both? :events [:1 :2] :halt? true}])
         (list {:id 0 :when core/seen-all-of? :events [[:1] [:2]] :halt? true :dispatch-n '()})))

  (is (= (core/massage-rules [{:when :seen-any-of? :events [:1 :2] :dispatch [:2] :halt? true}])
         (list {:id 0 :when core/seen-any-of? :events [[:1] [:2]] :halt? true :dispatch-n (list [:2])})))

	(is (= (core/massage-rules [{:when :seen? :event :foo :dispatch [:bar]}
															test-sequence-rule
															{:when :seen? :event :hello :dispatch [:world]}]))
			(list {:id 0
						 :when       core/seen-any-of?
						 :events     [[:foo]]
						 :dispatch-n [[:bar]]
						 :halt?      false}
						{:id 1
						 :when       core/seen-any-of?
						 :events     [[:async/success :a]]
						 :dispatch-n [[:b]]
						 :halt?      false}
						{:id 2
						 :when       core/seen-any-of?
						 :events     [[:async/success :b]]
						 :dispatch-n [[:c]]
						 :halt?      false}
						{:id 3
						 :when       core/seen-any-of?
						 :events     [[:async/success :c]]
						 :dispatch-n [[:foo] [:bar]]
						 :halt?      true}
						{:id 4
						 :when       core/seen-any-of?
						 :events     [[:hello]]
						 :dispatch-n [[:world]]
						 :halt?      false})))


(deftest test-setup
  (let [flow       {:id             :some-id
										:first-dispatch [:1]
                    :rules          [
                                     {:when :seen? :event :1 :dispatch [:2]}
                                     {:when :seen? :event [:foo :bar] :halt? true}]}
        handler-fn (core/make-flow-event-handler flow)]
    (is (= (handler-fn {:db {}} [:dummy-id :setup])
           {:db             {}
            :dispatch       [:1]
            :forward-events {:register     :some-id
														 :events      #{:1 :foo}
														 :dispatch-to [:some-id]}}))))

(deftest test-forwarding
  (let [flow {:first-dispatch [:start]
              :id             :test-id
              :db-path        [:p]
              :rules [{:id 0 :when :seen? :event :1 :dispatch [:2]}
                      {:id 1 :when :seen? :event [:foo :bar] :halt? true}
											{:id :hello :when :seen? :events [[:1] [:foo :bar]] :dispatch [:world]}
                      {:id 2 :when :seen-any-of? :events [:4 :5] :dispatch [:6]}]}
        handler-fn  (core/make-flow-event-handler flow)]

    ;; event :no should cause nothing to happen, but should be remembered.
    (is (= (handler-fn
             {:db {:p {:seen-events {:33 true}
                       :rules-fired #{}}}}
             [:test-id [:no]])
           {:db {:p {:seen-events {:33 true :no true}
                     :rules-fired #{}}}}))

    ;; new event should not cause a new dispatch because task is already started  (:id 0 is in :rules-fired)
    (is (= (handler-fn
             {:db {:p {:seen-events {:1 true}
                       :rules-fired #{0}}}}
             [:test-id [:1]])
           {:db {:p {:seen-events {:1 true} :rules-fired #{0}}}}))

    ;; new event should cause a dispatch
    (is (= (handler-fn
             {:db {:p {:seen-events {}
                       :rules-fired #{}}}}
             [:test-id [:1]])
           {:db {:p {:seen-events {:1 true} :rules-fired #{0}}}
            :dispatch-n (list [:2])}))

    ;; new event should cause a dispatch
    (is (= (handler-fn
             {:db {:p {:seen-events {:1 true}
                       :rules-fired #{0}}}}
             [:test-id [:foo :bar]])
           {:db {:p {:seen-events {:1 true :foo {:bar true}} :rules-fired #{0 1 :hello}}}
            :dispatch-n (list [:world] [:test-id :halt-flow])}))

    ;; make sure :seen-any-of? works
    (is (= (handler-fn
             {:db {:p {:seen-events {}
                       :rules-fired #{}}}}
             [:test-id [:4]])
           {:db {:p {:seen-events {:4 true} :rules-fired #{2}}}
            :dispatch-n (list [:6])}))))

(deftest test-event-filtering
	(let [test-events [[:a 1] [:a 2] [:a 3]]
				flow {:first-dispatch [:start]
							:id             :test-id
							:db-path        [:p]
							:rules [{:id         :map
											 :when       :seen?
											 :event      [:start]
											 :dispatch-n test-events}
											{:id       :collect
											 :when     :seen?
											 :events   test-events
											 :dispatch [:foobar]}]}
				handler-fn  (core/make-flow-event-handler flow)]

		;; test that all test events are fired when start happens
		(is (= (handler-fn
						 {:db {:p {:seen-events {}
											 :rules-fired #{}}}}
						 [:test-id [:start]])
					 {:db {:p {:seen-events {:start true}
										 :rules-fired #{:map}}}
						:dispatch-n test-events}))

		;; test the first test event is added to the seen events
		(is (= (handler-fn
						 {:db {:p {:seen-events {:start true}
											 :rules-fired #{:map}}}}
						 [:test-id [:a 1]])
					 {:db {:p {:seen-events {:start true :a {1 true}}
										 :rules-fired #{:map}}}}))))


(deftest test-halt1
  (let [flow {:id :some-id
							:first-dispatch [:1]
              :rules []}
        handler-fn   (core/make-flow-event-handler flow)]
    (is (= (handler-fn {:db {}} [:dummy-id :halt-flow])
           { ;; :db {}
            :deregister-event-handler :some-id
            :forward-events           {:unregister :some-id}}))))

;; Aggh. I don't have dissoc-in available to make this work.
#_(deftest test-halt2
    (let [flow {:id  :blah
                :db-path [:p]
                :first-dispatch [:1]
                :rules []}
          handler-fn   (core/make-flow-event-handler flow)]
      (is (= (handler-fn {:db {:p {:seen-events #{:33} :rules-fired #{}}}} :halt-flow)
             {:db                       {}
              :deregister-event-handler :blah
              :forward-events           {:unregister :blah}}))))
