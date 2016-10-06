(ns day8.re-frame.async-flow-fx.flow-test
	(:require [cljs.test :refer-macros [is deftest]]
						[day8.re-frame.rule :as rule]
						[day8.re-frame.flow :as flow]))


(def rule-1
		{:id          :rule-1
		 :when        :seen?
		 :events      #{[:success [:foo]]}
		 :dispatch-n  [[:bar]]
		 :halt?       false})

(def c-rule-1 (rule/compile :flow-1 0 rule-1))

(def rule-2
		{:id          :rule-2
		 :when        :seen?
		 :events      #{[:success [:bar]]}
		 :dispatch-n  [[:success [:foobar]]]
		 :halt?       true})

(def c-rule-2 (rule/compile :flow-1 1 rule-2))

(def rule-3
		{:id          :rule-3
		 :when        :seen-any-of?
		 :events      #{[:error [:foo]] [:error [:bar]]}
		 :dispatch-n  [[:error [:foobar]]]
		 :halt?       true})

(def c-rule-3 (rule/compile :flow-1 2 rule-3))

(def m-state
	(flow/map->FlowState
		{:rules       {:flow-1/rule-1 c-rule-1
									 :flow-1/rule-2 c-rule-2
									 :flow-1/rule-3 c-rule-3}

		 :flows       {:flow-1 [c-rule-1 c-rule-2 c-rule-3]}

		 :matcher     {:success {[:foo] #{:flow-1/rule-1}
														 [:bar] #{:flow-1/rule-2}}
									 :error   {[:foo] #{:flow-1/rule-3}
														 [:bar] #{:flow-1/rule-3}}}

		 :fired-rules #{}}))

(deftest test-add-flow
	(is (= (flow/install flow/fresh-state {:id :flow-1 :rules [rule-1 rule-2 rule-3]}) m-state)))

(deftest test-remove-rules
	(is (= (flow/uninstall (flow/install flow/fresh-state {:id :flow-1 :rules [rule-1 rule-2 rule-3]})
											   :flow-1)
				 flow/fresh-state)))

(defn play
	[machine-state & events]
	(reduce (fn [[state dispatches] event-v]
						(flow/transition state event-v))
					[machine-state nil]
					events))

(deftest test-record-events
	;; ensure that the first rule is fired when its event is fired.
	(is (= (play m-state [:success [:foo]])
				 [(-> m-state
							(update :rules
											assoc
											:flow-1/rule-1 (update c-rule-1 :seen-events conj [:success [:foo]]))
							(update :fired-rules conj :flow-1/rule-1))
					(list [:bar])]))

	;; ensure that rules are not fired twice.
	(is (= (play (-> m-state
									 (update :rules
													 assoc
													 :flow-1/rule-1 (update c-rule-1 :seen-events conj [:success [:foo]]))
									 (update :fired-rules conj :flow-1/rule-1))
							 [:success [:foo]])
				 [(-> m-state
							(update :rules
											assoc
											:flow-1/rule-1 (update c-rule-1 :seen-events conj [:success [:foo]]))
							(update :fired-rules conj :flow-1/rule-1))
					[]]))

	(is (= (play m-state [:success [:foo]] [:success [:bar]])
				 [(-> m-state
							(update :rules
											assoc
											:flow-1/rule-1 (update c-rule-1 :seen-events conj [:success [:foo]])
											:flow-1/rule-2 (update c-rule-2 :seen-events conj [:success [:bar]]))
							(update :fired-rules conj :flow-1/rule-1 :flow-1/rule-2))
					(list [:success [:foobar]] [:async-flow/halt :flow-1])]))

	(is (= (play m-state [:error [:foo]])
				 [(-> m-state
							(update :rules
											assoc
											:flow-1/rule-3 (update c-rule-3 :seen-events conj [:error [:foo]]))
							(update :fired-rules conj :flow-1/rule-3))
					(list [:error [:foobar]] [:async-flow/halt :flow-1])]))

	(is (= (play m-state [:error [:bar]])
				 [(-> m-state
							(update :rules
											assoc
											:flow-1/rule-3 (update c-rule-3 :seen-events conj [:error [:bar]]))
							(update :fired-rules conj :flow-1/rule-3))
					(list [:error [:foobar]] [:async-flow/halt :flow-1])])))
