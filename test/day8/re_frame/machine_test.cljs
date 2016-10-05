(ns day8.re-frame.async-flow-fx.machine-test
	(:require [cljs.test :refer-macros [is deftest]]
						[day8.re-frame.machine :as m]
						[cljs.pprint :refer [pprint]]
						[clojure.data :refer [diff]]
						[day8.re-frame.event-cache :as cache]
						[day8.re-frame.flow :as flow]))

(def rule-1
	(flow/map->Rule
		{:id         :flow-1/rule-1
		 :when       cache/seen-all-of?
		 :events     #{[:success [:foo]]}
		 :dispatch-n [[:bar]]
		 :halt?      false}))

(def rule-2
	(flow/map->Rule
		{:id         :flow-1/rule-2
		 :when       cache/seen-all-of?
		 :events     #{[:success [:bar]]}
		 :dispatch-n [[:success [:foobar]]]
		 :halt?      true}))

(def rule-3
	(flow/map->Rule
		{:id         :flow-1/rule-3
		 :when       cache/seen-some-of?
		 :events     #{[:error [:foo]] [:error [:bar]]}
		 :dispatch-n [[:error [:foobar]]]
		 :halt?      true}))

(def test-rules [rule-1 rule-2 rule-3])

(def m-state
	(m/map->FlowState
		{:rules       {:flow-1/rule-1 rule-1
								   :flow-1/rule-2 rule-2
								   :flow-1/rule-3 rule-3}

		 :flows       {:flow-1 (list :flow-1/rule-1 :flow-1/rule-2 :flow-1/rule-3)}

		 :matcher     {:success {[:foo] #{:flow-1/rule-1}
														 [:bar] #{:flow-1/rule-2}}
									 :error   {[:foo] #{:flow-1/rule-3}
														 [:bar] #{:flow-1/rule-3}}}

		 :seen-events {:flow-1/rule-1 (:events rule-1)
									 :flow-1/rule-2 (:events rule-2)
									 :flow-1/rule-3 (:events rule-3)}

		 :fired-rules #{}}))

(deftest test-add-flow
	(is (= (m/install m/fresh-state {:id :flow-1 :rules test-rules}) m-state)))

(deftest test-remove-rules
	(is (= (m/uninstall (m/install m/fresh-state {:id :flow-1 :rules test-rules})
										  :flow-1)
				 m/fresh-state)))

(defn play
	[machine-state & events]
	(reduce (fn [[state dispatches] event-v]
						(m/transition state event-v))
					[machine-state nil]
					events))

(deftest test-record-events
	(is (= (play m-state [:success [:foo]])
				 [(assoc m-state
						:seen-events  {:flow-1/rule-1 #{}
													 :flow-1/rule-2 (:events rule-2)
													 :flow-1/rule-3 (:events rule-3)}
						:fired-rules #{:flow-1/rule-1})
					(list [:bar])]))

	(is (= (play m-state [:success [:foo]] [:success [:bar]])
				 [(assoc m-state
						:seen-events  {:flow-1/rule-1 #{}
													 :flow-1/rule-2 #{}
													 :flow-1/rule-3 (:events rule-3)}
						:fired-rules #{:flow-1/rule-1 :flow-1/rule-2})
					(list [:success [:foobar]] [:async-flow/halt :flow-1])]))

	(is (= (play m-state [:error [:foo]])
				 [(assoc m-state
						:seen-events  {:flow-1/rule-1 (:events rule-1)
													 :flow-1/rule-2 (:events rule-2)
													 :flow-1/rule-3 (disj (:events rule-3) [:error [:foo]])}
						:fired-rules #{:flow-1/rule-3})
					(list [:error [:foobar]] [:async-flow/halt :flow-1])]))

	(is (= (play m-state [:error [:bar]])
				 [(assoc m-state
						:seen-events  {:flow-1/rule-1 (:events rule-1)
													 :flow-1/rule-2 (:events rule-2)
													 :flow-1/rule-3 (disj (:events rule-3) [:error [:bar]])}
						:fired-rules #{:flow-1/rule-3})
					(list [:error [:foobar]] [:async-flow/halt :flow-1])])))
