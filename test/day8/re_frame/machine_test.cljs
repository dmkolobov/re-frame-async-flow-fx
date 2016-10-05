(ns day8.re-frame.async-flow-fx.machine-test
	(:require [cljs.test :refer-macros [is deftest]]
						[day8.re-frame.machine :as m]
						[cljs.pprint :refer [pprint]]
						[clojure.data :refer [diff]]
						[day8.re-frame.event-cache :as cache]
						[day8.re-frame.rule :as rule]))

(def rule-1
	(rule/map->Rule
		{:id         :flow-1/rule-1
		 :when       cache/seen-all-of?
		 :events     #{[:success [:foo]]}
		 :dispatch-n [[:bar]]
		 :halt?      false}))

(def rule-2
	(rule/map->Rule
		{:id         :flow-1/rule-2
		 :when       cache/seen-all-of?
		 :events     #{[:success [:bar]]}
		 :dispatch-n [[:success [:foobar]]]
		 :halt?      true}))

(def rule-3
	(rule/map->Rule
		{:id         :flow-1/rule-3
		 :when       cache/seen-some-of?
		 :events     #{[:error [:foo]] [:error [:bar]]}
		 :dispatch-n [[:error [:foobar]]]
		 :halt?      true}))

(def test-rules [rule-1 rule-2 rule-3])

(deftest test-compile
	(is (= (m/compile {:id :flow-1
								 	   :rules [{:id       :rule-1
															:when     :seen?
															:event    [:success [:foo]]
															:dispatch [:bar]}

											 	  	 {:id       :rule-2
															:when     :seen?
															:event    [:success [:bar]]
															:dispatch [:success [:foobar]]
															:halt?    true}

													   {:id       :rule-3
															:when     :seen-any-of?
															:events   [[:error [:foo]]
																			   [:error [:bar]]]
															:dispatch [:error [:foobar]]
															:halt?    true}]})
				 test-rules))

	(is (= (m/compile {:id :flow-1
										 :rules [[[[:success :foo] [:bar]]
															[[:success :bar] [:car]]
															[[:success :car] [:success :foo :bar :car]]]
														 {:when     :seen-any-of?
															:events   [[:error :foo]
																				 [:error :bar]
																				 [:error :car]]
															:dispatch [:error :foo :bar :car]
															:halt?    true}]})
				 [(rule/map->Rule
						{:id :flow-1/rule-1 :when cache/seen-all-of? :events #{[:success :foo]} :dispatch-n [[:bar]] :halt? false})
					(rule/map->Rule
						{:id :flow-1/rule-2 :when cache/seen-all-of? :events #{[:success :bar]} :dispatch-n [[:car]] :halt? false})
					(rule/map->Rule
						{:id :flow-1/rule-3 :when cache/seen-all-of? :events #{[:success :car]} :dispatch-n [[:success :foo :bar :car]] :halt? false})
					(rule/map->Rule
						{:id         :flow-1/rule-4
						 :when       cache/seen-some-of?
						 :events     #{[:error :foo]
													 [:error :bar]
													 [:error :car]}
						 :dispatch-n [[:error :foo :bar :car]]
						 :halt?      true})])))

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
	(is (= (m/add-rules m/fresh-state :flow-1 test-rules) m-state)))

(deftest test-remove-rules
	(is (= (m/remove-rules (m/add-rules m/fresh-state :flow-1 test-rules)
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

	;(deftest test-rule-actions
	;(is (= (m/rule-actions rule-1) (:dispatch-n rule-1)))
	;(is (= (m/rule-actions rule-2) (conj (:dispatch-n rule-2) [:async-flow/halt :flow-1])))
	;(is (= (m/rule-actions rule-3) (conj (:dispatch-n rule-3) [:async-flow/halt :flow-1]))))
