(ns day8.re-frame.async-flow-fx.flow-test
	(:require [cljs.test :refer-macros [is deftest]]
						[day8.re-frame.flow :as flow]
						[day8.re-frame.event-cache :as cache]))

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

(deftest test-rule-effects
	(is (= (flow/fire-rule rule-1) (:dispatch-n rule-1)))
	(is (= (flow/fire-rule rule-2) (conj (:dispatch-n rule-2) [:async-flow/halt :flow-1])))
	(is (= (flow/fire-rule rule-3) (conj (:dispatch-n rule-3) [:async-flow/halt :flow-1]))))

(deftest test-compile
				 (is (= (flow/compile {:id :flow-1
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

				 (is (= (flow/compile {:id :flow-1
															 :rules [(flow/chain
																				 [:success :foo] [:bar]
																				 [:success :bar] [:car]
																				 [:success :car] [:success :foo :bar :car])
																			 {:when     :seen-any-of?
																				:events   [[:error :foo]
																									 [:error :bar]
																									 [:error :car]]
																				:dispatch [:error :foo :bar :car]
																				:halt?    true}]})
								[(flow/map->Rule
									 {:id :flow-1/rule-1 :when cache/seen-all-of? :events #{[:success :foo]} :dispatch-n [[:bar]] :halt? false})
								 (flow/map->Rule
									 {:id :flow-1/rule-2 :when cache/seen-all-of? :events #{[:success :bar]} :dispatch-n [[:car]] :halt? false})
								 (flow/map->Rule
									 {:id :flow-1/rule-3 :when cache/seen-all-of? :events #{[:success :car]} :dispatch-n [[:success :foo :bar :car]] :halt? false})
								 (flow/map->Rule
									 {:id         :flow-1/rule-4
										:when       cache/seen-some-of?
										:events     #{[:error :foo]
																	[:error :bar]
																	[:error :car]}
										:dispatch-n [[:error :foo :bar :car]]
										:halt?      true})])))
