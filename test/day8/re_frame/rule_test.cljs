(ns day8.re-frame.async-flow-fx.rule-test
	(:require [cljs.test :refer-macros [is deftest]]
						[day8.re-frame.rule :as rule]))

(def rule-1
	(rule/map->Rule
		{:id          :flow-1/rule-1
		 :when-fn     rule/seen-all?
		 :events      #{[:success [:foo]]}
		 :dispatch-n  [[:bar]]
		 :halt?       false
		 :capture?    false
		 :seen-events []}))

(def rule-2
	(rule/map->Rule
		{:id          :flow-1/rule-2
		 :when-fn     rule/seen-all?
		 :events      #{[:success [:bar]]}
		 :dispatch-n  [[:success [:foobar]]]
		 :halt?       true
		 :capture?    true
		 :seen-events []}))

(def rule-3
	(rule/map->Rule
		{:id         :flow-1/rule-3
		 :when-fn    rule/seen-any?
		 :events     #{[:error [:foo]] [:error [:bar]]}
		 :dispatch-n [[:error [:foobar]]]
		 :halt?      true
		 :capture?   false
		 :seen-events []}));;

(def test-rules [rule-1 rule-2 rule-3])

(deftest test-rule-effects
				 (is (= (rule/fire rule-1) (:dispatch-n rule-1)))
				 (is (= (rule/fire (assoc rule-2 :seen-events [[:success [:bar] :data]]))
								[[:success [:foobar] [:success [:bar] :data]]
								 [:async-flow/halt :flow-1]]))
				 (is (= (rule/fire rule-3) (conj (:dispatch-n rule-3) [:async-flow/halt :flow-1]))))

(deftest test-compile
	(is (= (rule/compile :flow-1 nil {:id       :rule-1
																						:when     :seen?
																						:event    [:success [:foo]]
																						:dispatch [:bar]})
				 rule-1))
	(is (= (rule/compile :flow-1 nil {:id       :rule-2
																						:when     :seen?
																						:event    [:success [:bar]]
																						:dispatch [:success [:foobar]]
																						:halt?    true
																						:capture? true})
				 rule-2))
	(is (= (rule/compile :flow-1 nil {:id       :rule-3
																						:when     :seen-any-of?
																						:events   [[:error [:foo]]
																											 [:error [:bar]]]
																						:dispatch [:error [:foobar]]
																						:halt?    true})
				rule-3)))
