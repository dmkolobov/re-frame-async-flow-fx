(ns day8.re-frame.async-flow-fx.rule-test
	(:require [cljs.test :refer-macros [is deftest]]
						[day8.re-frame.rule :as rule]
						[day8.re-frame.event-cache :as cache]))

(def rule-1
	{:id         :flow-1/rule-1
	 :when       cache/seen-all-of?
	 :events     #{[:success [:foo]]}
	 :dispatch-n [[:bar]]})

(def rule-2
	{:id         :flow-1/rule-2
	 :when       cache/seen-all-of?
	 :events     #{[:success [:bar]]}
	 :dispatch-n [[:success [:foobar]]]
	 :halt?      true})

(def rule-3
	{:id         :flow-1/rule-3
	 :when       cache/seen-some-of?
	 :events     #{[:error [:foo]] [:error [:bar]]}
	 :dispatch-n [[:error [:foobar]]]
	 :halt?      true})

(deftest test-rule-effects
	(is (= (rule/rule-effects rule-1) (:dispatch-n rule-1)))
	(is (= (rule/rule-effects rule-2) (conj (:dispatch-n rule-2) [:async-flow/halt :flow-1])))
	(is (= (rule/rule-effects rule-3) (conj (:dispatch-n rule-3) [:async-flow/halt :flow-1]))))
