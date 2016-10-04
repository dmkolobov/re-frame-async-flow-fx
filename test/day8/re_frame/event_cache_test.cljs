(ns day8.re-frame.async-flow-fx.event-cache-test
	(:require [cljs.test :refer-macros [is deftest]]
						[day8.re-frame.event-cache :as cache]))

(def rule-1 {:id     :rule-1
						 :events #{[:foo]
											 [:bar]
											 [:car]}})

(deftest test-add-rule
	(is (= (cache/add-rule {} rule-1)
				 {:rule-1 #{[:foo] [:bar] [:car]}})))

(deftest test-remove-rule
	(is (= (cache/remove-rule {:rule-1 #{[:foobar]}} :rule-1)
				 {})))

(deftest test-record-event
	(is (= (cache/record-event {:rule-1 #{[:foo] [:bar]}}
														 :rule-1
														 [:foo 42])
				 {:rule-1 #{[:bar]}})))

(let [seen        (cache/add-rule {} rule-1)
			with-events (fn [& events]
										(reduce #(cache/record-event %1 :rule-1 %2)
													  seen
													  events))]

	(deftest test-seen-all-of?
		(is (not (cache/seen-all-of? (with-events [:foo] [:bar]) rule-1)))
		(is (cache/seen-all-of? (with-events [:foo] [:bar] [:car]) rule-1)))

	(deftest test-seen-some-of?
		(is (not (cache/seen-some-of? seen rule-1)))
		(is (cache/seen-some-of? (with-events [:foo]) rule-1))))


