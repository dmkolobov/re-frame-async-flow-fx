(ns day8.re-frame.async-flow-fx.matcher-test
	(:require [cljs.test :refer-macros [is deftest]]
						[day8.re-frame.matcher :as matcher]))

(deftest test-remove-event
	(is (= (matcher/remove-rule {:foo {:bar   {:car #{:rule-1}}
																		 :other {:hello #{:rule-2}}}}
															{:id     :rule-1
															 :events [[:foo :bar :car]]})
				 {:foo {:other {:hello #{:rule-2}}}}))

	(is (= (matcher/remove-rule {:foo {:bar   {:car   #{:rule-1}
																		         :hello #{:rule-2}}}}
															{:id     :rule-1
															 :events [[:foo :bar :car]]})
				 {:foo {:bar {:hello #{:rule-2}}}}))

	(is (= (matcher/remove-rule {:foo {:bar   {:car   #{:rule-1}
																						 :hello #{:rule-1}}}}
															{:id     :rule-1
															 :events [[:foo :bar :car]
																				[:foo :bar :hello]]})
				 {}))

	(is (= (matcher/remove-rule {:foo #{:rule1}}
															{:id :rule-1 :events [[:foo]]})
				 {})))

