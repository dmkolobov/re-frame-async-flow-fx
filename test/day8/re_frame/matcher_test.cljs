(ns day8.re-frame.async-flow-fx.matcher-test
	(:require [cljs.test :refer-macros [is deftest]]
						[day8.re-frame.matcher :as matcher]))

(deftest test-add-rule
	(is (= (matcher/add-rule {}
													 {:id :rule-1
														:events [[:foo :bar :car]
														         [:foo :bar :hello]]})
				 {:foo {:bar   {:car   #{:rule-1}
												:hello #{:rule-1}}}})))

(deftest test-remove-rule
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

(deftest test-matches
	(is (= (matcher/matching-rules {:foo {:bar {:car #{:rule-1}}
																				:other #{:rule-2}}}
																 [:foo :bar :car 42])
				 #{:rule-1})))
