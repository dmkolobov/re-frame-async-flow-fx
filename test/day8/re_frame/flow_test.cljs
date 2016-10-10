(ns day8.re-frame.async-flow-fx.flow-test
	(:require [cljs.test :refer-macros [is deftest]]
						[day8.re-frame.rule :as rule]
						[day8.re-frame.flow :as flow]))

(def flow-1
	{:id      :flow-1
	 :db-path [:path]
	 :rules [{:id          :rule-1
					  :when        :seen?
					  :events      #{[:success :foo]}
					  :dispatch    [:bar]}

					 {:id          :rule-2
						:when        :seen?
						:events      #{[:success :bar] [:hello]}
						:dispatch-n  [[:success :foobar] [:other-success]]
						:halt?       true
						:capture?    true}

					 {:id          :rule-3
						:when        :seen-any-of?
						:events      #{[:error :foo] [:error :bar]}
						:dispatch    [:error :foobar]
						:halt?       true}]})

(defn play
	[flow & events]
	(loop [state  (first
									(flow/install flow/fresh-state flow))
				 fired  []
				 events events]
		(if (seq events)
			(let [[event & events'] events
						[state' dispatches] (flow/transition state event)]
				(recur state'
							 (into (conj fired event) dispatches)
							 events'))
			fired)))

(deftest test-deps
	(is (= (second
					 (flow/install flow/fresh-state flow-1))
				 #{:success :error :hello})))

(deftest test-transition
	(is (= (play flow-1 [:success :foo])
				 [[:success :foo]
					[:bar]]))

	(is (= (play flow-1 [:success :foo] [:success :foo] [:success :foo])
				 [[:success :foo]
					[:bar]
					[:success :foo]
					[:success :foo]]))

	(is (= (play flow-1 [:success :foo] [:hello])
				 [[:success :foo]
					[:bar]
					[:hello]]))

	(is (= (play flow-1 [:success :foo] [:hello :world] [:success :bar :data])
				 [[:success :foo]
					[:bar]
					[:hello :world]
					[:success :bar :data]
					[:success :foobar [:hello :world] [:success :bar :data]]
					[:other-success [:hello :world] [:success :bar :data]]
					[:async-flow/halt :flow-1]]))

	(is (= (play flow-1 [:error :foo])
				 [[:error :foo]
					[:error :foobar]
					[:async-flow/halt :flow-1]]))

	(is (= (play flow-1 [:success :foo] [:error :bar])
				 [[:success :foo]
					[:bar]
					[:error :bar]
					[:error :foobar]
					[:async-flow/halt :flow-1]])))

(deftest test-uninstall
	(is (= (flow/uninstall (first (flow/install flow/fresh-state flow-1))
												 :flow-1)
				 flow/fresh-state)))
