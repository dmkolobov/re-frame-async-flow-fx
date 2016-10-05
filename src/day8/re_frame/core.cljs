(ns day8.re-frame.core
	(:require
		[re-frame.core :as re-frame]

		[day8.re-frame.machine :as machine]))

(def forwarded-events
	(comp (mapcat :events)
				(map first)))

(def flow-path [:async-flow/state])

(def flow-interceptor
	(re-frame/->interceptor
		{:before (fn [context]
							 (update context
											 :coeffects
											 (fn [{:keys [db] :as coeffects}]
												 (assoc coeffects :flow-state (get-in db flow-path)))))
		 :after  (fn [context]
							 (update context
											 :effects
											 (fn [{:keys [flow-state] :as effects}]
												 (-> effects
														 (update :db assoc-in flow-path flow-state)
														 (dissoc :flow-state)))))}))

(re-frame/reg-event-fx
	:async-flow/initialize
	[flow-interceptor re-frame/trim-v]
	(fn [{:keys [flow-state]} [{:keys [id first-dispatch rules] :as flow}]]
		(let [new-rules (machine/compile flow)]
			{:flow-state     (machine/add-rules flow-state new-rules)
			 :dispatch       first-dispatch
			 :forward-events {:id          id
												:events      (into #{} forwarded-events new-rules)
												:dispatch-to :async-flow/step}})))

(re-frame/reg-event-fx
	:async-flow/step
	[flow-interceptor re-frame/trim-v]
	(fn [{:keys [flow-state]} [event-v]]
		(let [[flow-state dispatches] (machine/transition flow-state event-v)]
			{:dispatch-n dispatches :flow-state flow-state})))

(re-frame/reg-event-fx
	:async-flow/halt
	[flow-interceptor re-frame/trim-v]
	(fn [{:keys [flow-state]} [flow-id]]
		(machine/remove-rules flow-state flow-id)))

(re-frame/reg-fx
	:async-flow
	(fn [flow]
		(re-frame/dispatch :async-flow/initialize flow)))
