(ns day8.re-frame.core
	(:require
		[re-frame.core :as re-frame]

		[day8.re-frame.flow :as machine]))

(def forwarded-events
	(comp (mapcat :events)
				(map first)))

(def flow-path [:async-flow/state])

(defn get-state
	[{:keys [db] :as coeffects}]
	(assoc coeffects :flow-state (get-in db flow-path)))

(defn set-state
	[{:keys [flow-state] :as effects}]
	(-> effects
			(update :db assoc-in flow-path flow-state)
			(dissoc :flow-state)))

(def flow-interceptor
	(re-frame/->interceptor
		{:before (fn [context]
							 (update context :coeffects get-state))

		 :after  (fn [context]
							 (update context :effects set-state))}))

;; event handlers

(re-frame/reg-event-fx
	:async-flow/initialize
	[flow-interceptor re-frame/trim-v]
	(fn [{:keys [flow-state]} [{:keys [id first-dispatch rules] :as flow}]]
		(let [new-rules (machine/compile flow)]
			{:flow-state     (machine/install flow-state flow)
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

;; fx handler

(re-frame/reg-fx
	:async-flow
	(fn [flow]
		(re-frame/dispatch :async-flow/initialize flow)))
