(ns day8.re-frame.core
	(:require
		[re-frame.core :as re-frame]

		[day8.re-frame.flow :as flow]))

(defn get-state
	[{:keys [db event] :as coeffects}]
	(let [[flow-path & event'] event]
		(assoc coeffects
			:flow-state (if-let [state (get-in db flow-path)]
										state
										(assoc flow/fresh-state :flow-path flow-path))
			:event      event')))

(defn set-state
	[{:keys [flow-state] :as effects}]
	(-> effects
			(update :db assoc-in (:flow-path flow-state) flow-state)
			(dissoc :flow-state)))

(def flow-interceptor
	(re-frame/->interceptor
		{:before (fn [context]
							 (update context :coeffects get-state))

		 :after  (fn [context]
							 (update context :effects set-state))}))

(def intercept [re-frame/trim-v flow-interceptor])

;; event handlers
;;
(defn normalize-dispatches
	[first-dispatch first-dispatches flow]
	(if first-dispatch [first-dispatch] first-dispatches))

(defn init-flow
	[{:keys [flow-state]} [{:keys [id first-dispatch first-dispatches] :as flow}]]
	(let [[flow-state' deps] (flow/install flow-state flow)
				dispatches         (normalize-dispatches first-dispatch first-dispatches flow)]
		{:flow-state     flow-state'
		 :dispatch-n     dispatches
		 :forward-events {:id          id
											:events      deps
											:dispatch-to [:async-flow/transition (:flow-path flow-state)]}}))

(re-frame/reg-event-fx :async-flow/init intercept init-flow)

(defn transition-flow
	[{:keys [flow-state]} [event-v]]
		(let [[flow-state dispatches] (flow/transition flow-state event-v)]
			{:dispatch-n dispatches
			 :flow-state flow-state}))

(re-frame/reg-event-fx :async-flow/transition intercept transition-flow)

(defn halt-flow
	[{:keys [flow-state]} [flow-id]]
	{:flow-state     (flow/uninstall flow-state flow-id)
	 :forward-events {:unregister flow-id}})

(re-frame/reg-event-fx :async-flow/halt intercept halt-flow)

;; fx handler

(re-frame/reg-fx
	:async-flow
	(fn [{:keys [db-path] :as flow}]
		(re-frame/dispatch [:async-flow/init db-path flow])))
