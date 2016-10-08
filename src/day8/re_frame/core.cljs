(ns day8.re-frame.core
	(:require
		[re-frame.core :as re-frame]

		[day8.re-frame.flow :as flow]))

(defn get-state
	[{:keys [db event] :as coeffects}]
	(let [[path & _] event]
		(assoc coeffects
			:time-machine (if-let [state (get-in db path)]
											state
											flow/fresh-state))))

(defn set-state
	[{:keys [time-machine] :as effects} path]
	(-> effects
			(update :db assoc-in path time-machine)
			(dissoc :time-machine)))

(def flow-interceptor
	(re-frame/->interceptor
		{:before (fn [context]
							 (update context :coeffects get-state))

		 :after  (fn [context]
							 (let [path (first (get-in context [:coeffects :event]))]
								 (update context :effects set-state path)))}))

(def intercept [re-frame/trim-v flow-interceptor])

;; event handlers
;;
(defn normalize-dispatches
	[first-dispatch first-dispatches flow]
	(if first-dispatch [first-dispatch] first-dispatches))

(defn init-flow
	[{:keys [time-machine]} [path {:keys [id first-dispatch first-dispatches] :as flow}]]
	(let [[time-machine' deps] (flow/install time-machine flow)
				dispatches         (normalize-dispatches first-dispatch first-dispatches flow)]
		{:time-machine     time-machine'
		 :dispatch-n     dispatches
		 :forward-events {:id          id
											:events      deps
											:dispatch-to [:async-flow/transition (:flow-path time-machine)]}}))

(re-frame/reg-event-fx :async-flow/init intercept init-flow)

(defn transition-flow
	[{:keys [time-machine]} [path event-v]]
		(let [[time-machine dispatches] (flow/transition time-machine event-v)
					halt-xform                (map (fn [[id & _ :as dispatch]]
																					 (if (= :async-flow/halt id)
																						 [:async-flow/halt path id]
																						 dispatch)))]
			{:dispatch-n   (into [] halt-xform dispatches)
			 :time-machine time-machine}))

(re-frame/reg-event-fx :async-flow/transition intercept transition-flow)

(defn halt-flow
	[{:keys [time-machine]} [path flow-id]]
	{:time-machine   (flow/uninstall time-machine flow-id)
	 :forward-events {:unregister flow-id}})

(re-frame/reg-event-fx :async-flow/halt intercept halt-flow)

;; fx handler

(defn unique-flow-id
	[]
	(let [s (gensym)] (keyword (str "flow-" s))))

(defn fx-handler
	[{:keys [db-path id]
		:or   {db-path [:async-flow/state]
					 id      (unique-flow-id)}
		:as   flow}]
	(re-frame/dispatch [:async-flow/init db-path flow]))

(re-frame/reg-fx :async-flow fx-handler)
