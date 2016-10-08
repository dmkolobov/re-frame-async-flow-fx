(ns day8.re-frame.core
	(:require
		[re-frame.core :as re-frame]

		[day8.re-frame.flow :as flow]))

;; -------- state ---------

(defn unique-flow-id
	"Generate a unique flow id."
	[]
	(let [s (gensym)] (keyword (str "flow-" s))))

(defn fx-handler
	"Given a flow, dispatch an event to install the flow into the time machine.
	State will be stored in the db-path of the flow, or in the path [:async-flow/state]."
	[{:keys [db-path id]
		:or   {db-path [:async-flow/state]
					 id      (unique-flow-id)}
		:as   flow}]
	(re-frame/dispatch [:async-flow/init db-path flow]))

(re-frame/reg-fx :async-flow fx-handler)

;; -------- state ---------

(defn get-state
	"All time machine events have the db-path of the machine as their first non-id element.
	Given a coeffects map, adds a key :time-machine to the coeffects with the machine state
	stored at the event's db-path as the value."
	[{:keys [db event] :as coeffects}]
	(let [[path & _] event]
		(assoc coeffects
			:time-machine (if-let [state (get-in db path)]
											state
											flow/fresh-state))))

(defn set-state
	"Given an effects map containing a :time-machine key and a db-path, stores the
	value of the :time-machine key in the db under db-path"
	[{:keys [time-machine] :as effects} db-path]
	(-> effects
			(update :db assoc-in db-path time-machine)
			(dissoc :time-machine)))

(def flow-interceptor
	(re-frame/->interceptor
		{:before (fn get-state-ctx [context]
							 (update context :coeffects get-state))

		 :after  (fn set-state-ctx [context]
							 (let [path (first (get-in context [:coeffects :event]))]
								 (update context :effects set-state path)))}))

(def intercept [re-frame/trim-v flow-interceptor])

;; -------- event handlers ---------

(defn normalize-dispatches
	[first-dispatch first-dispatches flow]
	(if first-dispatch [first-dispatch] first-dispatches))

(defn init
	[{:keys [time-machine]} [db-path {:keys [id first-dispatch first-dispatches] :as flow}]]
	(let [[time-machine' deps] (flow/install time-machine flow)
				dispatches           (normalize-dispatches first-dispatch first-dispatches flow)]
		{:time-machine   time-machine'
		                 ;; kick off processing by dispatching first events.
		 :dispatch-n     dispatches
		                 ;; start forwarding the flow's event dependencies.
		 :forward-events {:id          id
											:events      deps
											:dispatch-to [:async-flow/transition (:flow-path time-machine)]}}))

(re-frame/reg-event-fx :async-flow/init intercept init)

(defn qualify-halts
	[db-path dispatches]
	(map (fn [[event-id flow-id :as dispatch]]
				 (if (= :async-flow/halt event-id)
					 [:async-flow/halt db-path flow-id]
					 dispatch))
			 dispatches))

(defn transition
	"Transition the machine state at db-path and dispatch all events returned by
	the transition."
	[{:keys [time-machine]} [db-path event-v]]
		(let [[time-machine' dispatches] (flow/transition time-machine event-v)]
			               ;; add db-path to any :async-flow/halt events.
			{:dispatch-n   (qualify-halts db-path dispatches)
			 :time-machine time-machine'}))

(re-frame/reg-event-fx :async-flow/transition intercept transition)

(defn halt
	"Uninstall the flow-id flow from the machine state at db-path, and unregister
	any forwarded events associated with that flow."
	[{:keys [time-machine]} [db-path flow-id]]
	                 ;; remove rules and matcher entries from machine state
	{:time-machine   (flow/uninstall time-machine flow-id)
	                 ;; stop forwarding the flow's event dependencies
	 :forward-events {:unregister flow-id}})

(re-frame/reg-event-fx :async-flow/halt intercept halt)
