(ns day8.re-frame.machine
	(:require
		[re-frame.core :as re-frame :refer [trim-v]]
		[day8.re-frame.event-cache :as event-cache]
		[day8.re-frame.matcher :as matcher]))

(defrecord FlowState
	[flows rules matcher seen-events fired-rules])

(defn matching-rules
	"Given a machine state and an event vector, return the IDs of
	all rules which mention this event in their rules."
	[{:keys [matcher]} event-v]
	(matcher/matching-rules matcher event-v))

(defn record-event
	"Given a machine state, a list of rule IDs, and an event vector,
	return a new machine state with that event being seen by all the
	provided rules."
	[flow-state event-v rule-ids]
	(update flow-state
					:seen-events
					(fn [seen]
						(reduce (fn [seen id]
											(event-cache/record-event seen id event-v))
										seen
										rule-ids))))

(defn filter-ready
	"Given a machine state and a seq of rule-ids, return a seq of rules
	from these IDs which are ready to be fired."
	[{:keys [rules seen-events]} rule-ids]
	(filter (fn [rule] ((:when rule) seen-events rule))
					(map #(get rules %) rule-ids)))

(defn rule-actions
	"Given a rule, return the events this rule should dispatch when ready."
	[{:keys [flow-id halt? dispatch-n]}]
	(if-let [halt-event (when halt? [:async/flow-halt flow-id])]
		(conj dispatch-n halt-event)
		dispatch-n))

;; ---- events handlers ----

(re-frame/reg-event-fx
	:async-flow/initialize
	;;[flow-interceptor trim-v]
	(fn [{:keys [flow-state]} {:keys [id first-dispatch rules] :as flow}]
		(let [new-rules [] ;; (massage-rules id rules)
					{:keys [matcher seen-events rules flows]} flow-state]

			{:flow-state     (assoc flow-state
												 :rules        (reduce #(assoc %1 (:id %2) %2)
																			  			 rules
																			  			 new-rules)
												 :matcher      (reduce matcher/add-rule matcher new-rules)
												 :seen-events  (reduce event-cache/add-rule seen-events new-rules)
												 :flows        (assoc flows id (map :id new-rules)))

			 :dispatch       first-dispatch

			 :forward-events {:id          id
												:events      (->> new-rules (mapcat :events) (map first))
												:dispatch-to :async-flow/step}})))

(re-frame/reg-event-fx
	:async-flow/step
	;;[flow-interceptor trim-v]
	(fn [{:keys [flow-state]} [event-v]]
		(let [rule-ids    (matching-rules flow-state event-v)
					flow-state  (record-event flow-state event-v rule-ids)
					ready-rules (filter-ready flow-state rule-ids)
					dispatches  (mapcat rule-actions ready-rules)]
			{:dispatch-n dispatches
			 :flow-state (update flow-state
													 :fired-rules
													 into
													 (map :id ready-rules))})))

(re-frame/reg-event-fx
	:async-flow/halt
	;;[flow-interceptor trim-v]
	(fn [{:keys [flow-state]} flow-id]
		(let [{:keys [matcher seen-events rules]} flow-state]
			())))
