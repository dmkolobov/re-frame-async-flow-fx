(ns day8.re-frame.machine
	(:require
		[re-frame.core :as re-frame :refer [trim-v]]
		[day8.re-frame.event-cache :as event-cache]
		[day8.re-frame.matcher :as matcher]))

(defrecord FlowState
	[flows rules matcher seen-events fired-rules])

(def fresh-state
	(FlowState. {} {} {} {} #{}))

(defn add-rules
	[{:keys [matcher seen-events rules flows] :as flow-state} new-rules]
	(assoc flow-state
		:rules        (reduce #(assoc %1 (:id %2) %2) rules new-rules)
		:matcher      (reduce matcher/add-rule matcher new-rules)
		:seen-events  (reduce event-cache/add-rule seen-events new-rules)
		:flows        (reduce #(update %1 (keyword (namespace %2)) conj %2) flows (map :id new-rules))))

(defn rule-actions
	"Given a rule, return the events this rule should dispatch when ready."
	[{:keys [id halt? dispatch-n]}]
	(if-let [halt-event (when halt? [:async-flow/halt (keyword (namespace id))])]
		(conj dispatch-n halt-event)
		dispatch-n))

(defn record-event
	"Given a machine state and an event vector, return a tuple [machine-state dispatches],
	where machine-state is the state of the machine after seeing the event, and dispatches
	are the events that should be dispatched after seeing the event."
	[{:keys [seen-events matcher fired-rules rules] :as flow-state} event-v]
	(let [match-ids   (matcher/matching-rules matcher event-v)
				seen-events (reduce #(event-cache/record-event %1 %2 event-v) seen-events match-ids)
				ready-rules (filter #((:when %) seen-events) (map #(get rules %) match-ids))]
		[(assoc flow-state
			 :seen-events seen-events
			 :fired-rules (into fired-rules (map :id ready-rules)))
		 (mapcat rule-actions ready-rules)]))

;; ---- events handlers ----

(re-frame/reg-event-fx
	:async-flow/initialize
	;;[flow-interceptor trim-v]
	(fn [{:keys [flow-state]} {:keys [id first-dispatch rules] :as flow}]
		(let [new-rules []]
			{:flow-state     (add-rules flow-state new-rules)
			 :dispatch       first-dispatch
			 :forward-events {:id          id
												:events      (->> new-rules (mapcat :events) (map first))
												:dispatch-to :async-flow/step}})))

(re-frame/reg-event-fx
	:async-flow/step
	;;[flow-interceptor trim-v]
	(fn [{:keys [flow-state]} [event-v]]
		(let [[flow-state dispatches] (record-event flow-state event-v)]
			{:dispatch-n dispatches
			 :flow-state flow-state})))

(re-frame/reg-event-fx
	:async-flow/halt
	;;[flow-interceptor trim-v]
	(fn [{:keys [flow-state]} flow-id]
		(let [{:keys [matcher seen-events rules]} flow-state]
			())))
