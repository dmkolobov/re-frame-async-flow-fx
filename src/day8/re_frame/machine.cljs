(ns day8.re-frame.machine
	(:require
		[re-frame.core :as re-frame :refer [trim-v]]
		[day8.re-frame.event-cache :as event-cache]
		[day8.re-frame.matcher :as matcher]
		[day8.re-frame.flow :as flow]))

(defrecord FlowState
	[flows rules matcher seen-events fired-rules])

(def fresh-state
	(FlowState. {} {} {} {} #{}))

(defn install
	[{:keys [matcher seen-events rules flows] :as flow-state} flow]
	(let [new-rules (:rules flow)]
		(assoc flow-state
			:rules        (persistent!
											(reduce #(assoc! %1 (:id %2) %2) (transient rules) new-rules))
			:matcher      (matcher/add-rules matcher new-rules)
			:seen-events  (event-cache/add-rules seen-events new-rules)
			:flows        (assoc flows (:id flow) (map :id new-rules)))))

(defn uninstall
	[{:keys [matcher seen-events rules flows] :as flow-state} flow-id]
	(let [rule-ids   (get flows flow-id)
				flow-rules (map #(get rules %) rule-ids)]
		(assoc flow-state
			:rules       (persistent!
										 (reduce dissoc! (transient rules) rule-ids))
			:matcher     (matcher/remove-rules matcher flow-rules)
			:seen-events (event-cache/remove-rules seen-events rule-ids)
			:flows       (dissoc flows flow-id))))

(defn transition
	"Given a machine state and an event vector, return a tuple [machine-state dispatches],
	where machine-state is the state of the machine after seeing the event, and dispatches
	are the events that should be dispatched after seeing the event."
	[{:keys [seen-events matcher fired-rules rules] :as flow-state} event-v]
	(let [match-ids   (matcher/matching-rules matcher event-v)
				seen-events (reduce #(event-cache/record-event %1 %2 event-v) seen-events match-ids)
				ready-rules (filter #((:when %) seen-events %) (map #(get rules %) match-ids))]
		[(assoc flow-state
			 :seen-events seen-events
			 :fired-rules (into fired-rules (map :id ready-rules)))
		 (mapcat flow/fire-rule ready-rules)]))
