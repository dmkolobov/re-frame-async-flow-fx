(ns day8.re-frame.machine
	(:require
		[re-frame.core :as re-frame :refer [trim-v]]
		[day8.re-frame.event-cache :as event-cache]
		[day8.re-frame.matcher :as matcher]
		[day8.re-frame.rule :as rule]))

(defrecord FlowState
	[flows rules matcher seen-events fired-rules])

(def fresh-state
	(FlowState. {} {} {} {} #{}))

(defn add-rules
	[{:keys [matcher seen-events rules flows] :as flow-state} new-rules]
	(assoc flow-state
		:rules        (persistent!
										(reduce #(assoc! %1 (:id %2) %2) (transient rules) new-rules))
		:matcher      (matcher/add-rules matcher new-rules)
		:seen-events  (event-cache/add-rules seen-events new-rules)
		:flows        (reduce #(update %1 (keyword (namespace %2)) conj %2) flows (map :id new-rules))))

(defn remove-rules
	[{:keys [matcher seen-events rules flows] :as flow-state} flow-id]
	(let [rule-ids   (get flows flow-id)
				flow-rules (map #(get rules %) rule-ids)]
		(assoc flow-state
			:rules       (persistent! (reduce dissoc! (transient rules) rule-ids))
			:matcher     (matcher/remove-rules matcher flow-rules)
			:seen-events (event-cache/remove-rules seen-events rule-ids)
			:flows       (dissoc flows flow-id))))

(defn compile
	"Given a machine specification, return a sequence of normalized rules
	which can be added to an existing machine."
	[{:keys [id rules]}]
	(->> rules
			 (mapcat #(if (map? %) [%] (rule/causality-seq->rules %)))
			 (map-indexed #(rule/spec->rule id %1 %2))))

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
		 (mapcat rule/rule-effects ready-rules)]))
