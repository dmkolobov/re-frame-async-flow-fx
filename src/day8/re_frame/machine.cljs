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
		:rules        (reduce #(assoc %1 (:id %2) %2) rules new-rules)
		:matcher      (reduce matcher/add-rule matcher new-rules)
		:seen-events  (reduce event-cache/add-rule seen-events new-rules)
		:flows        (reduce #(update %1 (keyword (namespace %2)) conj %2) flows (map :id new-rules))))

(defn remove-rules
	[{:keys [matcher seen-events rules flows] :as flow-state} flow-id]
	(let [rule-ids   (get flows flow-id)
				flow-rules (map #(get rules %) rule-ids)]
		(assoc flow-state
			:rules       (reduce dissoc rules rule-ids)
			:matcher     (reduce matcher/remove-rule matcher flow-rules)
			:seen-events (reduce dissoc seen-events rule-ids)
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
