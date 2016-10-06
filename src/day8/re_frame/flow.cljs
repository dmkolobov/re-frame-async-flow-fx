(ns day8.re-frame.flow
	(:require [re-frame.core :as re-frame]
						[day8.re-frame.rule :as rule]
						[day8.re-frame.matcher :as matcher]
						[clojure.set :as set]))


(defrecord FlowState
	[flows rules matcher fired-rules])

(def fresh-state
	(FlowState. {} {} {} #{}))

(defn- add-rules
	[rules new-rules]
	(persistent!
		(reduce (fn [rules {:keys [id] :as rule}]
							(assoc! rules id rule))
						(transient rules)
						new-rules)))

(defn- remove-rules
	[rules removed-rules]
	(persistent!
		(reduce (fn [rules {:keys [id]}]
							(dissoc! rules id))
						(transient rules)
						removed-rules)))

(defn install
	"Incorporate the rules of the given flow into the machine state."
	[{:keys [matcher rules flows] :as flow-state} {:keys [id] :as flow}]
	(let [new-rules (->> (:rules flow)
											 (flatten)
											 (map-indexed #(rule/compile (:id flow) %1 %2)))]
		(assoc flow-state
			:rules    (add-rules rules new-rules)
			:matcher  (matcher/add-rules matcher new-rules)
			:flows    (assoc flows (:id flow) new-rules))))

(defn uninstall
	"Remove all traces of the given flow from the machine state."
	[{:keys [fired-rules matcher rules flows] :as flow-state} flow-id]
	(let [flow-rules (get flows flow-id)]
		(assoc flow-state
			:rules       (remove-rules rules flow-rules)
			:matcher     (matcher/remove-rules matcher flow-rules)
			:flows       (dissoc flows flow-id)
			:fired-rules (set/difference fired-rules
																	 (into #{} (map :id flow-rules))))))

(defn transition
	"Given a machine state and an event vector, return a tuple [machine-state dispatches],
	where machine-state is the state of the machine after seeing the event, and dispatches
	are the events that should be dispatched after seeing the event."
	[{:keys [matcher fired-rules rules] :as flow-state} event-v]
	(let [changed-rules (->> (set/difference (matcher/matching-rules matcher event-v)
																					 fired-rules)
													 (map #(get rules %))
													 (map #(rule/record % event-v)))
				ready-rules   (filter rule/ready? changed-rules)]
		[(-> flow-state
				 (update :rules add-rules changed-rules)
				 (update :fired-rules into (map :id ready-rules)))
		 (mapcat rule/fire ready-rules)]))
