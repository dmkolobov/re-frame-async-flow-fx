(ns day8.re-frame.flow
	(:require [re-frame.core :as re-frame]
						[day8.re-frame.rule :as rule]
						[day8.re-frame.matcher :as matcher]
						[clojure.set :as set]))

(defrecord MachineState
	[flows rules matcher fired-rules])

(def fresh-state
	(MachineState. {} {} {} #{}))

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

(def dep-xform
	(comp (mapcat :events)
				(map first)))

(defn install
	"Incorporate the rules of the given flow into the machine state."
	[{:keys [matcher rules flows] :as time-machine} {:keys [id] :as flow}]
	(let [new-rules (->> (:rules flow)
											 (flatten)
											 (map-indexed (fn [idx spec]
																			(rule/compile (:id flow) idx spec))))]
		[(assoc time-machine
			:rules    (add-rules rules new-rules)
			:matcher  (matcher/add-rules matcher new-rules)
			:flows    (assoc flows (:id flow) new-rules))
		 (into #{} dep-xform new-rules)]))

(defn uninstall
	"Remove all traces of the given flow from the machine state."
	[{:keys [fired-rules matcher rules flows] :as time-machine} flow-id]
	(let [flow-rules (get flows flow-id)]
		(assoc time-machine
			:rules       (remove-rules rules flow-rules)
			:matcher     (matcher/remove-rules matcher flow-rules)
			:flows       (dissoc flows flow-id)
			:fired-rules (set/difference fired-rules
																	 (into #{} (map :id flow-rules))))))

(defn transition
	"Given a machine state and an event vector, return a tuple of the machine
	state after seeing the event, and the seq of events to dispatch resulting
	from the rules fired."
	[{:keys [matcher fired-rules rules] :as time-machine} event-v]
	(let [changed-rules (->> (set/difference (matcher/matching-rules matcher event-v)
																					 fired-rules)
													 (map #(get rules %))
													 (map #(rule/record % event-v)))
				ready-rules   (filter rule/ready? changed-rules)]
		[(-> time-machine
				 (update :rules add-rules changed-rules)
				 (update :fired-rules into (map :id ready-rules)))
		 (mapcat rule/fire ready-rules)]))
