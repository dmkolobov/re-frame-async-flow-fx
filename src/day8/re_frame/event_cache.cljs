(ns day8.re-frame.event-cache
	(:require [clojure.set :refer [difference]]))

(defn add-rules
	[event-cache rules]
	(persistent!
		(reduce (fn [t-cache {:keys [id events]}]
							(assoc! t-cache id events))
						(transient event-cache)
						rules)))

(defn remove-rules
	[event-cache rule-ids]
	(persistent!
		(reduce dissoc!
						(transient event-cache)
						rule-ids)))

(defn matches?
	[event-v event-pattern]
	(= event-pattern (subvec event-v 0 (count event-pattern))))

(defn record-event
	[event-cache id event-v]
	(update event-cache
					id
					(fn [patterns]
						(into #{} (remove #(matches? event-v %) patterns)))))

(defn seen-all-of?
	[event-cache {:keys [id] :as rule}]
	(empty? (get event-cache id)))

(defn seen-some-of?
	[event-cache {:keys [id events] :as rule}]
	(some? (seq (difference events (get event-cache id)))))


