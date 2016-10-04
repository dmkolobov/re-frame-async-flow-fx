(ns day8.re-frame.event-cache
	(:require [clojure.set :refer [difference]]))

(defn add-rule
	[event-cache {:keys [id events]}]
	(assoc event-cache id events))

(defn remove-rule
	[event-cache id]
	(dissoc event-cache id))

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


