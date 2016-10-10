(ns day8.re-frame.matcher
	(:require [re-frame.core :refer [reg-event-fx trim-v]]
						[cljs.pprint :refer [pprint]]))

(defn add-rule
	"Given a matcher and a flow rule, return a new matcher which
	includes all of the events mentioned in the rule."
	[matcher {:keys [id events] :as rule}]
	(reduce (fn [matcher event-v]
						(update-in matcher
											 event-v
											 (fn [rule-set]
												 (if rule-set (conj rule-set id) #{id}))))
					matcher
					events))

(defn add-rules
	[matcher rules]
	(reduce add-rule matcher rules))

(defn- removable-path?
	[matcher path]
	(let [x (get-in matcher path)]
		(or (set? x)
				(and (map? x) (= 1 (count x))))))

(defn- event-dissoc-path
	[matcher event-v]
	(loop [path    []
				 event-v event-v]
		(if (and (seq event-v)
						 (removable-path? matcher event-v))
			(recur event-v (pop event-v))
			path)))

(defn- remove-event
	[matcher event-v]
	(let [path (event-dissoc-path matcher event-v)]
		(if (= 1 (count path))
			(dissoc matcher (first path))
			(update-in matcher (butlast path) dissoc (last path)))))

(defn remove-rule
	"Given a matcher and a flow rule, return a new matcher without
	all of the events mentioned in the rule."
	[matcher {:keys [id events] :as rule}]
	(reduce (fn [matcher event-v]
						(if-let [rule-set (get-in matcher event-v)]
							(if (> 1 (count rule-set))
								(assoc-in matcher event-v (disj rule-set id))
								(remove-event matcher event-v))
							matcher))
					matcher
					events))

(defn remove-rules
	[matcher rules]
	(reduce remove-rule matcher rules))

(defn matching-rules
	[matcher event-v]
	(when (seq event-v)
		(when-let [x (get matcher (first event-v))]
			(if (set? x) x (recur x (rest event-v))))))

