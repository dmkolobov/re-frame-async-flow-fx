(ns day8.re-frame.rule
	(:require [re-frame.core :as re-frame]))

(defprotocol IAmRule
	(fire [_]
	 "Return a sequence of events which should be dispatched when this rule is fired.")

	(record [_ event-v]
	 "Return a new rule which has seen the given event.")

	(ready? [_]
		"Returns true if the event should be fired."))

;; ---- when predicates ----

(defn matches?
	[pattern event-v]
	(and (<= (count pattern) (count event-v))
			 (= (subvec event-v 0 (count pattern))
		       pattern)))

(defn seen-all?
	"Returns true if the rule has seen all of its required events."
	[{:keys [events seen-events]}]
	(empty? (reduce (fn [patterns event]
									  (remove #(matches? % event) patterns))
								  events
								  seen-events)))

(defn seen-any?
	"Returns true if the rule has seen any of its required events."
	[{:keys [events seen-events]}]
	(> (count events) (- (count events) (count seen-events))))

;; ---- rule definition ----

(defrecord Rule
	[id when-fn events dispatch-n halt? trace? seen-events]

	IAmRule

	(ready? [this]
		(when-fn this))

	(fire [_]
		(cond-> (if trace?
							(mapv #(into % seen-events) dispatch-n)
							dispatch-n)
						halt? (conj [:async-flow/halt (keyword (namespace id))])))

	(record [this event-v]
		(assoc this :seen-events (conj seen-events event-v))))

;; ---- rule compilation ----


(def map-when->fn {:seen?        seen-all?
									 :seen-both?   seen-all?
									 :seen-all-of? seen-all?
									 :seen-any-of? seen-any?})

(defn when->fn
	[when-kw]
	(if-let [when-fn (map-when->fn when-kw)]
		when-fn
		(re-frame/console :error  "async-flow: got bad value for :when - " when-kw)))

(defn normalize-dispatch
	[dispatch dispatch-n rule]
	(cond
		dispatch-n (if dispatch
								 (re-frame/console :error "async-flow: rule can only specify one of :dispatch and :dispatch-n. Got both: " rule)
								 dispatch-n)
		dispatch   [dispatch]
		:else      '()))

(defn normalize-events
	[event events rule]
	(cond
		events (if event
						 (re-frame/console :error "async-flow: rules can only specify one of :event and :events. Got both: " rule)
						 (set events))
		event  #{event}
		:else  (re-frame/console :error "async-flow: must specify one of :event or :events. Got none: " rule)))

(defn compile
	[flow-id index {:keys [id when event events dispatch dispatch-n halt? trace?] :as rule}]
	(map->Rule
		{:id          (keyword (name flow-id)
													 (if id
														 (name id)
														 (str "rule-" (inc index))))
		 :halt?       (or halt? false)
		 :trace?    (or trace? false)
		 :when-fn     (when->fn when)
		 :events      (normalize-events event events rule)
		 :dispatch-n  (normalize-dispatch dispatch dispatch-n rule)
		 :seen-events []}))
