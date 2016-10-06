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

(defn seen-all?
	"Returns true if the rule has seen all of its required events."
	[{:keys [events seen-events]}]
	(= (count events) (count seen-events)))

(defn seen-any?
	"Returns true if the rule has seen any of its required events."
	[{:keys [events seen-events]}]
	(> (count events) (- (count events) (count seen-events))))

;; ---- rule definition ----

(defrecord Rule
	[id when-fn events dispatch-n halt? capture? seen-events]

	IAmRule

	(ready? [this]
		(when-fn this))

	(fire [_]
		(if halt?
			(conj (if capture?
							(mapv (fn [event-v]
										(into event-v seen-events))
									dispatch-n)
							dispatch-n)
						[:async-flow/halt (keyword (namespace id))])
			dispatch-n))

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
						 (re-frame/console :error "")
						 (set events))
		event  #{event}
		:else  (re-frame/console :error "")))

(defn compile
	[flow-id index {:keys [id when event events dispatch dispatch-n halt? capture?] :as rule}]
	(map->Rule
		{:id          (keyword (name flow-id)
													 (if id
														 (name id)
														 (str "rule-" (inc index))))
		 :halt?       (or halt? false)
		 :capture?    (or capture? false)
		 :when-fn     (when->fn when)
		 :events      (normalize-events event events rule)
		 :dispatch-n  (normalize-dispatch dispatch dispatch-n rule)
		 :seen-events []}))
