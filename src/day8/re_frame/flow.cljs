(ns day8.re-frame.flow
	(:require [re-frame.core :as re-frame]
						[day8.re-frame.event-cache :as event-cache]))


(defrecord Rule [id when events dispatch-n halt?])

(def map-when->fn {:seen?        event-cache/seen-all-of?
									 :seen-both?   event-cache/seen-all-of?
									 :seen-all-of? event-cache/seen-all-of?
									 :seen-any-of? event-cache/seen-some-of?})

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
		dispatch   (list dispatch)
		:else      '()))

(defn normalize-events
	[event events rule]
	(cond
		events (if event
						 (re-frame/console :error "")
						 (set events))
		event  #{event}
		:else  (re-frame/console :error "")))

(defn spec->rule
	[flow-id index {:keys [id when event events dispatch dispatch-n halt?] :as rule}]
	(map->Rule
		{:id         (keyword (name flow-id)
													(if id
														(name id)
														(str "rule-" (inc index))))
		 :halt?      (or halt? false)
		 :when       (when->fn when)
		 :events     (normalize-events event events rule)
		 :dispatch-n (normalize-dispatch dispatch dispatch-n rule)}))

(defn causality-seq->rules
	[causality-seq]
	(loop [[cause effect] (first causality-seq)
				 clauses        (rest causality-seq)
				 rules          []]
		(if cause
			(recur (first clauses)
						 (rest clauses)
						 (conj rules {:when     :seen?
													:event    cause
													:dispatch effect}))
			rules)))

(defn fire-rule
	"Given a rule, return the events that should be dispatched when the rule is fired."
	[{:keys [id halt? dispatch-n]}]
	(if-let [halt-event (when halt? [:async-flow/halt (keyword (namespace id))])]
		(conj dispatch-n halt-event)
		dispatch-n))

(defn compile
	"Given a machine specification, return a sequence of normalized rules
	which can be added to an existing machine."
	[{:keys [id rules]}]
	(->> rules
			 (mapcat #(if (map? %) [%] (causality-seq->rules %)))
			 (map-indexed #(spec->rule id %1 %2))))
