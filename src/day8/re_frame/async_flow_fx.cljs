(ns day8.re-frame.async-flow-fx
  (:require
    [re-frame.core :as re-frame]
    [clojure.set :as set]
		[cljs.pprint]
    [day8.re-frame.forward-events-fx]))

(defn conj-event
	[trie event-v]
	(assoc-in trie event-v true))

(defn seen?
	"Given an event trie and an event vector, return true if
	the event vector is in the event trie."
	[trie event-v]
	(if (seq event-v)
		(let [[x & r] event-v]
			(if-let [t (get trie x)]
				(recur t r)
				false))
		true))

(defn seen-all-of?
  [required-patterns seen-events]
	(every? (partial seen? seen-events) required-patterns))

(defn seen-any-of?
  [required-patterns seen-events]
	(some? (some (partial seen? seen-events) required-patterns)))

(defn startable-rules
  "Given the accumulated set of seen events and the set of rules already started,
  return the list of rules which should now be started"
  [rules now-seen-events rules-fired]
  (->> (remove (comp rules-fired :id) rules)
       (filterv (fn [task] ((:when task) (:events task) now-seen-events)))))


(def map-when->fn {:seen?        seen-all-of?
									 :seen-both?   seen-all-of?
									 :seen-all-of? seen-all-of?
									 :seen-any-of? seen-any-of?})

(defn when->fn
	[when-kw]
	(if-let [when-fn (map-when->fn when-kw)]
		when-fn
		(re-frame/console :error  "async-flow: got bad value for :when - " when-kw)))

(defn normalize-events
	[{:keys [event events] :as rule}]
	(assoc rule
		:events (if event
							(if (keyword? event)
								[[event]]
								[event])
							(mapv (fn [pattern]
											(if (keyword? pattern) [pattern] pattern))
										events))))

(defn create-dispatch
	[dispatch dispatch-n rule]
	(cond
		dispatch-n (if dispatch
								 (re-frame/console :error "async-flow: rule can only specify one of :dispatch and :dispatch-n. Got both: " rule)
								 dispatch-n)
		dispatch   (list dispatch)
		:else      '()))

(defn expand-sequence-rule
	[{:keys [events step-success dispatch dispatch-n halt?] :as rule}]
	(loop [rules  []
				 prev   (first events)
				 events (rest events)]
		(if (seq events)
			(recur (conj rules
									 {:when     :seen?
										:events   (list (conj step-success prev))
										:dispatch (first events)})
						 (first events)
						 (rest events))
			(conj rules
						{:when       :seen?
						 :events     (list (conj step-success prev))
						 :dispatch-n (create-dispatch dispatch dispatch-n rule)
						 :halt?      halt?}))))

(defn massage-rule
  "Massage the supplied rules as follows:
    - replace `:when` keyword value with a function implementing the predicate
    - ensure that only `:dispatch` or `:dispatch-n` is provided
    - add a unique :id, if one not already present"
  [index {:as rule :keys [id when event events dispatch dispatch-n halt?]}]
	{:id         (or id index)
	 :halt?      (or halt? false)
	 :when       (when->fn when)
	 :events     events
	 :dispatch-n (create-dispatch dispatch dispatch-n rule)})

(defn massage-rules
	""
	[rules]
	(reduce (fn [rules rule]
						(if (= (:when rule) :seen-sequence?)
							(->> (expand-sequence-rule rule)
									 (map-indexed (fn [index expanded-rule]
																	(massage-rule (+ index (count rules))
																								expanded-rule))))
							(conj rules (massage-rule (count rules) rule))))
					[]
					(map normalize-events rules)))

;; -- Event Handler

(def rule-event-ids (comp (map :events) (map first)))

(defn make-flow-event-handler
  "Given a flow definitiion, returns an event handler which implements this definition"
  [{:keys [id db-path rules first-dispatch]}]
  (let [
        ;; Subject to db-path, state is either stored in app-db or in a local atom
        ;; Two pieces of state are maintained:
        ;;  - the set of seen events
        ;;  - the set of started tasks
        _           (assert (or (nil? db-path) (vector? db-path)) "aync-flow: db-path must be a vector")
        local-store (atom {})
        set-state   (if db-path
                      (fn [db seen started]
                        (assoc-in db db-path {:seen-events seen :rules-fired started}))
                      (fn [db seen started]
                        (reset! local-store {:seen-events seen :rules-fired started})
                        db))
        get-state   (if db-path
                      (fn [db] (get-in db db-path))
                      (fn [_] @local-store))

        rules       (massage-rules rules)]       ;; all of the events refered to in the rules

    ;; Return an event handler which will manage the flow.
    ;; This event handler will receive 3 kinds of events:
    ;;   (dispatch [:id :setup])
    ;;   (dispatch [:id :halt-flow])
    ;;   (dispatch [:id [:forwarded :event :vector]])
    ;;
    ;; This event handler returns a map of effects - it expects to be registered using
		;; reg-event-fx
    ;;
    (fn async-flow-event-hander
      [{:keys [db]} event-v]

      (condp = (second event-v)
        ;; Setup this flow coordinator:
        ;;   1. Establish initial state - :seen-events and ::rules-fired are made empty sets
        ;;   2. dispatch the first event, to kick start flow
        ;;   3. arrange for the events to be forwarded to this handler
        :setup {:db             (set-state db {} #{})
                :dispatch       first-dispatch
                :forward-events {:register    id
                                 :events      (->> rules
																									 (mapcat :events)
																									 (map first)
																									 (set))
                                 :dispatch-to [id]}}

        ;; Teardown this flow coordinator:
        ;;   1. remove this event handler
        ;;   2. remove any state stored in app-db
        ;;   3. deregister the events forwarder
        :halt-flow {;; :db (dissoc db db-path)  ;; Aggh. I need dissoc-in to make this work.
                    :forward-events           {:unregister id}
                    :deregister-event-handler id}

        ;; Here we are managing the flow.
        ;; A new event has been forwarded, so work out what should happen:
        ;;  1. does this new event mean we should dispatch another?
        ;;  2. remember this event has happened
        (let [{:keys [seen-events rules-fired]} (get-state db)
              new-seen-events (conj-event seen-events (second event-v))
              ready-rules     (startable-rules rules new-seen-events rules-fired)
						  add-halt?       (some :halt? ready-rules)
							ready-rules-ids (->> ready-rules (map :id) set)
							new-rules-fired (set/union rules-fired ready-rules-ids)
							new-dispatches  (cond-> (mapcat :dispatch-n ready-rules)
																			add-halt? vec
																			add-halt? (conj [id :halt-flow]))]
							(merge {:db (set-state db new-seen-events new-rules-fired)}
										 (when (seq new-dispatches) {:dispatch-n new-dispatches})))))))


(defn- ensure-has-id
	"Ensure `flow` has an id.
	Return a vector of [id flow]"
	[flow]
	(if-let [id (:id flow)]
		[id flow]
		(let [new-id (keyword (str "async-flow/" (gensym "id-")))]
			[new-id (assoc flow :id new-id)])))


;; -- Effect handler


(defn flow->handler
	"Action the given flow effect"
  [flow]
	(let [[id flow']  (ensure-has-id flow)]
		(re-frame/reg-event-fx id (make-flow-event-handler flow'))   ;; register event handler
		(re-frame/dispatch [id :setup])))                            ;; kicks things off

(re-frame/reg-fx
  :async-flow
  flow->handler)


