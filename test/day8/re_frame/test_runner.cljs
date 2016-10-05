(ns day8.re-frame.async-flow-fx.test-runner
  (:require
    [cljs.test :as cljs-test :include-macros true]
    [jx.reporter.karma :as karma :include-macros true]
    ;; Test Namespaces -------------------------------
    [day8.re-frame.async-flow-fx.async-flow-fx-test]
		[day8.re-frame.async-flow-fx.matcher-test]
		[day8.re-frame.async-flow-fx.event-cache-test]
		[day8.re-frame.async-flow-fx.machine-test]
		[day8.re-frame.async-flow-fx.rule-test])
  (:refer-clojure :exclude (set-print-fn!)))

(enable-console-print!)

;; ---- BROWSER based tests ----------------------------------------------------
(defn ^:export set-print-fn! [f]
  (set! cljs.core.*print-fn* f))


(defn ^:export run-html-tests []
  (cljs-test/run-tests
    'day8.re-frame.async-flow-fx.async-flow-fx-test
		'day8.re-frame.async-flow-fx.matcher-test
		'day8.re-frame.async-flow-fx.event-cache-test
		'day8.re-frame.async-flow-fx.machine-test
		'day8.re-frame.async-flow-fx.rule-test))

;; ---- KARMA  -----------------------------------------------------------------

(defn ^:export run-karma [karma]
  (karma/run-tests
    karma
    'day8.re-frame.async-flow-fx.async-flow-fx-test))
