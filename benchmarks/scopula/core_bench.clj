(ns scopula.core-bench
  (:require [scopula.core :as scopula]
            [clj-memory-meter.core :as mm]
            [clj-memory-meter.trace :as mmtrace]
            [clojure.set :as set]
            [criterium.core :as c]))

(def scope-1 "admin/integration/module-instance:read")
(def scope-2 "admin/integration/module-instance")

(def scope-repr-1 (scopula/to-scope-repr scope-1))
(def scope-repr-2 (scopula/to-scope-repr scope-2))

(def lst1 (:path scope-repr-1))
(def lst2 (:path scope-repr-2))

(defn is-sub-list-2 [lst super-lst]
  (loop [l1 (seq lst)
         l2 (seq super-lst)]
    (cond
      (nil? l1) true
      (nil? l2) false
      (= (first l1) (first l2)) (recur (next l1) (next l2))
      :else false)))

(def lists
  [["admin"]
   ["admin" "integration" "module-instance"]
   ["iroh-int"]
   ["iroh-int" "module-instance"]])

(defn t [f]
  (f lst1 lst2))


(defn old-is-sub-list?
  "Does `super-lst` begin with `lst`?"
  [lst super-lst]
  (let [n (count lst)]
    (= (take n super-lst) lst)))


(defn check-is-sub-list []
  (println "without loop")
    (c/quick-bench (t old-is-sub-list?) :gc-summary true)

  (println "with loop")
  (c/quick-bench (t is-sub-list-2) :gc-summary true))

(defn check-ram-diff []
  (mmtrace/trace-var #'t)
  (println "Old Version: ")
  (mmtrace/trace-var #'old-is-sub-list?)
  (mmtrace/with-relative-usage
    (t old-is-sub-list?))

  (println "Current Branch Version: ")
  (mmtrace/trace-var #'scopula/is-sub-list?)
  (mmtrace/with-relative-usage
    (t scopula/is-sub-list?) ))


(def accesses [#{}
               #{:read}
               #{:write}
               #{:read :write}])

(defn check-access []
  (println "with superset?")
  (c/quick-bench
      (for [ac1 accesses
            ac2 accesses]
        (set/superset? ac1 ac2))
    :gc-summary true)

  (println "with every?")
  (c/quick-bench
      (for [ac1 accesses
            ac2 accesses]
        (every? ac1 ac2))
    :gc-summary true))
