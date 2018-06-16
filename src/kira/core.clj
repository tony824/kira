(ns kira.core
  (:require [clj-time.coerce :as ct]
            [clj-time.core :as t])
  (:import (org.joda.time DateTime)))

(defn date-time
  [x]
  (try
    (cond
      (instance? DateTime x) x
      (integer? x) (or (ct/from-long (long x)) :invalid)
      (inst? x)    (or (ct/from-date x) :invalid)
      (string? x)  (or (ct/from-string x) :invalid)
      :else :invalid)
    (catch Exception e
      :invalid)))

(defn my-compare
  "Determines whether 2 events [l1 r1] [l2 r2] overlap or not
   strict is false (l1<=r2) and (r1>=l2)
   strict is true (l1<r2) and (r1>l2)"
  [strict-mode? [l1 r1] [l2 r2]]
  (try
    (if strict-mode?
      (and (neg? (compare l1 r2))
           (pos? (compare r1 l2)))
      (and (not= (compare l1 r2) 1)
           (not= (compare r1 l2) -1)))
    (catch Exception e
      (println (str "Unexpected error-" (.getMessage ^Exception e)))
      nil)))

(defn overlapped?
  "Make events comparable"
  [strict-mode? & args]
  (let [[l1 r1 l2 r2 :as coll] (map date-time (mapcat identity args))]
    (when (not-any? #(= :invalid %) coll)
      (my-compare strict-mode? [l1 r1] [l2 r2]))))

(defn overlap
  "Return overlapped events
   coll is a sequence of [l r]
   strict-mode? overlap strictly or not"
  [coll strict-mode?]
  (loop [f (first coll) r (rest coll) acc []]
    (if (seq r)
      (let [t (->> r
                   (filter (partial overlapped? strict-mode? f))
                   (map (partial list f))
                   (concat acc))]
        (recur (first r) (rest r) t))
      acc)))

(defn -main [& args]
  []
  (overlap '([1 2] [2 3] [3 4] [2 5] [2 8]) true)
  (overlap '([1 2] [2 3] [3 4] [2 5] [2 8]) false))
