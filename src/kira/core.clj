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
   strict-mode? is false, (l1<=r2) and (r1>=l2)
   strict-mode? is true, (l1<r2) and (r1>l2)"
  [strict-mode? [l1 r1] [l2 r2]]
  (try
    (if strict-mode?
      (and (neg? (compare l1 r2))
           (pos? (compare r1 l2)))
      (not (or (pos? (compare l1 r2))
               (neg? (compare r1 l2)))))
    (catch Exception e
      (println (str "Unexpected error-" (.getMessage ^Exception e)))
      nil)))

(defn overlapped?
  "Make events comparable before comparing"
  [strict-mode? & args]
  (let [coll (->> args
                  (mapcat identity)
                  (map date-time)
                  (take 4))]
    (when-let [[f s] (and (= 4 (count coll))
                          (not-any? #(= :invalid %) coll)
                          (partition 2 coll))]
      (my-compare strict-mode? f s))))

(defn overlap
  "Return overlapped events
   coll is a sequence of [l r]
   strict-mode? overlap strictly or not"
  ([coll]
   (overlap coll false))
  ([coll strict-mode?]
   (loop [f (first coll) r (rest coll) acc []]
     (if (seq r)
       (let [t (->> r
                    (filter (partial overlapped? strict-mode? f))
                    (map (partial list f))
                    (concat acc))]
         (recur (first r) (rest r) t))
       acc))))

(defn -main [& args]
  []
  (let [coll (->> (repeatedly 5 #(rand-int 24))
                  set
                  (partition 2 1)
                  (map sort))]
    (println "INPUT:" coll)
    (println "Overlapped:"(overlap coll))
    (println "Overlapped with strict mode:" (overlap coll true))))
