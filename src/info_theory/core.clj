(ns info-theory.core
  (:use [midje sweet])
  (:require [incanter.core :as i]
            [incanter.charts :as c]
            [incanter.stats :as s]        
            [incanter.optimize :as optimize]))

(defn- ordinal-idx
  "Returns a sequence of indices that rank the values of the supplied
  time series in ascending order.  If there are equal values, the
  lexicographic ordering kicks in and the order at which the values
  appear is used to order the indices.

  Example:
    (take 5 (repeatedly #(rand-int 5))) => (0 0 4 1 3)
    (ordinal-idx '(0 0 4 1 3)) => (0 1 3 4 2)"
  [sub-ts]
  (let [indexed-vector (map-indexed vector sub-ts)]
    (map first
         (sort-by second indexed-vector))))

(defn- permutation-count
  "Returns a map of the ordinal sequences of length `D` and their
  count; note that the offset is fixed at 1"
  [D ts]
  (let [subs (partition D 1 ts)]
    (frequencies (map ordinal-idx subs))))

(defn- dgp
  [T]
  (let [e (s/sample-normal T)]
    (map (partial + 5) e)))

(defn demean [coll]
  (let [mu (s/mean coll)]
    (map #(- % mu) coll)))

(defn key-counts
  [m1 m2]
  (apply map vector
         (map val (merge-with vector m1 m2))))

(defn empirical-dist
  [coll]
  (let [n (float (reduce + coll))]
    (map #(/ % n)
         (reductions + coll))))

(defn ks-stat
  [emp-dist ref-dist]
  (reduce max (map - emp-dist ref-dist)))

(defn demean-test
  [D T]
  (let [y (dgp T)
        m-ref (permutation-count D y)
        m-emp (permutation-count D (demean y))]
    (apply ks-stat
           (map empirical-dist (key-counts m-emp m-ref)))))

(defn- log-fn [x]
  (* x (i/log2 x)))

(defn- to-freq
  "Returns the normalized frequency of the supplied column"
  [coll]
  (let [total (reduce + coll)]
    (map #(/ % total) coll)))

(defn permutation-entropy
  "Normalixed permutation entropy based on the Shannon entropy
  distribution"
  [D ts]
  (let [pi-seq (to-freq (vals (permutation-count D ts)))
        scale-by (* -1 (/ 1 (i/log2 (i/factorial D))))]
    (* scale-by
       (reduce + (map log-fn pi-seq)))))

(defn kl-entropy
  "Returns the normalized Kullback-Leibler entropy (KLE) information
  measure, which quantifies the distance between the orginal pattern
  probability distribution of `ts` and the uniform distribution"
  [D ts]
  (- 1 (permutation-entropy D ts)))

