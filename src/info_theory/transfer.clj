(ns info-theory.transfer
  (:use [clojure.contrib.generic.functor :only (fmap)]
        info-theory.core)
  (:require [incanter.core :as i]
            [incanter.stats :as s]
            [incanter.charts :as c]))

(defn norm-map
  "accepts a hash map, returns a map where each value has been
  normalized."
  [m]
  (let [N (reduce + (vals m))]
    (fmap #(float (/ % N)) m)))

(defn prop-vec
  "accepts a variable number of sequences and returns the proportion
  of sub-vectors as a hash map, where the sub-vectors are determined
  based on the indices in the supplied vector"
  [& args]
  (->> (apply map vector (vec args))
       frequencies
       norm-map))

(defn info-calc
  "accepts four maps and a vector of length three.  the first map is
  the total, three-way joint probability; the second is the two-way
  joint probability between the two original sequencies; the third is
  the joint probability of an element and the subsequent element in
  the target sequence; and the fourth is the relative frequency of
  elements in the target sequence.  returns the additional information
  required to represent the next value in the target sequence."
  [a b c d triplet]
  (let [[xin xi yi] triplet
        total-joint (get a [xin xi yi])
        joint-prob  (get b [xi yi])
        future-prob (get c [xin xi])
        total-prob  (get d [xi])]
    (* total-joint
       (i/log10
        (/ (* total-joint total-prob)
           (* joint-prob future-prob))))))

(defn info-map
  "returns the aggregate information transfer from y to x over the
  course of the target sequence x"
  [x y]
  (let [a (prop-vec (rest x) x y)
        b (prop-vec x y)
        c (prop-vec (rest x) x)
        d (prop-vec x)]
    (reduce +
            (map (partial info-calc a b c d)
                 (apply map vector [(rest x) x y])))))




