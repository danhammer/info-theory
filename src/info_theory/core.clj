(ns info-theory.core
  (:require [incanter.core :as i]
            [incanter.stats :as s]
            [incanter.charts :as c]))

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

(defn permutation-count
  "Returns a map of the ordinal sequences of length `D` and their
  count; note that the offset is fixed at 1"
  [D ts]
  (let [subs (partition D 1 ts)]
    (frequencies (map ordinal-idx subs))))

(defn mean-dgp
  "Simple data generating process for a random series-like variable
  with length T, mean 5, and error distributed standard normal."
  [T]
  (let [e (s/sample-normal T)]
    (map (partial + 5) e)))

(defn linear-dgp [T x]
  (let [e (s/sample-normal T)]
    (map (partial + 5) x e)))

(defn demean
  "returns a collection with the mean value subtracted"
  [coll]
  (let [mu (s/mean coll)]
    (map #(- % mu) coll)))

(defn key-counts
  "returns two vectors of values, appropriately aligned, from the
  supplied hash maps.  This allows for non-standard keys, like
  sequences."
  [m1 m2]
  (apply map vector
         (map val (merge-with vector m1 m2))))

(defn empirical-dist
  "returns the results from applying the empirical distribution
  function to a pre-sorted collection"
  [coll]
  (let [n (float (reduce + coll))]
    (map #(/ % n)
         (reductions + coll))))

(defn ks-stat
  "returns the Kolmogorov–Smirnov test statistics from two properly
  aligned distributions of identical length."
  [emp-dist ref-dist]
  (reduce max
          (map (comp i/abs -) emp-dist ref-dist)))

(defn retrieve-diff
  "accepts the length of the permutation series, and the errors from
  the reference and new series; returns the the K-S test statistic
  associated with the comparison of the permutation entropy
  distributions associated with a time series of length T and the
  supplied D length."
  [D e-ref e-new]
  {:pre [= (count e-ref) (count e-new)]}
  (let [m-ref (permutation-count D e-ref)
        m-new (permutation-count D e-new)]
    (apply ks-stat
           (map empirical-dist (key-counts m-new m-ref)))))

(defn demean-illustration
  "compares the residuals from a series and the demeaned series"
  [D T]
  (let [y (mean-dgp T)]
    (retrieve-diff D y (demean y))))

(defn linear-residuals
  "returns an incanter vector of residuals from a linear model; cribbed from
  incanter.stats linear model."
  [y x & {:keys [intercept] :or {intercept true}}]
  (let [_x (if intercept (i/bind-columns (replicate (i/nrow x) 1) x) x)
        xt (i/trans _x)
        xtx (i/mmult xt _x)
        coefs (i/mmult (i/solve xtx) xt y)]
    (i/minus y (i/mmult _x coefs))))

(defn linear-illustration
  "compares the linear DGP with the residuals from a linear model"
  [D T]
  (let [x (s/sample-normal T :mean 3)
        y (linear-dgp T x)]
    (retrieve-diff D y (linear-residuals y x))))

(defn hist-diffstat
  "returns a histogram of the ks-stat for a MC-simulation, iterated B
  times."
  [B f D T]
  (let [dgp-fn (fn [x] (f D T))]
    (c/histogram (pmap dgp-fn (range B))
                 :nbins 20)))

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

