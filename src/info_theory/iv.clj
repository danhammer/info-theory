(ns info-theory.iv
  (:use info-theory.core)
  (:require [incanter.core :as i]
            [incanter.stats :as s]
            [incanter.charts :as c]))

(defn vcov-fn
  "returns a variance-covariance matrix that corresponds to the
  published data generating process."
  [& {:keys [r13  r23  r2z]
      :or   {r13 0 r23 0.5 r2z 0.5}}]
  (i/matrix [[1   0   r13   0   0]
             [0   1   r23   r2z 0]
             [r13 r23 1     0   0]
             [0   r2z 0     1   0]
             [0   0   0     0   1]]))

(defn rmvn-chol
  "generates an (N x k) data set that is distributed joint normal,
  according to the supplied variance-covariance incanter matrix;
  default is an (N x 3) independently normal data set.

  Example (check variances):
    (let [X (rmvn-chol 1000 :vcov-mat (vcov-fn))]
      (map s/variance (i/trans X)))
    => (1.031 0.978 1.024 0.976 0.971)"
  [N & {:keys [vcov-mat] :or {vcov-mat (vcov-fn)}}]
  (let [Q (i/decomp-cholesky vcov-mat)
        k (second (i/dim Q))
        m (take k (repeatedly #(s/sample-normal N)))]
    (i/mmult (i/trans (i/bind-columns m)) Q)))

(defn prebind-ones
  "accepts an (N x k) matrix, returns a (N x (k + 1)) matrix with the
  first a first column of ones"
  [mat]
  (let [ones (repeat (i/nrow mat) 1)]
    (i/bind-columns ones mat)))

(defn dependent-var
  "accepts a matrix of data with the final column the idiosyncratic
  error, and returns a dependent variable according to the data
  generating process in the associated paper."
  [data & {:keys [true-beta] :or {true-beta [1 2 -4 1]}}]
  (let [total-vec (conj true-beta 1)
        X (prebind-ones (i/sel data :cols [0 1 2 4]))]
    (i/mmult X total-vec)))

(defn iv-res
  "accepts a y vector and X matrix (with no intercept) and returns the
  2SLS model."
  [y X]
  (let [endog (i/sel X :cols 1)
        first (i/sel X :cols [0 3])
        hat (:fitted (s/linear-model endog first))
        new-X (i/bind-columns (i/sel X :cols 0) hat)]
    (s/linear-model y new-X)))

(defn iv-permuation
  "returns the KS statistic in the comparison of the raw y variable
  and the errors from the linear model, either from two-stage least
  squares or just the (biased) linear model.  The supplied arguments
  indicate the number of observations N and the window length for
  permutation entropy."
  [N D & {:keys [two-stage] :or {two-stage true}}]
  (let [X (rmvn-chol N)
        y (dependent-var X)
        e (if (true? two-stage)
            (:residuals (iv-res y X))
            (:residuals (s/linear-model y (i/sel X :cols [0 1]))))]
    (retrieve-diff D y e)))

(defn simulate-diff
  "collect the KS statistics for B iterations, N observations, D
  window length."
  [B N D & {:keys [two-stage] :or {two-stage true}}]
  (let [iter-fn (fn [] (iv-permuation N D :two-stage two-stage))]
    (take B (repeatedly #(iter-fn)))))
