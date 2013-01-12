(ns info-theory.iv
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
  [N & {:keys [vcov-mat] :or {vcov-mat (i/identity-matrix 3)}}]
  (let [Q (i/decomp-cholesky vcov-mat)
        k (second (i/dim Q))
        m (take k (repeatedly #(s/sample-normal N)))]
    (i/mmult (i/trans (i/bind-columns m)) Q)))

(def true-beta [1 2 -4 1])

