# info-theory

The purpose of this project is to build up a library to illustrate
ideas from conversations with [George Judge](http://goo.gl/BDX2J).

## Examining entropy from a de-meaned series

The first application is to compare the distribution of the dynamic
sequences between a raw time series and the demeaned time series.  We
generate the time series using the following function:

```clojure
(defn dgp [T]
  (let [e (s/sample-normal T)]
    (map (partial + 5) e)))
```

This will return a series of length `T`, mean 5, and error distributed
standard normal.  We then apply the `permutation-count` function to
return a hash-map of the permutation sequences and their frequency.
We do the same for the de-meaned version of the series.  The following
image displays the count histogram for each of the sequences for a
series of length T=400, of which there are 24 for D = 4.  The
sequences are arbitrarily ordered.

![](https://dl.dropbox.com/u/5365589/permutation-entropy.png)

The function to generate the time series and test for differences in
the empirical distribution functions of the permutation counts is
below.

```clojure
(defn demean-test
  "Returns the K-S test statistic associated with the comparison of
  the permutation entropy distributions associated with a time series
  of length T and the supplied D length."
  [D T]
  (let [y (dgp T)
        m-ref (permutation-count D y)
        m-emp (permutation-count D (demean y))]
    (apply ks-stat
           (map empirical-dist (key-counts m-emp m-ref)))))
```

For this application, the Kolomogorov-Smirnov test statistic is
_always_ 0, since the demeaning only shifts the time series, and does
not change the sequencing of relative values.  This can be seen in the
following line graphs.

![](https://dl.dropbox.com/u/5365589/lines.png)

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
