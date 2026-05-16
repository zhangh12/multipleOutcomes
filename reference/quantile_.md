# Creating Objects of Group Quantile Differences

`quantile_` is a wrapper function creating an object that, for each
requested probability, computes the difference between the two arms'
sample quantiles of the outcome. The object is passed to
`jointCovariance` or `pated` through `...`. Because the
empirical-quantile estimator has no tractable closed-form score, this
engine is available only through the bootstrap path (`nboot > 0`).

## Usage

``` r
quantile_(formula, probs = c(0.25, 0.5, 0.75), data_index = 1)
```

## Arguments

- formula:

  a two-sided formula `y ~ arm` where the right-hand side is a binary
  grouping variable.

- probs:

  numeric vector of probabilities in `(0, 1)`. By default the first
  quartile, median, and third quartile.

- data_index:

  integer. Index of the data frame in the `data` argument of
  `jointCovariance` to be used.
