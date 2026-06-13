# Creating Objects of Hierarchical Log Win-Ratio Statistic

`winratio_` creates an object to be passed into `jointCovariance` or
`pated` through its `...` argument. The object defines a hierarchical
win-ratio statistic across a list of endpoints, each declared by
[`nb_tte()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md),
[`nb_continuous()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md),
or
[`nb_binary()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md).

## Usage

``` r
winratio_(formula, endpoints, data_index = 1)
```

## Arguments

- formula:

  a two-sided R formula `<label> ~ arm`. The LHS is used purely as a row
  label in
  [`pated()`](https://zhangh12.github.io/multipleOutcomes/reference/pated.md)
  output; the RHS must contain exactly one variable, the column in
  `data` holding treatment-arm assignment.

- endpoints:

  a non-empty list of endpoint specs built by
  [`nb_tte()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md),
  [`nb_continuous()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md),
  or
  [`nb_binary()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md).
  The order of the list encodes the hierarchical priority: endpoint 1 is
  checked first; if it ties, the pair falls through to endpoint 2; and
  so on.

- data_index:

  integer. Index of the data frame in the `data` argument of
  `jointCovariance` to be used.

## Value

An object of class `c("jc_spec_winratio", "jc_spec")`.

## Details

The fitted coefficient is on the log scale: \$\$\widehat\theta =
\log(\widehat{WR}) = \log(\widehat\pi_W / \widehat\pi_L)\$\$ where
\\\widehat\pi_W\\ and \\\widehat\pi_L\\ are the proportions of all
control-treatment pairs where the treatment subject wins or loses,
respectively. Tied pairs remain in the denominator of both proportions
but do not contribute to either numerator.

The raw win ratio can be recovered by exponentiating the coefficient.
The log scale is used because it gives the standard large-sample Wald
representation and maps the null win ratio of 1 to 0.

The arm reference level is inferred from the arm column the same way
`model.matrix(~ arm)` would: `levels(arm)[1]` for factor, the smaller
value for numeric or logical, and the alphabetically first value for
character. To override, convert the column to a factor with the desired
level order before calling `winratio_()`.

## See also

[`nb_tte()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md),
[`nb_continuous()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md),
[`nb_binary()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md),
[`jointCovariance()`](https://zhangh12.github.io/multipleOutcomes/reference/jointCovariance.md),
[`pated()`](https://zhangh12.github.io/multipleOutcomes/reference/pated.md).
