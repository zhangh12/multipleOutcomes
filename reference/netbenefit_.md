# Creating Objects of Hierarchical Net-Benefit (Win-Difference) Statistic

`netbenefit_` creates an object to be passed into `jointCovariance` or
`pated` through its `...` argument. The object defines a hierarchical
net-benefit (a.k.a. win-difference, proportion-in-favor) statistic
across a list of endpoints, each declared by
[`nb_tte()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md),
[`nb_continuous()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md),
or
[`nb_binary()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md).

## Usage

``` r
netbenefit_(formula, endpoints, data_index = 1)
```

## Arguments

- formula:

  a two-sided R formula `<label> ~ arm`. The LHS is used purely as a row
  label in
  [`pated()`](https://zhangh12.github.io/multipleOutcomes/reference/pated.md)
  output; the first RHS variable is the name of the column in `data`
  holding treatment-arm assignment.

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

An object of class `c("jc_spec_netbenefit", "jc_spec")`.

## Details

The estimator is \$\$\widehat\Delta = \frac{N_W - N_L}{N_W + N_L +
N_T}\$\$ where \\N_W\\, \\N_L\\, and \\N_T\\ are the numbers of
treatment wins, losses, and overall ties across all \\n_C \times n_T\\
pairs (control vs. treatment subject). The per-subject influence
function is available in closed form, so both the asymptotic and the
bootstrap paths of `jointCovariance` are supported.

The arm reference level is inferred from the arm column the same way
`model.matrix(~ arm)` would: `levels(arm)[1]` for factor, the smaller
value for numeric or logical, and the alphabetically first value for
character. To override, convert the column to a factor with the desired
level order before calling `netbenefit_()`.

## See also

[`nb_tte()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md),
[`nb_continuous()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md),
[`nb_binary()`](https://zhangh12.github.io/multipleOutcomes/reference/nb_endpoint.md),
[`jointCovariance()`](https://zhangh12.github.io/multipleOutcomes/reference/jointCovariance.md),
[`pated()`](https://zhangh12.github.io/multipleOutcomes/reference/pated.md).
