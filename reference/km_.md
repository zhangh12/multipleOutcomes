# Creating Objects of Kaplan-Meier Curve

`km_` is a wrapper function creating an object of Kaplan-Meier curve to
be passed into `jointCovariance`, the main function of this package
through its argument `...`. The object defines how a Kaplan-Meier curve
would be fitted.

## Usage

``` r
km_(formula, times = NULL, conf_type, data_index = 1)
```

## Arguments

- formula:

  a formula created by
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html).

- times:

  numeric vector of time. Survival probabilities at `times` are computed
  (with g-transformation defined by `conf_type`).

- conf_type:

  character. Type of confidence interval. It must be one of `"log"`,
  `"log-log"`, `"plain"`, `"logit"`, or `"arcsin"`.

- data_index:

  integer. Index of the data frame in the `data` argument of
  `jointCovariance` to be used when fitting a generalized linear model.

## Details

Usually, g-transformation is applied to the survival probability `S(t)`
to obtain pointwise confidence interval of a Kaplan-Meier curve. This
can be achieved by specifying `conf_type`. For identity transformation,
use `conf_type = "plain"`.

This function can only be used with `jointCovariance` when the bootstrap
method is used to estimate variance-covariance matrix of multiple
outcome models.
