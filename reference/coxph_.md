# Creating Objects of Proportional Hazards Regression Model

`coxph_` is a wrapper function of
[`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html) to
create an object to be passed into `jointCovariance`, the main function
of this package through its argument `...`. The object defines how a
proportional hazard model would be fitted.

## Usage

``` r
coxph_(formula, data_index = 1)
```

## Arguments

- formula:

  see `formula` in
  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html).

- data_index:

  integer. Index of the data frame in the `data` argument of
  `jointCovariance` to be used when fitting a proportional hazards
  model.

## Details

Not all arguments of
[`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html) are
supported in `coxph_` due to the complexity in handling environment and
scope, which is particularly difficult for arguments like `weights`,
`subset`, etc.
