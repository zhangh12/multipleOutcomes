# Creating Objects of Logrank Test

`logrank_` is a wrapper function
[`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html) to
create an object to be passed into `jointCovariance`, the main function
of this package through its argument `...`. Logrank test is the score
test under the proportional hazards regression model. The object defines
how a logrank test would be computed.

## Usage

``` r
logrank_(formula, ties = c("efron", "breslow", "exact"), data_index = 1)
```

## Arguments

- formula:

  see `formula` in
  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html).

- ties:

  character string specifying the method for tie handling. One of
  `"efron"` (default), `"breslow"`, or `"exact"`. Passed through to
  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html).

- data_index:

  integer. Index of the data frame in the `data` argument of
  `jointCovariance` to be used when computing testing statistic of
  logrank test.

## Details

Not all arguments of
[`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html) are
supported in `logrank_` due to the complexity in handling environment
and scope, which is particularly difficult for arguments like `weights`,
`subset`, etc.
