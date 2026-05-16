# Creating Objects of Generalized Linear Models

`glm_` is a wrapper function of
[`stats::glm`](https://rdrr.io/r/stats/glm.html) to create an object to
be passed into `jointCovariance`, the main function of this package
through its argument `...`. The object defines how a GLM model would be
fitted.

## Usage

``` r
glm_(formula, family, data_index = 1)
```

## Arguments

- formula:

  see `formula` in [`stats::glm`](https://rdrr.io/r/stats/glm.html).

- family:

  currently supports `"gaussian"` or `"binomial"`. Other families are
  under testing.

- data_index:

  integer. Index of the data frame in the `data` argument of
  `jointCovariance` to be used when fitting a generalized linear model.

## Details

Not all arguments of [`stats::glm`](https://rdrr.io/r/stats/glm.html)
are supported in `glm_` due to the complexity in handling environment
and scope, which is particularly difficult for arguments like `weights`,
`subset`, etc.
