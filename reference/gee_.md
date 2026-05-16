# Creating Objects of Generalized Estimation Equation Model

`gee_` is a wrapper function of `gee::gee` to create an object to be
passed into `jointCovariance`, the main function of this package through
its argument `...`. The object defines how a GEE model would be fitted.

This package does not import the package `gee`. Instead, codes of `gee`
are modified and integrated to compute score and information matrix.
Thus, users does not need to install the package `gee` to use this
package.

## Usage

``` r
gee_(formula, family, corstr, R = NULL, b = NULL, Mv = 1, data_index = 1)
```

## Arguments

- formula:

  see `formula` in `gee::gee`.

- family:

  see `family` in `gee::gee`.

- corstr:

  see `corstr` in `gee::gee`.

- R:

  see `R` in `gee::gee`.

- b:

  see `b` in `gee::gee`.

- Mv:

  see `Mv` in `gee::gee`.

- data_index:

  integer. Index of the data frame in the `data` argument of
  `jointCovariance` to be used when fitting a GEE model.

## Details

Not all arguments of `stats::gee` are supported in `gee_` due to the
complexity in handling environment and scope, which is particularly
difficult for arguments like `subset`, etc.
