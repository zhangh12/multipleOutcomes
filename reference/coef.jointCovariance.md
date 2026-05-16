# Extract Model Coefficients

`coef` is a generic function.

## Usage

``` r
# S3 method for class 'jointCovariance'
coef(object, model_index = NULL, ...)
```

## Arguments

- object:

  an object returned by
  [`jointCovariance()`](https://zhangh12.github.io/multipleOutcomes/reference/jointCovariance.md).

- model_index:

  `NULL` if displaying coefficients of all fitted models; otherwise, an
  integer indicating the fitted model.

- ...:

  for debugging only

## Value

a vector of coefficient estimates
