# Calculate Variance-Covariance Matrix for a Fitted Model Object

Returns the variance-covariance matrix of the main parameters of fitted
model objects. The "main" parameters of models correspond to those
returned by `coef`.

## Usage

``` r
# S3 method for class 'jointCovariance'
vcov(object, model_index = NULL, ...)
```

## Arguments

- object:

  an object returned by
  [`jointCovariance()`](https://zhangh12.github.io/multipleOutcomes/reference/jointCovariance.md).

- model_index:

  `NULL` if displaying covariance matrix of all fitted models;
  otherwise, an integer indicating the fitted model.

- ...:

  for debugging only

## Value

a matrix of covariance of all estimates
