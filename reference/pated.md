# Prognostic Variables Assisted Treatment Effect Detection

`pated` is a wrapper function of `jointCovariance` for testing treatment
effect in randomized clinical trials. It assumes that prognostic
variables are fully randomized. This assumption can help enhancing
statistical power of conventional approaches in detecting the treatment
effect. Specifically, the sensitivity of the conventional models
specified in `...` are improved by `pated`.

## Usage

``` r
pated(
  ...,
  data,
  nboot = 0,
  compute_cov = TRUE,
  seed = NULL,
  transform = "identity"
)
```

## Arguments

- ...:

  model specifications built by
  [`glm_()`](https://zhangh12.github.io/multipleOutcomes/reference/glm_.md),
  [`coxph_()`](https://zhangh12.github.io/multipleOutcomes/reference/coxph_.md),
  [`logrank_()`](https://zhangh12.github.io/multipleOutcomes/reference/logrank_.md),
  [`gee_()`](https://zhangh12.github.io/multipleOutcomes/reference/gee_.md),
  [`mmrm_()`](https://zhangh12.github.io/multipleOutcomes/reference/mmrm_.md),
  [`km_()`](https://zhangh12.github.io/multipleOutcomes/reference/km_.md),
  or
  [`quantile_()`](https://zhangh12.github.io/multipleOutcomes/reference/quantile_.md).
  The first specification is the primary outcome whose treatment effect
  is being tested; the rest are prognostic covariates used to tighten
  the SE.

- data:

  either a single data frame (when all models are fitted on the same
  dataset) or a list of data frames (one entry per `data_index`). Each
  data frame must have a `pid` column carrying subject identifiers;
  records with the same `pid` across different data frames refer to the
  same subject.

- nboot:

  non-zero integer if bootstrap is adopted. By default 0.

- compute_cov:

  logic. If `TRUE`, empirical covariance matrix is computed using
  bootstrap estimate and returned. Bootstrap estimate will be abandoned.
  If `FALSE`, bootstrap estimate will be returned and no empirical
  covariance matrix is computed.

- seed:

  random seed when generate bootstrap data.

- transform:

  character. Now only supports `"identity"`.

## Value

a data frame of testing results.

## Examples

``` r
## More examples can be found in the vignettes.
library(survival)
library(mvtnorm)

genData <- function(seed = NULL){
  set.seed(seed)
  n <- 300
  sigma <- matrix(c(1, .6, .6, 1), 2)
  x <- rmvnorm(n, sigma = sigma)
  z1 <- rbinom(n, 1, .6)
  z2 <- rnorm(n)
  trt <- rbinom(n, 1, .5)

  bet <- c(-.2, .2)
  y <- -.5 + x %*% bet + z1 * .3 - z2 * .1 + .1 * trt - .1 * rnorm(n)
  death <- rbinom(n, 1, .8)
  data.frame(
    y = as.numeric(y), trt = trt,
    z1 = z1, z2 = z2,
    x1 = x[, 1], x2 = x[, 2],
    death, pid = paste0('s-', seq_len(n))
  )
}

dat <- genData(seed = 31415926)

## `data_index` defaults to 1 in every spec constructor and a single
## data.frame is auto-wrapped into a list, so neither needs spelling out
## when all models are fitted on the same dataset.
fit <-
  pated(
    coxph_(Surv(time = y, event = death) ~ trt),
    glm_(z1 ~ trt, family = 'binomial'),
    glm_(z2 ~ trt, family = 'gaussian'),
    glm_(x1 ~ trt, family = 'gaussian'),
    glm_(x2 ~ trt, family = 'gaussian'),
    data = dat
  )

fit
#>                            term family     estimate     stderr       pvalue
#> 1 Surv(time = y, event = death)  PATED -0.288684461 0.08056904 0.0003395805
#> 2 Surv(time = y, event = death)  coxph -0.201743178 0.13062378 0.1224769862
#> 3                            z1    glm -0.269021294 0.23351160 0.2492930398
#> 4                            z2    glm  0.110597580 0.11184325 0.3227304829
#> 5                            x2    glm  0.039205611 0.11165593 0.7254917405
#> 6                            x1    glm  0.007879826 0.11228370 0.9440521271
#>       method       corr
#> 1      PATED         NA
#> 2   Standard  1.0000000
#> 3 Prognostic -0.4589612
#> 4 Prognostic  0.4075322
#> 5 Prognostic -0.3087809
#> 6 Prognostic  0.2148699
```
