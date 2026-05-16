# Fitting Regression Models for Multiple Outcomes and Returning the Matrix of Covariance

`jointCovariance` can fit different types of models for multiple
outcomes simultaneously and return model parameters and
variance-covariance matrix for further analysis.

## Usage

``` r
jointCovariance(..., data, nboot = 0, compute_cov = TRUE, seed = NULL)
```

## Arguments

- ...:

  objects returned by
  [`glm_()`](https://zhangh12.github.io/multipleOutcomes/reference/glm_.md),
  [`coxph_()`](https://zhangh12.github.io/multipleOutcomes/reference/coxph_.md),
  [`logrank_()`](https://zhangh12.github.io/multipleOutcomes/reference/logrank_.md),
  [`gee_()`](https://zhangh12.github.io/multipleOutcomes/reference/gee_.md)
  and `gmm_()`.

- data:

  a data frame if all models are fitted on the same dataset; otherwise a
  list of data frames for fitting models in `...`. Note that a dataset
  can be used to fit multiple models, thus, `length(data)` is
  unnecessary to be equal to the number of models in `...`. The row
  names in a data frame are not treated as subject IDs. Instead, all
  data frame should consist of a column `pid` as subject IDs. For any
  two records in different data frames that correspond to the same
  subject, their values in `pid` should be consistent. For data frames
  to be used by GEE, `pid` defines clusters. See `id` and the Details
  section of `gee:gee`.

- nboot:

  non-zero integer if bootstrap is adopted. By default 0.

- compute_cov:

  logic. If `TRUE` and `nboot > 0`, empirical covariance matrix is
  computed using bootstrap estimate and returned. Bootstrap estimate
  will be abandoned. If `FALSE`, bootstrap estimate will be returned and
  no empirical covariance matrix is computed.

- seed:

  random seed when generate bootstrap data.

## Value

It returns an object of class "jointCovariance", which is a list
containing the following components:

|  |  |
|----|----|
| `coefficients` | an unnamed vector of coefficients of all fitted models. Use `id_map` for variable mapping. |
|  |  |
| `mcov` | a unnamed matrix of covariance of `coefficients`. Use `id_map` for variable mapping. |
|  |  |
| `id_map` | a list mapping the elements in `coefficients` and `mcov` to variable names. |
|  |  |
| `n_shared_sample_sizes` | a matrix of shared sample sizes between datasets being used to fit the models. |
|  |  |
| `call` | the matched call. |

## Examples

``` r
## More examples can be found in the vignettes.
library(survival)
library(mvtnorm)
library(tidyr)
genData <- function(seed = NULL){
  
  set.seed(seed)
  n <- 300
  sigma <- matrix(.7, 4, 4)
  diag(sigma) <- 1
  v <- rmvnorm(n, sigma = sigma)
  x1 <- v[, 1]
  x2 <- v[, 2]
  z1 <- (v[, 3] > 0) + 0
  z2 <- v[, 4]
  
  trt <- rbinom(n, 1, .5)
  
  bet <- c(-.3,.3)
  y <- -log(runif(n))/
    exp(-.3 * x1 + .3 * x2 + z1 * .5 - z2 * .3 + .1 * trt + rnorm(n))
  
  z1[sample.int(n, 50)] <- NA
  z2[sample.int(n, 50)] <- NA
  x1[sample.int(n, 50)] <- NA
  x2[sample.int(n, 50)] <- NA
  death <- ifelse(y > 2, 0, 1)
  y[y > 2] <- 2
  
  pid <- paste0('pid-', 1:n)
  ret <- data.frame(
    y = y, trt = trt, 
    z1 = z1, z2 = z2, 
    x1 = x1, x2 = x2, 
    death, pid)
  ret
}

dat1 <- genData()

## create a dataset with repeated measurements x
dat2 <- dat1 %>% pivot_longer(c(x1, x2), names_to='visit', values_to='x') %>% 
  dplyr::select(x, trt, visit, pid) %>% as.data.frame()

dat2$visit <- as.factor(dat2$visit)
dat2$pid <- as.factor(dat2$pid)

fit <- jointCovariance(
  coxph_(Surv(time = y, event = death) ~ trt, data_index = 1),
  logrank_(Surv(time = y, event=death) ~ trt, data_index = 1),
  glm_(z1 ~ trt, family = 'binomial', data_index = 1), 
  glm_(z2 ~ trt, family = 'gaussian', data_index = 1), 
  mmrm_(x ~ trt + us(visit | pid), reml = TRUE, data_index = 2), 
  gee_(x ~ trt, family = 'gaussian', corstr = 'independence', data_index = 2), 
  data = list(dat1, dat2))
#> mmrm() registered as car::Anova extension
#> method in mmrm_control is re-set to be "Residual"
#> vcov in mmrm_control is re-set to be "Empirical"

fit
#> $coefficients
#>  [1]  0.08039782  0.62944355 -0.08576682  0.12213447  0.01835858 -0.13334461
#>  [7] -0.10139381  0.07958077 -0.11501843  0.09952479
#> 
#> $mcov
#>                [,1]          [,2]          [,3]          [,4]          [,5]
#>  [1,]  1.628603e-02  0.1271629079 -0.0007228741 -0.0008734002 -0.0001894825
#>  [2,]  1.271629e-01  0.9932305284 -0.0055457811 -0.0069150434 -0.0014489303
#>  [3,] -7.228741e-04 -0.0055457811  0.0286240033 -0.0286240033  0.0068056583
#>  [4,] -8.734002e-04 -0.0069150434 -0.0286240033  0.0649996646 -0.0068065664
#>  [5,] -1.894825e-04 -0.0014489303  0.0068056583 -0.0068065664  0.0073034875
#>  [6,] -9.986200e-04 -0.0078196721 -0.0068075499  0.0145952533 -0.0073034875
#>  [7,]  2.033393e-05  0.0001841844  0.0060216482 -0.0060231700  0.0037944215
#>  [8,] -1.512654e-03 -0.0119679927 -0.0060245885  0.0144993273 -0.0037952913
#>  [9,]  2.191215e-04  0.0017086509  0.0062594111 -0.0062624769  0.0038670068
#> [10,] -1.780559e-03 -0.0140345175 -0.0062626590  0.0146283417 -0.0038678634
#>               [,6]          [,7]         [,8]          [,9]        [,10]
#>  [1,] -0.000998620  2.033393e-05 -0.001512654  0.0002191215 -0.001780559
#>  [2,] -0.007819672  1.841844e-04 -0.011967993  0.0017086509 -0.014034517
#>  [3,] -0.006807550  6.021648e-03 -0.006024589  0.0062594111 -0.006262659
#>  [4,]  0.014595253 -6.023170e-03  0.014499327 -0.0062624769  0.014628342
#>  [5,] -0.007303487  3.794422e-03 -0.003795291  0.0038670068 -0.003867863
#>  [6,]  0.015526405 -3.793520e-03  0.008600338 -0.0038659095  0.008643580
#>  [7,] -0.003793520  4.973808e-03 -0.004973808  0.0049875186 -0.004987519
#>  [8,]  0.008600338 -4.973808e-03  0.010737295 -0.0049875186  0.010653161
#>  [9,] -0.003865910  4.987519e-03 -0.004987519  0.0052224187 -0.005222419
#> [10,]  0.008643580 -4.987519e-03  0.010653161 -0.0052224187  0.011025692
#> 
#> $id_map
#> $id_map[[1]]
#> trt 
#>   1 
#> 
#> $id_map[[2]]
#> trt 
#>   2 
#> 
#> $id_map[[3]]
#> (Intercept)         trt 
#>           3           4 
#> 
#> $id_map[[4]]
#> (Intercept)         trt 
#>           5           6 
#> 
#> $id_map[[5]]
#> (Intercept)         trt 
#>           7           8 
#> 
#> $id_map[[6]]
#> (Intercept)         trt 
#>           9          10 
#> 
#> 
#> $n_shared_sample_sizes
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]  300  300  250  250  290  290
#> [2,]  300  300  250  250  290  290
#> [3,]  250  250  250  205  241  241
#> [4,]  250  250  205  250  243  243
#> [5,]  290  290  241  243  290  290
#> [6,]  290  290  241  243  290  290
#> 
#> $call
#> jointCovariance.default(coxph_(Surv(time = y, event = death) ~ 
#>     trt, data_index = 1), logrank_(Surv(time = y, event = death) ~ 
#>     trt, data_index = 1), glm_(z1 ~ trt, family = "binomial", 
#>     data_index = 1), glm_(z2 ~ trt, family = "gaussian", data_index = 1), 
#>     mmrm_(x ~ trt + us(visit | pid), reml = TRUE, data_index = 2), 
#>     gee_(x ~ trt, family = "gaussian", corstr = "independence", 
#>         data_index = 2), data = list(dat1, dat2))
#> 
#> attr(,"class")
#> [1] "jointCovariance"


bfit <-jointCovariance(
  coxph_(Surv(time=y, event=death) ~ trt, data_index = 1),
  logrank_(Surv(time=y, event=death) ~ trt, data_index = 1),
  glm_(z1 ~ trt, family = 'binomial', data_index = 1),
  glm_(z2 ~ trt, family = 'gaussian', data_index = 1),
  mmrm_(x ~ trt + us(visit | pid), reml = TRUE, data_index = 2),
  gee_(x ~ trt, family = 'gaussian', corstr = 'independence', data_index = 2),
  data = list(dat1, dat2), nboot = 10)
#> method in mmrm_control is re-set to be "Residual"
#> vcov in mmrm_control is re-set to be "Empirical"

summary(bfit)
#> Call:
#> jointCovariance.default(coxph_(Surv(time = y, event = death) ~ 
#>     trt, data_index = 1), logrank_(Surv(time = y, event = death) ~ 
#>     trt, data_index = 1), glm_(z1 ~ trt, family = "binomial", 
#>     data_index = 1), glm_(z2 ~ trt, family = "gaussian", data_index = 1), 
#>     mmrm_(x ~ trt + us(visit | pid), reml = TRUE, data_index = 2), 
#>     gee_(x ~ trt, family = "gaussian", corstr = "independence", 
#>         data_index = 2), data = list(dat1, dat2), nboot = 10)
#> 
#> Coefficients:
#>                      Estimate Std. Error z value Pr(>|z|)
#> model_1_trt          0.080398   0.157792  0.5095   0.6104
#> model_2_trt          0.629444   1.236528  0.5090   0.6107
#> model_3_(Intercept) -0.085767   0.143246 -0.5987   0.5493
#> model_3_trt          0.122134   0.255381  0.4782   0.6325
#> model_4_(Intercept)  0.018359   0.097943  0.1874   0.8513
#> model_4_trt         -0.133345   0.144983 -0.9197   0.3577
#> model_5_(Intercept) -0.101394   0.078402 -1.2932   0.1959
#> model_5_trt          0.079581   0.112492  0.7074   0.4793
#> model_6_(Intercept) -0.115018   0.086736 -1.3261   0.1848
#> model_6_trt          0.099525   0.115582  0.8611   0.3892

## km_() and quantile_() require nboot > 0 because they have no
## closed-form score. compute_cov is forced to FALSE for km_().
## When all models share one dataset, `data_index` and `list(...)`
## can be omitted.
kfit <- jointCovariance(
  km_(Surv(time = y, event = death) ~ trt, conf_type = 'log',
      times = c(0.5, 1, 1.5)),
  glm_(z1 ~ trt, family = 'binomial'),
  data = dat1, nboot = 30, seed = 1)
#> compute_cov is set to FALSE as km_ is in use. You can compute the covariance matrix using $bootstrap_estimate. 

qfit <- jointCovariance(
  quantile_(y ~ trt, probs = c(0.25, 0.5, 0.75)),
  glm_(z2 ~ trt, family = 'gaussian'),
  data = dat1, nboot = 30, seed = 1)
```
