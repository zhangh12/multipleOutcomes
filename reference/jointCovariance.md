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
#>  [1]  0.28051354  2.19709841 -0.26826398  0.45340642 -0.04969322  0.21341942
#>  [7]  0.01131517  0.07838419  0.01603139  0.09149298
#> 
#> $mcov
#>                [,1]         [,2]         [,3]          [,4]         [,5]
#>  [1,]  1.635173e-02  0.125718402 -0.001274613 -3.500174e-05  0.001830420
#>  [2,]  1.257184e-01  0.969943188 -0.009286670 -1.259712e-03  0.013438767
#>  [3,] -1.274613e-03 -0.009286670  0.033936652 -3.393665e-02  0.007950412
#>  [4,] -3.500174e-05 -0.001259712 -0.033936652  6.497031e-02 -0.007947272
#>  [5,]  1.830420e-03  0.013438767  0.007950412 -7.947272e-03  0.008797555
#>  [6,] -3.639170e-03 -0.028077574 -0.007952369  1.577401e-02 -0.008797555
#>  [7,]  6.621132e-04  0.004847165  0.006144735 -6.144878e-03  0.004400522
#>  [8,] -1.826210e-03 -0.014371017 -0.006144687  1.513319e-02 -0.004400671
#>  [9,]  7.291185e-04  0.005343536  0.006049679 -6.050251e-03  0.004468933
#> [10,] -1.792178e-03 -0.014040200 -0.006049710  1.526871e-02 -0.004468977
#>               [,6]          [,7]         [,8]          [,9]        [,10]
#>  [1,] -0.003639170  0.0006621132 -0.001826210  0.0007291185 -0.001792178
#>  [2,] -0.028077574  0.0048471651 -0.014371017  0.0053435355 -0.014040200
#>  [3,] -0.007952369  0.0061447353 -0.006144687  0.0060496789 -0.006049710
#>  [4,]  0.015774008 -0.0061448779  0.015133193 -0.0060502506  0.015268710
#>  [5,] -0.008797555  0.0044005217 -0.004400671  0.0044689329 -0.004468977
#>  [6,]  0.016982477 -0.0044012022  0.009934177 -0.0044700754  0.010187780
#>  [7,] -0.004401202  0.0048702031 -0.004870203  0.0048294234 -0.004829423
#>  [8,]  0.009934177 -0.0048702031  0.012390849 -0.0048294234  0.012562057
#>  [9,] -0.004470075  0.0048294234 -0.004829423  0.0049778722 -0.004977872
#> [10,]  0.010187780 -0.0048294234  0.012562057 -0.0049778722  0.013179947
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
#> [1,]  300  300  250  250  292  292
#> [2,]  300  300  250  250  292  292
#> [3,]  250  250  250  207  244  244
#> [4,]  250  250  207  250  242  242
#> [5,]  292  292  244  242  292  292
#> [6,]  292  292  244  242  292  292
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

# \donttest{
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
#> model_1_trt          0.280514   0.144859  1.9365  0.05281 .
#> model_2_trt          2.197098   1.140384  1.9266  0.05403 .
#> model_3_(Intercept) -0.268264   0.181748 -1.4760  0.13994  
#> model_3_trt          0.453406   0.248755  1.8227  0.06835 .
#> model_4_(Intercept) -0.049693   0.099091 -0.5015  0.61602  
#> model_4_trt          0.213419   0.141638  1.5068  0.13186  
#> model_5_(Intercept)  0.011315   0.082801  0.1367  0.89130  
#> model_5_trt          0.078384   0.149562  0.5241  0.60022  
#> model_6_(Intercept)  0.016031   0.082072  0.1953  0.84513  
#> model_6_trt          0.091493   0.157054  0.5826  0.56019  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

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
# }
```
