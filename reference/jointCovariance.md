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
#>  [1]  0.10331855  0.82419099  0.15180601 -0.28760755  0.04624109 -0.11354564
#>  [7]  0.01579814 -0.04458443  0.03313590 -0.07851811
#> 
#> $mcov
#>                [,1]          [,2]         [,3]         [,4]          [,5]
#>  [1,]  1.572164e-02  1.251261e-01 -0.001989314  0.002148512  6.693996e-08
#>  [2,]  1.251261e-01  9.963004e-01 -0.015478933  0.016921924 -6.303944e-05
#>  [3,] -1.989314e-03 -1.547893e-02  0.030477950 -0.030477950  7.460195e-03
#>  [4,]  2.148512e-03  1.692192e-02 -0.030477950  0.064532784 -7.465507e-03
#>  [5,]  6.693996e-08 -6.303944e-05  0.007460195 -0.007465507  7.926947e-03
#>  [6,] -5.404805e-04 -4.355545e-03 -0.007463851  0.014713173 -7.926947e-03
#>  [7,]  1.033211e-04  7.995547e-04  0.007441119 -0.007441036  5.057736e-03
#>  [8,] -8.467127e-04 -6.813168e-03 -0.007441481  0.014196561 -5.055273e-03
#>  [9,]  7.639183e-05  5.972507e-04  0.007294609 -0.007294549  5.087332e-03
#> [10,] -7.690904e-04 -6.209997e-03 -0.007295006  0.013929518 -5.085458e-03
#>                [,6]          [,7]          [,8]          [,9]         [,10]
#>  [1,] -0.0005404805  0.0001033211 -0.0008467127  7.639183e-05 -0.0007690904
#>  [2,] -0.0043555450  0.0007995547 -0.0068131679  5.972507e-04 -0.0062099971
#>  [3,] -0.0074638513  0.0074411189 -0.0074414813  7.294609e-03 -0.0072950057
#>  [4,]  0.0147131725 -0.0074410363  0.0141965606 -7.294549e-03  0.0139295176
#>  [5,] -0.0079269466  0.0050577364 -0.0050552730  5.087332e-03 -0.0050854576
#>  [6,]  0.0161180469 -0.0050578393  0.0087733424 -5.087447e-03  0.0090047560
#>  [7,] -0.0050578393  0.0057711392 -0.0057711392  5.791189e-03 -0.0057911886
#>  [8,]  0.0087733424 -0.0057711392  0.0107659897 -5.791189e-03  0.0108347221
#>  [9,] -0.0050874466  0.0057911886 -0.0057911886  6.000799e-03 -0.0060007995
#> [10,]  0.0090047560 -0.0057911886  0.0108347221 -6.000799e-03  0.0112920249
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
#> [3,]  250  250  250  209  244  244
#> [4,]  250  250  209  250  243  243
#> [5,]  292  292  244  243  292  292
#> [6,]  292  292  244  243  292  292
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
#> model_1_trt          0.103319   0.098405  1.0499   0.2937
#> model_2_trt          0.824191   0.775237  1.0631   0.2877
#> model_3_(Intercept)  0.151806   0.113871  1.3331   0.1825
#> model_3_trt         -0.287608   0.220386 -1.3050   0.1919
#> model_4_(Intercept)  0.046241   0.061787  0.7484   0.4542
#> model_4_trt         -0.113546   0.075930 -1.4954   0.1348
#> model_5_(Intercept)  0.015798   0.061258  0.2579   0.7965
#> model_5_trt         -0.044584   0.063088 -0.7067   0.4798
#> model_6_(Intercept)  0.033136   0.054919  0.6034   0.5463
#> model_6_trt         -0.078518   0.057951 -1.3549   0.1754

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
