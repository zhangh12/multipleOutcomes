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
#>  [1]  0.064052959  0.500051571 -0.081345639 -0.155982545 -0.025622116
#>  [6] -0.240982437 -0.004295738 -0.103036218  0.012762921 -0.132798780
#> 
#> $mcov
#>                [,1]          [,2]          [,3]         [,4]          [,5]
#>  [1,]  1.625810e-02  0.1269998142  0.0005789243 -0.001222628  0.0009140156
#>  [2,]  1.269998e-01  0.9922466518  0.0044898971 -0.009664981  0.0070399072
#>  [3,]  5.789243e-04  0.0044898971  0.0325741525 -0.032574153  0.0075124371
#>  [4,] -1.222628e-03 -0.0096649809 -0.0325741525  0.064515802 -0.0075146149
#>  [5,]  9.140156e-04  0.0070399072  0.0075124371 -0.007514615  0.0079938336
#>  [6,] -2.010523e-03 -0.0156932362 -0.0075013337  0.014962614 -0.0079938336
#>  [7,] -5.876725e-05 -0.0004414186  0.0063349114 -0.006334963  0.0042326398
#>  [8,]  3.537237e-04  0.0027453860 -0.0063332229  0.012934524 -0.0042328151
#>  [9,] -3.207981e-05 -0.0002419112  0.0062651304 -0.006265353  0.0040912605
#> [10,]  1.438744e-04  0.0010921470 -0.0062634521  0.012867598 -0.0040913742
#>               [,6]          [,7]          [,8]          [,9]         [,10]
#>  [1,] -0.002010523 -5.876725e-05  0.0003537237 -3.207981e-05  0.0001438744
#>  [2,] -0.015693236 -4.414186e-04  0.0027453860 -2.419112e-04  0.0010921470
#>  [3,] -0.007501334  6.334911e-03 -0.0063332229  6.265130e-03 -0.0062634521
#>  [4,]  0.014962614 -6.334963e-03  0.0129345236 -6.265353e-03  0.0128675985
#>  [5,] -0.007993834  4.232640e-03 -0.0042328151  4.091261e-03 -0.0040913742
#>  [6,]  0.016057110 -4.237177e-03  0.0087494249 -4.095602e-03  0.0087214524
#>  [7,] -0.004237177  4.900835e-03 -0.0049008349  4.745276e-03 -0.0047452763
#>  [8,]  0.008749425 -4.900835e-03  0.0109239956 -4.745276e-03  0.0106138534
#>  [9,] -0.004095602  4.745276e-03 -0.0047452763  4.834979e-03 -0.0048349793
#> [10,]  0.008721452 -4.745276e-03  0.0106138534 -4.834979e-03  0.0108242741
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
#> [1,]  300  300  250  250  288  288
#> [2,]  300  300  250  250  288  288
#> [3,]  250  250  250  210  240  240
#> [4,]  250  250  210  250  242  242
#> [5,]  288  288  240  242  288  288
#> [6,]  288  288  240  242  288  288
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
#>                       Estimate Std. Error z value Pr(>|z|)  
#> model_1_trt          0.0640530  0.1391812  0.4602  0.64536  
#> model_2_trt          0.5000516  1.0750214  0.4652  0.64182  
#> model_3_(Intercept) -0.0813456  0.1475522 -0.5513  0.58143  
#> model_3_trt         -0.1559825  0.2282230 -0.6835  0.49431  
#> model_4_(Intercept) -0.0256221  0.0978597 -0.2618  0.79346  
#> model_4_trt         -0.2409824  0.1249520 -1.9286  0.05378 .
#> model_5_(Intercept) -0.0042957  0.0768924 -0.0559  0.95545  
#> model_5_trt         -0.1030362  0.1010201 -1.0200  0.30775  
#> model_6_(Intercept)  0.0127629  0.0705661  0.1809  0.85647  
#> model_6_trt         -0.1327988  0.0992310 -1.3383  0.18081  
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
```
