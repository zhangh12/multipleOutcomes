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
#>  [1]  0.11602890  0.93168860 -0.17366349  0.32026697 -0.05873972  0.01271030
#>  [7] -0.07036580  0.12589874 -0.05975743  0.08339852
#> 
#> $mcov
#>                [,1]          [,2]         [,3]          [,4]          [,5]
#>  [1,]  1.543647e-02  1.234140e-01 -0.000714732  2.612814e-05  0.0002893519
#>  [2,]  1.234140e-01  9.871759e-01 -0.005709737  8.964820e-06  0.0023355830
#>  [3,] -7.147320e-04 -5.709737e-03  0.031734133 -3.173413e-02  0.0076351390
#>  [4,]  2.612814e-05  8.964820e-06 -0.031734133  6.442951e-02 -0.0076360723
#>  [5,]  2.893519e-04  2.335583e-03  0.007635139 -7.636072e-03  0.0073029678
#>  [6,] -1.436163e-03 -1.164885e-02 -0.007646408  1.543419e-02 -0.0073029678
#>  [7,]  1.431307e-04  1.179109e-03  0.007486003 -7.485541e-03  0.0046262423
#>  [8,] -1.711218e-03 -1.397176e-02 -0.007484032  1.673044e-02 -0.0046264039
#>  [9,]  2.125966e-04  1.723619e-03  0.007674354 -7.673904e-03  0.0046903574
#> [10,] -1.772753e-03 -1.443188e-02 -0.007671407  1.715982e-02 -0.0046905175
#>               [,6]          [,7]         [,8]          [,9]        [,10]
#>  [1,] -0.001436163  0.0001431307 -0.001711218  0.0002125966 -0.001772753
#>  [2,] -0.011648850  0.0011791094 -0.013971764  0.0017236191 -0.014431883
#>  [3,] -0.007646408  0.0074860026 -0.007484032  0.0076743539 -0.007671407
#>  [4,]  0.015434194 -0.0074855410  0.016730442 -0.0076739041  0.017159823
#>  [5,] -0.007302968  0.0046262423 -0.004626404  0.0046903574 -0.004690518
#>  [6,]  0.014760118 -0.0046213423  0.009239999 -0.0046859427  0.009282291
#>  [7,] -0.004621342  0.0060697035 -0.006069703  0.0061181703 -0.006118170
#>  [8,]  0.009239999 -0.0060697035  0.013138638 -0.0061181703  0.013177148
#>  [9,] -0.004685943  0.0061181703 -0.006118170  0.0064839996 -0.006484000
#> [10,]  0.009282291 -0.0061181703  0.013177148 -0.0064839996  0.013873079
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
#> [3,]  250  250  250  207  243  243
#> [4,]  250  250  207  250  244  244
#> [5,]  292  292  243  244  292  292
#> [6,]  292  292  243  244  292  292
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
#> model_1_trt          0.116029   0.120653  0.9617   0.3362
#> model_2_trt          0.931689   0.967809  0.9627   0.3357
#> model_3_(Intercept) -0.173663   0.147525 -1.1772   0.2391
#> model_3_trt          0.320267   0.222499  1.4394   0.1500
#> model_4_(Intercept) -0.058740   0.107884 -0.5445   0.5861
#> model_4_trt          0.012710   0.120992  0.1051   0.9163
#> model_5_(Intercept) -0.070366   0.058644 -1.1999   0.2302
#> model_5_trt          0.125899   0.089754  1.4027   0.1607
#> model_6_(Intercept) -0.059757   0.063443 -0.9419   0.3462
#> model_6_trt          0.083399   0.094779  0.8799   0.3789

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
