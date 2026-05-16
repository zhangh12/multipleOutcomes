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
#>  [1]  0.114763782  0.914043662  0.115831816  0.149275931  0.023516965
#>  [6]  0.021534389 -0.003481828  0.001985450 -0.003313751  0.017943711
#> 
#> $mcov
#>                [,1]         [,2]         [,3]         [,4]          [,5]
#>  [1,]  0.0156726071  0.124327714 -0.003014050  0.004357725  0.0005758461
#>  [2,]  0.1243277142  0.986761945 -0.023383177  0.034304612  0.0044622517
#>  [3,] -0.0030140503 -0.023383177  0.033168860 -0.033168860  0.0068153253
#>  [4,]  0.0043577250  0.034304612 -0.033168860  0.064724633 -0.0068602365
#>  [5,]  0.0005758461  0.004462252  0.006815325 -0.006860237  0.0082001306
#>  [6,] -0.0015769265 -0.012644988 -0.006816356  0.013485374 -0.0082001306
#>  [7,]  0.0004390852  0.003433940  0.007036984 -0.007034904  0.0044847682
#>  [8,] -0.0013210131 -0.010538376 -0.007031246  0.014564401 -0.0044858539
#>  [9,]  0.0003918193  0.003066510  0.007340231 -0.007338907  0.0046176712
#> [10,] -0.0010072901 -0.008020632 -0.007335540  0.014886949 -0.0046187440
#>               [,6]          [,7]         [,8]          [,9]        [,10]
#>  [1,] -0.001576927  0.0004390852 -0.001321013  0.0003918193 -0.001007290
#>  [2,] -0.012644988  0.0034339395 -0.010538376  0.0030665098 -0.008020632
#>  [3,] -0.006816356  0.0070369845 -0.007031246  0.0073402310 -0.007335540
#>  [4,]  0.013485374 -0.0070349035  0.014564401 -0.0073389075  0.014886949
#>  [5,] -0.008200131  0.0044847682 -0.004485854  0.0046176712 -0.004618744
#>  [6,]  0.016100349 -0.0044847598  0.008697018 -0.0046176637  0.008521276
#>  [7,] -0.004484760  0.0055767924 -0.005576792  0.0056994503 -0.005699450
#>  [8,]  0.008697018 -0.0055767924  0.012048242 -0.0056994503  0.011963294
#>  [9,] -0.004617664  0.0056994503 -0.005699450  0.0060864245 -0.006086424
#> [10,]  0.008521276 -0.0056994503  0.011963294 -0.0060864245  0.012461240
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
#> [1,]  300  300  250  250  291  291
#> [2,]  300  300  250  250  291  291
#> [3,]  250  250  250  212  243  243
#> [4,]  250  250  212  250  243  243
#> [5,]  291  291  243  243  291  291
#> [6,]  291  291  243  243  291  291
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
#> model_1_trt          0.1147638  0.1588465  0.7225   0.4700
#> model_2_trt          0.9140437  1.2648107  0.7227   0.4699
#> model_3_(Intercept)  0.1158318  0.1503387  0.7705   0.4410
#> model_3_trt          0.1492759  0.2911496  0.5127   0.6082
#> model_4_(Intercept)  0.0235170  0.1031410  0.2280   0.8196
#> model_4_trt          0.0215344  0.1171998  0.1837   0.8542
#> model_5_(Intercept) -0.0034818  0.0689771 -0.0505   0.9597
#> model_5_trt          0.0019855  0.0996949  0.0199   0.9841
#> model_6_(Intercept) -0.0033138  0.0603111 -0.0549   0.9562
#> model_6_trt          0.0179437  0.0843368  0.2128   0.8315

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
