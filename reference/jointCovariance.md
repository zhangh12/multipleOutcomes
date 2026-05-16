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
#>  [1]  3.370255e-01  2.653579e+00 -5.694345e-17  4.554755e-01  7.223719e-03
#>  [6]  9.579653e-02  3.255670e-02  7.271594e-02  1.994335e-02  1.047178e-01
#> 
#> $mcov
#>                [,1]         [,2]          [,3]          [,4]         [,5]
#>  [1,]  0.0161601752  0.124316121  0.0004455437 -0.0001517526  0.001867586
#>  [2,]  0.1243161212  0.960662922  0.0032354654 -0.0010006109  0.013560045
#>  [3,]  0.0004455437  0.003235465  0.0344827586 -0.0344827586  0.010674814
#>  [4,] -0.0001517526 -0.001000611 -0.0344827586  0.0659086498 -0.010674895
#>  [5,]  0.0018675864  0.013560045  0.0106748142 -0.0106748946  0.010611072
#>  [6,] -0.0036325618 -0.027951789 -0.0106818442  0.0172880831 -0.010611072
#>  [7,]  0.0013532969  0.009721122  0.0090852956 -0.0090821550  0.005997258
#>  [8,] -0.0021670005 -0.016395175 -0.0090802259  0.0155225651 -0.005997663
#>  [9,]  0.0013085344  0.009507654  0.0087627497 -0.0087617223  0.005681064
#> [10,] -0.0022106072 -0.016916927 -0.0087572903  0.0154851717 -0.005681382
#>               [,6]         [,7]         [,8]         [,9]        [,10]
#>  [1,] -0.003632562  0.001353297 -0.002167001  0.001308534 -0.002210607
#>  [2,] -0.027951789  0.009721122 -0.016395175  0.009507654 -0.016916927
#>  [3,] -0.010681844  0.009085296 -0.009080226  0.008762750 -0.008757290
#>  [4,]  0.017288083 -0.009082155  0.015522565 -0.008761722  0.015485172
#>  [5,] -0.010611072  0.005997258 -0.005997663  0.005681064 -0.005681382
#>  [6,]  0.018487158 -0.005994617  0.010859577 -0.005678058  0.010651846
#>  [7,] -0.005994617  0.007086684 -0.007086684  0.006671955 -0.006671955
#>  [8,]  0.010859577 -0.007086684  0.012651378 -0.006671955  0.012341514
#>  [9,] -0.005678058  0.006671955 -0.006671955  0.006667421 -0.006667421
#> [10,]  0.010651846 -0.006671955  0.012341514 -0.006667421  0.012661261
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
#> [1,]  300  300  250  250  295  295
#> [2,]  300  300  250  250  295  295
#> [3,]  250  250  250  209  246  246
#> [4,]  250  250  209  250  246  246
#> [5,]  295  295  246  246  295  295
#> [6,]  295  295  246  246  295  295
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
#>                        Estimate  Std. Error z value Pr(>|z|)   
#> model_1_trt          3.3703e-01  1.4604e-01  2.3077 0.021015 * 
#> model_2_trt          2.6536e+00  1.1563e+00  2.2950 0.021736 * 
#> model_3_(Intercept) -5.6943e-17  1.4944e-01  0.0000 1.000000   
#> model_3_trt          4.5548e-01  1.7302e-01  2.6324 0.008477 **
#> model_4_(Intercept)  7.2237e-03  1.2902e-01  0.0560 0.955351   
#> model_4_trt          9.5797e-02  1.4700e-01  0.6517 0.514610   
#> model_5_(Intercept)  3.2557e-02  8.2771e-02  0.3933 0.694072   
#> model_5_trt          7.2716e-02  7.5877e-02  0.9583 0.337890   
#> model_6_(Intercept)  1.9943e-02  6.9679e-02  0.2862 0.774712   
#> model_6_trt          1.0472e-01  7.8831e-02  1.3284 0.184050   
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
