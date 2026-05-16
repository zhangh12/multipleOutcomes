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
#>  [1] -0.041349265 -0.320395826 -0.016529302  0.218766168  0.119550344
#>  [6]  0.010816800  0.024550786  0.011051445  0.032785986 -0.006565258
#> 
#> $mcov
#>                [,1]         [,2]         [,3]          [,4]          [,5]
#>  [1,]  0.0165782257  0.128315547  0.001034113  0.0006081668  0.0007667161
#>  [2,]  0.1283155472  0.993248570  0.008120459  0.0044369880  0.0059685812
#>  [3,]  0.0010341127  0.008120459  0.033060109 -0.0330601093  0.0089469853
#>  [4,]  0.0006081668  0.004436988 -0.033060109  0.0643859956 -0.0089538099
#>  [5,]  0.0007667161  0.005968581  0.008946985 -0.0089538099  0.0096552687
#>  [6,] -0.0011965931 -0.009281096 -0.008933074  0.0170947871 -0.0096552687
#>  [7,]  0.0006668136  0.005197870  0.007734964 -0.0077352667  0.0054207936
#>  [8,] -0.0006566166 -0.005131446 -0.007736891  0.0149447807 -0.0054208323
#>  [9,]  0.0005660354  0.004410652  0.007944507 -0.0079449338  0.0056343475
#> [10,] -0.0005947587 -0.004643409 -0.007946842  0.0151624097 -0.0056343570
#>               [,6]          [,7]          [,8]          [,9]         [,10]
#>  [1,] -0.001196593  0.0006668136 -0.0006566166  0.0005660354 -0.0005947587
#>  [2,] -0.009281096  0.0051978703 -0.0051314461  0.0044106521 -0.0046434089
#>  [3,] -0.008933074  0.0077349643 -0.0077368912  0.0079445072 -0.0079468419
#>  [4,]  0.017094787 -0.0077352667  0.0149447807 -0.0079449338  0.0151624097
#>  [5,] -0.009655269  0.0054207936 -0.0054208323  0.0056343475 -0.0056343570
#>  [6,]  0.017876803 -0.0054243695  0.0097948450 -0.0056375089  0.0100475916
#>  [7,] -0.005424369  0.0065718535 -0.0065718535  0.0067587312 -0.0067587312
#>  [8,]  0.009794845 -0.0065718535  0.0119167234 -0.0067587312  0.0120512811
#>  [9,] -0.005637509  0.0067587312 -0.0067587312  0.0071587008 -0.0071587008
#> [10,]  0.010047592 -0.0067587312  0.0120512811 -0.0071587008  0.0126399726
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
#> [3,]  250  250  250  204  243  243
#> [4,]  250  250  204  250  242  242
#> [5,]  291  291  243  242  291  291
#> [6,]  291  291  243  242  291  291
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
#> model_1_trt         -0.0413493  0.1022781 -0.4043  0.68600  
#> model_2_trt         -0.3203958  0.7899544 -0.4056  0.68505  
#> model_3_(Intercept) -0.0165293  0.1751397 -0.0944  0.92481  
#> model_3_trt          0.2187662  0.2525646  0.8662  0.38639  
#> model_4_(Intercept)  0.1195503  0.0610368  1.9587  0.05015 .
#> model_4_trt          0.0108168  0.1219004  0.0887  0.92929  
#> model_5_(Intercept)  0.0245508  0.0617658  0.3975  0.69101  
#> model_5_trt          0.0110514  0.1256310  0.0880  0.92990  
#> model_6_(Intercept)  0.0327860  0.0646885  0.5068  0.61228  
#> model_6_trt         -0.0065653  0.1359049 -0.0483  0.96147  
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
