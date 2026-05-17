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
#>  [1]  0.21348514  1.70094701  0.08855340  0.22045144 -0.01972026  0.15608819
#>  [7] -0.04804925  0.01592017 -0.06886867  0.02893518
#> 
#> $mcov
#>                [,1]         [,2]         [,3]         [,4]          [,5]
#>  [1,]  0.0156802690  0.124006569 -0.002199680  0.004035362  0.0004939039
#>  [2,]  0.1240065686  0.982375107 -0.017113039  0.031871156  0.0036013938
#>  [3,] -0.0021996805 -0.017113039  0.035467671 -0.035467671  0.0077457149
#>  [4,]  0.0040353618  0.031871156 -0.035467671  0.065367278 -0.0077447718
#>  [5,]  0.0004939039  0.003601394  0.007745715 -0.007744772  0.0084994244
#>  [6,] -0.0010220752 -0.007921925 -0.007750579  0.015961143 -0.0084994244
#>  [7,]  0.0002576265  0.001927432  0.007416328 -0.007416054  0.0049762917
#>  [8,] -0.0007161122 -0.005752429 -0.007411753  0.015305818 -0.0049739764
#>  [9,]  0.0002927342  0.002235022  0.007653351 -0.007653047  0.0052518070
#> [10,] -0.0006166250 -0.004970783 -0.007648251  0.015592438 -0.0052493446
#>               [,6]          [,7]          [,8]          [,9]        [,10]
#>  [1,] -0.001022075  0.0002576265 -0.0007161122  0.0002927342 -0.000616625
#>  [2,] -0.007921925  0.0019274315 -0.0057524290  0.0022350225 -0.004970783
#>  [3,] -0.007750579  0.0074163279 -0.0074117532  0.0076533506 -0.007648251
#>  [4,]  0.015961143 -0.0074160535  0.0153058182 -0.0076530472  0.015592438
#>  [5,] -0.008499424  0.0049762917 -0.0049739764  0.0052518070 -0.005249345
#>  [6,]  0.016766859 -0.0049758104  0.0099420190 -0.0052515066  0.010379892
#>  [7,] -0.004975810  0.0060808183 -0.0060808183  0.0064284522 -0.006428452
#>  [8,]  0.009942019 -0.0060808183  0.0130431718 -0.0064284522  0.013535833
#>  [9,] -0.005251507  0.0064284522 -0.0064284522  0.0070329993 -0.007032999
#> [10,]  0.010379892 -0.0064284522  0.0135358331 -0.0070329993  0.014529294
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
#> [4,]  250  250  207  250  244  244
#> [5,]  292  292  244  244  292  292
#> [6,]  292  292  244  244  292  292
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
#> model_1_trt          0.213485   0.118814  1.7968  0.07237 .
#> model_2_trt          1.700947   0.940847  1.8079  0.07062 .
#> model_3_(Intercept)  0.088553   0.161753  0.5475  0.58406  
#> model_3_trt          0.220451   0.202356  1.0894  0.27597  
#> model_4_(Intercept) -0.019720   0.090206 -0.2186  0.82695  
#> model_4_trt          0.156088   0.112528  1.3871  0.16541  
#> model_5_(Intercept) -0.048049   0.102719 -0.4678  0.63995  
#> model_5_trt          0.015920   0.132841  0.1198  0.90461  
#> model_6_(Intercept) -0.068869   0.106670 -0.6456  0.51852  
#> model_6_trt          0.028935   0.130144  0.2223  0.82406  
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
