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
  [`gee_()`](https://zhangh12.github.io/multipleOutcomes/reference/gee_.md),
  [`mmrm_()`](https://zhangh12.github.io/multipleOutcomes/reference/mmrm_.md),
  [`km_()`](https://zhangh12.github.io/multipleOutcomes/reference/km_.md),
  [`quantile_()`](https://zhangh12.github.io/multipleOutcomes/reference/quantile_.md),
  [`netbenefit_()`](https://zhangh12.github.io/multipleOutcomes/reference/netbenefit_.md),
  or
  [`winratio_()`](https://zhangh12.github.io/multipleOutcomes/reference/winratio_.md).

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
#>  [1]  0.05847697  0.45218525  0.13883644 -0.12541342  0.04743941  0.02566724
#>  [7]  0.06897410 -0.05407552  0.05780440 -0.04296654
#> 
#> $mcov
#>                [,1]         [,2]          [,3]         [,4]          [,5]
#>  [1,]  0.0173290339  0.133924789 -0.0009420196  0.001000944  0.0007057356
#>  [2,]  0.1339247889  1.035261312 -0.0072486761  0.007724330  0.0053996004
#>  [3,] -0.0009420196 -0.007248676  0.0397951143 -0.039795114  0.0106658694
#>  [4,]  0.0010009440  0.007724330 -0.0397951143  0.066641961 -0.0106655704
#>  [5,]  0.0007057356  0.005399600  0.0106658694 -0.010665570  0.0088595898
#>  [6,] -0.0013776917 -0.010641408 -0.0106507718  0.017276414 -0.0088595898
#>  [7,]  0.0006261310  0.004822472  0.0080577357 -0.008054977  0.0051643816
#>  [8,] -0.0013496584 -0.010476164 -0.0080609861  0.013638592 -0.0051638094
#>  [9,]  0.0006817392  0.005252122  0.0081529456 -0.008150229  0.0053731852
#> [10,] -0.0014082482 -0.010936417 -0.0081562938  0.013723207 -0.0053727364
#>               [,6]         [,7]         [,8]          [,9]        [,10]
#>  [1,] -0.001377692  0.000626131 -0.001349658  0.0006817392 -0.001408248
#>  [2,] -0.010641408  0.004822472 -0.010476164  0.0052521218 -0.010936417
#>  [3,] -0.010650772  0.008057736 -0.008060986  0.0081529456 -0.008156294
#>  [4,]  0.017276414 -0.008054977  0.013638592 -0.0081502287  0.013723207
#>  [5,] -0.008859590  0.005164382 -0.005163809  0.0053731852 -0.005372736
#>  [6,]  0.016391815 -0.005165035  0.009387679 -0.0053740049  0.009495374
#>  [7,] -0.005165035  0.006993630 -0.006993630  0.0070923166 -0.007092317
#>  [8,]  0.009387679 -0.006993630  0.012003780 -0.0070923166  0.011987328
#>  [9,] -0.005374005  0.007092317 -0.007092317  0.0074350459 -0.007435046
#> [10,]  0.009495374 -0.007092317  0.011987328 -0.0074350459  0.012468507
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
#> [3,]  250  250  250  207  242  242
#> [4,]  250  250  207  250  242  242
#> [5,]  290  290  242  242  290  290
#> [6,]  290  290  242  242  290  290
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
#> model_1_trt          0.058477   0.135142  0.4327   0.6652
#> model_2_trt          0.452185   1.049986  0.4307   0.6667
#> model_3_(Intercept)  0.138836   0.242654  0.5722   0.5672
#> model_3_trt         -0.125413   0.304671 -0.4116   0.6806
#> model_4_(Intercept)  0.047439   0.110437  0.4296   0.6675
#> model_4_trt          0.025667   0.103390  0.2483   0.8039
#> model_5_(Intercept)  0.068974   0.081399  0.8474   0.3968
#> model_5_trt         -0.054076   0.115098 -0.4698   0.6385
#> model_6_(Intercept)  0.057804   0.092356  0.6259   0.5314
#> model_6_trt         -0.042967   0.140405 -0.3060   0.7596

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
