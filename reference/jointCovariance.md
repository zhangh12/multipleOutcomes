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
#>  [1]  0.156959260  1.263141539  0.137621378  0.115827520  0.073100567
#>  [6] -0.008857356  0.036221323  0.128823611  0.053092522  0.104006341
#> 
#> $mcov
#>                [,1]         [,2]         [,3]         [,4]          [,5]
#>  [1,]  0.0153329698  0.122644908 -0.001573951  0.001578933 -0.0003000783
#>  [2,]  0.1226449079  0.981854932 -0.012516388  0.012668220 -0.0023174026
#>  [3,] -0.0015739511 -0.012516388  0.030679157 -0.030679157  0.0069237994
#>  [4,]  0.0015789326  0.012668220 -0.030679157  0.064835299 -0.0069156488
#>  [5,] -0.0003000783 -0.002317403  0.006923799 -0.006915649  0.0073189555
#>  [6,] -0.0004591725 -0.003873493 -0.006938956  0.015834102 -0.0073189555
#>  [7,] -0.0006646217 -0.005230080  0.007121273 -0.007106235  0.0047684450
#>  [8,]  0.0002259563  0.001538896 -0.007118560  0.015743003 -0.0047678079
#>  [9,] -0.0007585949 -0.005980291  0.007549780 -0.007535649  0.0050771132
#> [10,]  0.0002760815  0.001919009 -0.007547105  0.016423279 -0.0050766535
#>                [,6]          [,7]          [,8]          [,9]         [,10]
#>  [1,] -0.0004591725 -0.0006646217  0.0002259563 -0.0007585949  0.0002760815
#>  [2,] -0.0038734935 -0.0052300803  0.0015388956 -0.0059802906  0.0019190093
#>  [3,] -0.0069389557  0.0071212733 -0.0071185596  0.0075497802 -0.0075471046
#>  [4,]  0.0158341021 -0.0071062349  0.0157430031 -0.0075356490  0.0164232794
#>  [5,] -0.0073189555  0.0047684450 -0.0047678079  0.0050771132 -0.0050766535
#>  [6,]  0.0164535947 -0.0047659713  0.0100892412 -0.0050747152  0.0106999180
#>  [7,] -0.0047659713  0.0061103011 -0.0061103011  0.0061150498 -0.0061150498
#>  [8,]  0.0100892412 -0.0061103011  0.0128575762 -0.0061150498  0.0129975992
#>  [9,] -0.0050747152  0.0061150498 -0.0061150498  0.0064927150 -0.0064927150
#> [10,]  0.0106999180 -0.0061150498  0.0129975992 -0.0064927150  0.0138160015
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
#> [3,]  250  250  250  209  241  241
#> [4,]  250  250  209  250  244  244
#> [5,]  291  291  241  244  291  291
#> [6,]  291  291  241  244  291  291
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
#>                       Estimate Std. Error z value Pr(>|z|)
#> model_1_trt          0.1569593  0.1148568  1.3666   0.1718
#> model_2_trt          1.2631415  0.9204162  1.3724   0.1700
#> model_3_(Intercept)  0.1376214  0.1897985  0.7251   0.4684
#> model_3_trt          0.1158275  0.2966933  0.3904   0.6962
#> model_4_(Intercept)  0.0731006  0.0838737  0.8716   0.3835
#> model_4_trt         -0.0088574  0.1622750 -0.0546   0.9565
#> model_5_(Intercept)  0.0362213  0.0614891  0.5891   0.5558
#> model_5_trt          0.1288236  0.1483228  0.8685   0.3851
#> model_6_(Intercept)  0.0530925  0.0722850  0.7345   0.4627
#> model_6_trt          0.1040063  0.1667498  0.6237   0.5328

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
