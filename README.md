
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multipleOutcomes

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

The goal of multipleOutcomes is to fit statistical models for multiple
outcomes simultaneously. It computes estimates of parameters across
fitted models and returns the matrix of asymptotic covariance. Various
applications of this package, including PATED (Prognostic Variables
Assisted Treatment Effect Detection), multiple comparison adjustment,
conditional power are illustrated.

## Installation

You can install the development version of multipleOutcomes from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("zhangh12/multipleOutcomes")
```

## Example

Let’s get started by analyzing a really randomized trial data

``` r
library(multipleOutcomes)
library(dplyr)

options(digits = 2)

data(indo)

fit <- pated(
  outcome ~ rx, 
  
  risk ~ rx, 
  gender ~ rx, 
  sod ~ rx, 
  pep ~ rx,
  recpanc ~ rx,
  psphinc ~ rx, 
  precut ~ rx, 
  difcan ~ rx,  
  amp ~ rx, 
  paninj ~ rx, 
  acinar ~ rx,
  asa81 ~ rx, 
  asa ~ rx,
  prophystent ~ rx, 
  therastent ~ rx, 
  pdstent ~ rx, 
  
  data = indo,
  family = c('binomial', 
             rep('gaussian', 1), 
             rep('binomial', 15))
)

plot(fit)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
print(fit)
#>           term   family estimate stderr pvalue     method    corr
#> 1      outcome    PATED  -0.7514  0.247 0.0023      PATED      NA
#> 2      outcome binomial  -0.7051  0.253 0.0053   Standard  1.0000
#> 3         risk gaussian   0.0833  0.072 0.2450 Prognostic  0.1397
#> 4       gender binomial   0.1710  0.201 0.3940 Prognostic  0.0041
#> 5          sod binomial   0.2482  0.215 0.2472 Prognostic -0.0105
#> 6          pep binomial  -0.0021  0.223 0.9923 Prognostic  0.1263
#> 7      recpanc binomial  -0.0700  0.178 0.6945 Prognostic  0.0331
#> 8      psphinc binomial   0.1335  0.165 0.4181 Prognostic  0.0375
#> 9       precut binomial  -0.0901  0.364 0.8046 Prognostic  0.0060
#> 10      difcan binomial   0.1057  0.186 0.5694 Prognostic  0.0481
#> 11         amp binomial   0.0411  0.479 0.9316 Prognostic  0.0829
#> 12      paninj binomial  -0.3609  0.274 0.1880 Prognostic  0.0535
#> 13      acinar binomial   0.2753  0.396 0.4871 Prognostic  0.0574
#> 14       asa81 binomial  -0.3947  0.316 0.2115 Prognostic -0.0079
#> 15         asa binomial  -0.0723  0.279 0.7953 Prognostic -0.0242
#> 16 prophystent binomial   0.2153  0.190 0.2562 Prognostic  0.0341
#> 17  therastent binomial  -0.2529  0.316 0.4233 Prognostic -0.0494
#> 18     pdstent binomial   0.1812  0.215 0.3990 Prognostic  0.0063
```
