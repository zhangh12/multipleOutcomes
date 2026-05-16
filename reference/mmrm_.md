# Creating Objects of Mixed Models for Repeated Measures

`mmrm_` is a wrapper function of
[`mmrm::mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
to create an object to be passed into `jointCovariance`, the main
function of this package through its argument `...`. The object defines
how a MMRM model would be fitted.

## Usage

``` r
mmrm_(
  formula,
  covariance = NULL,
  reml = TRUE,
  control = mmrm::mmrm_control(...),
  ...,
  data_index = 1
)
```

## Arguments

- formula:

  see `formula` in
  [`mmrm::mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).

- covariance:

  see `covariance` in
  [`mmrm::mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).

- reml:

  see `reml` in
  [`mmrm::mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).

- control:

  see `control` in
  [`mmrm::mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).

- ...:

  see `...` in
  [`mmrm::mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html).

- data_index:

  integer. Index of the data frame in the `data` argument of
  `jointCovariance` to be used when fitting a GEE model.

## Details

The argument `weights` of
[`mmrm::mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
is supported in `mmrm_` due to the complexity in handling environment
and scope.

Please always refer to help document of
[`mmrm::mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
before using `mmrm_`. For example, time variable and observation ID must
be factor variables in some cases, otherwise error may be prompted.
Users can call
[`mmrm::mmrm`](https://openpharma.github.io/mmrm/latest-tag/reference/mmrm.html)
using the same arguments being passed to `mmrm_` to check validity.
