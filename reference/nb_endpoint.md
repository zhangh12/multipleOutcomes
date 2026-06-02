# Endpoint Constructors for netbenefit\_()

These helpers build the individual endpoint specifications that
[`netbenefit_()`](https://zhangh12.github.io/multipleOutcomes/reference/netbenefit_.md)
consumes through its `endpoints` argument. Each call returns a small
list describing one comparison rule; the order in which they appear in
the `endpoints` list defines the hierarchical priority (first =
highest).

## Usage

``` r
nb_tte(
  time,
  event,
  direction = c("longer_better", "shorter_better"),
  margin = 0,
  censor_rule = c("informative", "ignore")
)

nb_continuous(
  value,
  direction = c("larger_better", "smaller_better"),
  margin = 0
)

nb_binary(value, direction = c("larger_better", "smaller_better"))
```

## Arguments

- time:

  character. Name of the time column in `data`.

- event:

  character. Name of the event-indicator column in `data` (1 = event
  observed, 0 = censored).

- direction:

  one of `"longer_better"` / `"shorter_better"` (`nb_tte()`) or
  `"larger_better"` / `"smaller_better"` (`nb_continuous()`,
  `nb_binary()`).

- margin:

  non-negative numeric. Minimum directional gap required to declare a
  winner on this endpoint. Defaults to 0.

- censor_rule:

  (`nb_tte()` only) one of `"informative"` / `"ignore"`.

- value:

  character. Name of the outcome column in `data`.

## Value

An object of class `c("nb_endpoint_<type>", "nb_endpoint")` carrying the
rule and its arguments.

## Direction

For each endpoint type, `direction` says whether a larger value (or
longer time, for TTE) counts as a win for the subject. With the default
convention, a treatment subject "wins" the endpoint when its value,
possibly minus a margin, exceeds the control subject's value. Flip the
default for endpoints where smaller (or shorter) is better (e.g., pain
scores; time to symptom relief).

## Margin

`margin` is a non-negative clinically meaningful difference threshold. A
pair only declares a winner when the directional gap strictly exceeds
`margin`; otherwise the pair ties at this endpoint and falls through to
the next one. `nb_binary()` does not expose `margin` because binary
values admit only two outcomes.

## Censoring

`nb_tte()` accepts a `censor_rule` argument. The default `"informative"`
matches the conventional Pocock/Buyse rule: if one subject is censored
and the other observed, a winner can only be declared in the direction
the censoring is uninformative about (i.e., the censored time is already
past the event time). `"ignore"` drops pairs where either subject is
censored and treats them as ties at this level.

## See also

[`netbenefit_()`](https://zhangh12.github.io/multipleOutcomes/reference/netbenefit_.md)
