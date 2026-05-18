# Changelog

## multipleOutcomes 0.16.4

### Tests

- New `test-inplace-formula.R` verifies that in-place formula
  transformations (`I(z > 4)`, `log(x)`, `scale(x)`, `factor(x)`,
  `Surv(t / c, 1 - cnsr)`, multi-variable Boolean expressions, and LHS
  arithmetic) produce numerically identical PATED output to the
  pre-computed-column equivalents, across
  [`coxph_()`](https://zhangh12.github.io/multipleOutcomes/reference/coxph_.md)
  primaries and
  [`glm_()`](https://zhangh12.github.io/multipleOutcomes/reference/glm_.md)
  prognostic specs.

## multipleOutcomes 0.16

### Breaking changes

- `multipleOutcomes()` and the legacy `*MO()` wrappers (`coxphMO`,
  `glmMO`, `geeMO`, `logrankMO`, `kmMO`, `quantileMO`) are removed. The
  package’s main entry point is now
  [`jointCovariance()`](https://zhangh12.github.io/multipleOutcomes/reference/jointCovariance.md),
  and each component model is specified through a constructor:
  [`glm_()`](https://zhangh12.github.io/multipleOutcomes/reference/glm_.md),
  [`coxph_()`](https://zhangh12.github.io/multipleOutcomes/reference/coxph_.md),
  [`logrank_()`](https://zhangh12.github.io/multipleOutcomes/reference/logrank_.md),
  [`gee_()`](https://zhangh12.github.io/multipleOutcomes/reference/gee_.md),
  [`mmrm_()`](https://zhangh12.github.io/multipleOutcomes/reference/mmrm_.md),
  [`km_()`](https://zhangh12.github.io/multipleOutcomes/reference/km_.md),
  or
  [`quantile_()`](https://zhangh12.github.io/multipleOutcomes/reference/quantile_.md).
  [`pated()`](https://zhangh12.github.io/multipleOutcomes/reference/pated.md)
  accepts the same spec constructors via `...`.
- Datasets supplied to
  [`jointCovariance()`](https://zhangh12.github.io/multipleOutcomes/reference/jointCovariance.md)
  /
  [`pated()`](https://zhangh12.github.io/multipleOutcomes/reference/pated.md)
  must now contain a column `pid` carrying subject identifiers. Records
  with the same `pid` across different data frames refer to the same
  subject.

### New features

- [`mmrm_()`](https://zhangh12.github.io/multipleOutcomes/reference/mmrm_.md)
  adapter for mixed models with repeated measures.
- [`km_()`](https://zhangh12.github.io/multipleOutcomes/reference/km_.md)
  adapter for Kaplan-Meier survival probabilities (bootstrap-only, since
  the empirical S(t) has no closed-form score).
- [`quantile_()`](https://zhangh12.github.io/multipleOutcomes/reference/quantile_.md)
  adapter for between-arm quantile differences (bootstrap-only).
- [`gee_()`](https://zhangh12.github.io/multipleOutcomes/reference/gee_.md)
  adapter exposing a GEE fit’s per-cluster score and Hessian so it slots
  into the joint asymptotic covariance machinery alongside other
  engines.
- `conf_type = "arcsin"` is fully supported in
  [`km_()`](https://zhangh12.github.io/multipleOutcomes/reference/km_.md)
  (previously silently fell back to logit transformation).

### User-experience improvements

- `data_index` defaults to 1 in every spec constructor, and a single
  data frame is auto-wrapped, so
  `jointCovariance(spec1(...), spec2(...), data = my_df)` is the
  shortest valid call.
- Spec constructors validate `data_index` (must be a positive integer
  scalar);
  [`jointCovariance()`](https://zhangh12.github.io/multipleOutcomes/reference/jointCovariance.md)
  adds an upfront bounds check naming the offending spec when
  `data_index` exceeds the number of supplied data frames.
- [`pated()`](https://zhangh12.github.io/multipleOutcomes/reference/pated.md)
  emits a warning when the residual variance goes negative — typically a
  sign that a prognostic variable is collinear with the primary outcome
  or with another prognostic.

### Bug fixes

- `KMAdapter$fit_model()` no longer strips the names off
  `self$estimate`, so `id_map` entries for KM models now carry the
  `time_(strata)_(time)` labels that
  [`pated()`](https://zhangh12.github.io/multipleOutcomes/reference/pated.md)’s
  arm lookup relies on.
- `parseTreatmentVariableFromCall()` now walks the formula AST instead
  of regex-parsing the deparsed call, so formulas with nested
  parentheses (e.g., `y ~ arm + us(visit | pid)` for `mmrm_`) parse
  correctly.
- `fitKMCurve()` uses `survival::summary(..., extend = TRUE)`
  consistently, preventing NAs from leaking into the bootstrap
  covariance matrix when a resample’s stratum has no observations past a
  requested time.
- [`pated()`](https://zhangh12.github.io/multipleOutcomes/reference/pated.md)
  no longer relies on `is.null(family)`, which silently bound
  [`stats::family`](https://rdrr.io/r/stats/family.html) (a function)
  after the `family` argument was removed.

### Documentation, testing, and tooling

- New testthat suite (52 tests / 245 expectations) covering every engine
  through both
  [`jointCovariance()`](https://zhangh12.github.io/multipleOutcomes/reference/jointCovariance.md)
  and
  [`pated()`](https://zhangh12.github.io/multipleOutcomes/reference/pated.md).
  Tests are split into a fast tier (~4 s) and a Monte Carlo tier (~25 s,
  opt-in via `MULTIPLEOUTCOMES_RUN_MC=1`) that validates empirical
  vs. theoretical covariance for single-engine, cross-engine,
  partial-overlap, and kitchen-sink configurations.
- Regression fixture for the README example pinned to 1e-10 (file
  `inst/testdata/readme_pated_reference.rds`).
- GitHub Actions workflows for code coverage (codecov) and pkgdown.
- README example, vignette, and help-page examples migrated to the new
  spec-constructor API.

## multipleOutcomes 0.15

- Initial support for Kaplan-Meier in the new spec-constructor interface
  via
  [`km_()`](https://zhangh12.github.io/multipleOutcomes/reference/km_.md).
- [`pated()`](https://zhangh12.github.io/multipleOutcomes/reference/pated.md)
  extended to handle KM time-stratum parameter vectors, including
  transformed-S(t) point estimates, pointwise confidence intervals, and
  KM-vs-PATED curve comparison plots.

## multipleOutcomes 0.14 and earlier

- See git history.
