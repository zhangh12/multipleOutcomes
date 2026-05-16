# Package index

## Joint estimation and inference

Build a joint asymptotic covariance across heterogeneous outcome models,
and run PATED for prognostic-covariate-adjusted treatment-effect tests.

- [`jointCovariance()`](https://zhangh12.github.io/multipleOutcomes/reference/jointCovariance.md)
  : Fitting Regression Models for Multiple Outcomes and Returning the
  Matrix of Covariance
- [`pated()`](https://zhangh12.github.io/multipleOutcomes/reference/pated.md)
  : Prognostic Variables Assisted Treatment Effect Detection

## Model specifications

Spec constructors describing how each component model is fit. Pass them
as the `...` arguments to
[`jointCovariance()`](https://zhangh12.github.io/multipleOutcomes/reference/jointCovariance.md)
or
[`pated()`](https://zhangh12.github.io/multipleOutcomes/reference/pated.md).

- [`glm_()`](https://zhangh12.github.io/multipleOutcomes/reference/glm_.md)
  : Creating Objects of Generalized Linear Models
- [`coxph_()`](https://zhangh12.github.io/multipleOutcomes/reference/coxph_.md)
  : Creating Objects of Proportional Hazards Regression Model
- [`logrank_()`](https://zhangh12.github.io/multipleOutcomes/reference/logrank_.md)
  : Creating Objects of Logrank Test
- [`gee_()`](https://zhangh12.github.io/multipleOutcomes/reference/gee_.md)
  : Creating Objects of Generalized Estimation Equation Model
- [`mmrm_()`](https://zhangh12.github.io/multipleOutcomes/reference/mmrm_.md)
  : Creating Objects of Mixed Models for Repeated Measures
- [`km_()`](https://zhangh12.github.io/multipleOutcomes/reference/km_.md)
  : Creating Objects of Kaplan-Meier Curve
- [`quantile_()`](https://zhangh12.github.io/multipleOutcomes/reference/quantile_.md)
  : Creating Objects of Group Quantile Differences

## Extracting results

- [`coef(`*`<jointCovariance>`*`)`](https://zhangh12.github.io/multipleOutcomes/reference/coef.jointCovariance.md)
  : Extract Model Coefficients
- [`vcov(`*`<jointCovariance>`*`)`](https://zhangh12.github.io/multipleOutcomes/reference/vcov.jointCovariance.md)
  : Calculate Variance-Covariance Matrix for a Fitted Model Object
- [`summary(`*`<jointCovariance>`*`)`](https://zhangh12.github.io/multipleOutcomes/reference/summary.jointCovariance.md)
  : Object Summaries
- [`print(`*`<summary.jointCovariance>`*`)`](https://zhangh12.github.io/multipleOutcomes/reference/print.summary.jointCovariance.md)
  : Title Summarize an Analysis of Multiple Outcomes.
- [`plot(`*`<pated>`*`)`](https://zhangh12.github.io/multipleOutcomes/reference/plot.pated.md)
  : Plot PATED Analysis Results

## Datasets and simulation

- [`actg`](https://zhangh12.github.io/multipleOutcomes/reference/actg.md)
  : ACTG 320 Clinical Trial Dataset
- [`simulateMoData()`](https://zhangh12.github.io/multipleOutcomes/reference/simulateMoData.md)
  : Generating Data for Simulation and Testing
