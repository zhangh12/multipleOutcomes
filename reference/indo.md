# Rectal Indomethacin for Prevention of Post-ERCP Pancreatitis

Patient-level data from a multi-center, randomized, double-blind,
placebo-controlled 2-arm trial (n = 602) of rectal indomethacin (100 mg)
versus placebo to prevent post-ERCP pancreatitis in high-risk patients,
as reported by Elmunzer, Higgins, et al. (2012) in the *New England
Journal of Medicine*.

This dataset was originally collected, cleaned, reformatted, and
released for public teaching and research use by Dr. Peter D. R. Higgins
in the medicaldata R package as `indo_rct`. The version shipped here is
redistributed in support of the worked examples in this package. The
variable definitions below follow medicaldata; note that a few columns
are stored as numeric (rather than factor) in this copy. Users who need
the authoritative copy or accompanying documentation should consult
medicaldata.

## Format

A data frame with 602 observations on the following 33 variables:

- `id`:

  Subject identifier (numeric); leading digit indicates center. Range
  1001–4003.

- `site`:

  Study site (factor, 4 levels): `1_UM` = University of Michigan, `2_IU`
  = Indiana University, `3_UK` = University of Kentucky, `4_Case` = Case
  Western Reserve University.

- `age`:

  Age in years (numeric), range 19–90.

- `risk`:

  Risk score for post-ERCP pancreatitis (numeric), range 1–5.5.

- `gender`:

  Sex (factor): `1_female`, `2_male`.

- `outcome`:

  Primary outcome: post-ERCP pancreatitis (numeric, 1 = yes, 0 = no).

- `sod`:

  Sphincter of Oddi dysfunction present (factor): `0_no`, `1_yes`.

- `pep`:

  History of prior post-ERCP pancreatitis (factor): `0_no`, `1_yes`.

- `recpanc`:

  History of recurrent pancreatitis (factor): `0_no`, `1_yes`.

- `psphinc`:

  Pancreatic sphincterotomy performed (factor): `0_no`, `1_yes`.

- `precut`:

  Sphincter pre-cut needed to enter papilla (factor): `0_no`, `1_yes`.

- `difcan`:

  Cannulation of papilla was difficult (factor): `0_no`, `1_yes`.

- `pneudil`:

  Pneumatic dilation of papilla performed (factor): `0_no`, `1_yes`.

- `amp`:

  Ampullectomy performed (factor): `0_no`, `1_yes`.

- `paninj`:

  Contrast injected into pancreas (factor): `0_no`, `1_yes`.

- `acinar`:

  Pancreas appeared to have acinarization on imaging (factor): `0_no`,
  `1_yes`.

- `brush`:

  Brushings taken from pancreatic duct (factor): `0_no`, `1_yes`.

- `asa81`:

  Aspirin used at 81 mg per day (factor with 3 levels): `0_no`, `1_yes`,
  and a third level retained from the source coding.

- `asa325`:

  Aspirin used at 325 mg per day (factor with 3 levels): `0_no`,
  `1_yes`, and a third level retained from the source coding.

- `asa`:

  Aspirin used at any dose (factor with 3 levels): `0_no`, `1_yes`, and
  a third level retained from the source coding.

- `prophystent`:

  Pancreatic duct stent placed per endoscopist judgment (factor):
  `0_no`, `1_yes`.

- `therastent`:

  Pancreatic duct stent placed to treat narrowing (factor): `0_no`,
  `1_yes`.

- `pdstent`:

  Pancreatic duct stent placed for any reason (factor): `0_no`, `1_yes`.

- `sodsom`:

  Sphincter of Oddi manometry performed (factor): `0_no`, `1_yes`.

- `bsphinc`:

  Biliary sphincterotomy performed (factor): `0_no`, `1_yes`.

- `bstent`:

  Biliary stent placed to relieve obstruction (factor): `0_no`, `1_yes`.

- `chole`:

  Choledocholithiasis present (factor): `0_no`, `1_yes`.

- `pbmal`:

  Biliary duct or pancreatic malignancy found (factor): `0_no`, `1_yes`.

- `train`:

  Trainee participated in ERCP (factor): `0_no`, `1_yes`.

- `status`:

  Patient status (factor): `0_inpatient`, `1_outpatient`.

- `type`:

  Sphincter of Oddi dysfunction type (factor): `0_no SOD`, `1_type 1`,
  `2_type 2`, `3_type 3`.

- `rx`:

  Treatment assignment (numeric, 1 = indomethacin, 0 = placebo).

- `bleed`:

  Reportable gastrointestinal bleeding (numeric, coded 1 = no, 2 = yes;
  `NA` when not assessed).

## Source

Higgins, P. D. R. medicaldata: Data Package for Medical Datasets. R
package, dataset `indo_rct`.
<https://CRAN.R-project.org/package=medicaldata>

## References

Elmunzer BJ, Higgins PDR, Saini SD, et al. A randomized trial of rectal
indomethacin to prevent post-ERCP pancreatitis. *New England Journal of
Medicine* 2012; 366(15):1414–1422.
[doi:10.1056/NEJMoa1111103](https://doi.org/10.1056/NEJMoa1111103)

## Examples

``` r
data(indo)
```
