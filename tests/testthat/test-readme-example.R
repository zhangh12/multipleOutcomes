# Regression fixture for the README example. Locks in the deterministic
# (asymptotic) pated() output on the bundled `indo` dataset. If a future
# change to GLMAdapter, FisherInformation, or pated's adjustment formula
# shifts any number by more than 1e-10, this test fails immediately and
# the README/help-page numbers (which are generated from the same call)
# stop being trustworthy.

test_that("README pated() example reproduces", {
  data(indo, package = "multipleOutcomes")
  indo <- indo %>% dplyr::mutate(pid = paste0("s-", id))

  fit <- pated(
    glm_(outcome     ~ rx, family = "binomial"),
    glm_(risk        ~ rx, family = "gaussian"),
    glm_(gender      ~ rx, family = "binomial"),
    glm_(sod         ~ rx, family = "binomial"),
    glm_(pep         ~ rx, family = "binomial"),
    glm_(recpanc     ~ rx, family = "binomial"),
    glm_(psphinc     ~ rx, family = "binomial"),
    glm_(precut      ~ rx, family = "binomial"),
    glm_(difcan      ~ rx, family = "binomial"),
    glm_(amp         ~ rx, family = "binomial"),
    glm_(paninj      ~ rx, family = "binomial"),
    glm_(acinar      ~ rx, family = "binomial"),
    glm_(asa81       ~ rx, family = "binomial"),
    glm_(asa         ~ rx, family = "binomial"),
    glm_(prophystent ~ rx, family = "binomial"),
    glm_(therastent  ~ rx, family = "binomial"),
    glm_(pdstent     ~ rx, family = "binomial"),
    data = indo
  )

  ref <- readRDS(
    system.file("testdata", "readme_pated_reference.rds",
                package = "multipleOutcomes")
  )

  # Row order: PATED first, Standard second, then prognostic rows. The fixture
  # captured the same ordering.
  expect_equal(as.character(fit$term),    ref$term)
  expect_equal(as.character(fit$family),  ref$family)
  expect_equal(as.character(fit$method),  ref$method)

  expect_equal(unname(as.numeric(fit$estimate)), ref$estimate, tolerance = 1e-10)
  expect_equal(unname(as.numeric(fit$stderr)),   ref$stderr,   tolerance = 1e-10)
  expect_equal(unname(as.numeric(fit$pvalue)),   ref$pvalue,   tolerance = 1e-10)
  expect_equal(unname(as.numeric(fit$corr)),     ref$corr,     tolerance = 1e-10)
  expect_equal(unname(as.numeric(attr(fit, "Rel. Eff."))),
               ref$rel_eff, tolerance = 1e-10)
})
