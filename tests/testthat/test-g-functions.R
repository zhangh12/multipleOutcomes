# gFunction / gInverseFunction must be exact mutual inverses on the unit
# interval for every documented conf_type, including arcsin.

test_that("g and g-inverse round-trip for every supported conf_type", {
  conf_types <- c("log", "log-log", "plain", "logit", "arcsin")
  x <- seq(0.05, 0.95, by = 0.05)
  for (ct in conf_types) {
    g    <- multipleOutcomes:::gFunction(ct)
    ginv <- multipleOutcomes:::gInverseFunction(ct)
    expect_equal(ginv(g(x)), x, tolerance = 1e-10,
                 label = paste0("ginv(g(x)) for conf_type=", ct))
  }
})

test_that("invalid conf_type is rejected by g/g-inverse", {
  expect_error(multipleOutcomes:::gFunction("oops"))
  expect_error(multipleOutcomes:::gInverseFunction("oops"))
})
