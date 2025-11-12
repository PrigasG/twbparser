test_that("calculated fields exclude parameters by default", {
  demo <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  if (!nzchar(demo) || !file.exists(demo)) skip("demo .twb not available")

  p <- TwbParser$new(demo)
  calcs <- p$get_calculated_fields()  # default: include_parameters = FALSE
  expect_true(is.data.frame(calcs))
  expect_false("Parameters" %in% unique(calcs$datasource))
})

test_that("calculated fields can include parameters on demand", {
  demo <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  if (!nzchar(demo) || !file.exists(demo)) skip("demo .twb not available")

  p <- TwbParser$new(demo)
  calcs_all <- p$get_calculated_fields(include_parameters = TRUE)
  expect_true(is.data.frame(calcs_all))
  # cannot assert presence (file-dependent), but should not error and may include "Parameters"
})
