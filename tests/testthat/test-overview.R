test_that("overview returns one-row tibble", {
  demo <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  if (!nzchar(demo) || !file.exists(demo)) skip("demo .twb not available")

  p <- TwbParser$new(demo)
  ov <- p$get_overview()
  expect_s3_class(ov, "tbl_df")
  expect_equal(nrow(ov), 1L)
  expect_true(all(c("file", "datasources", "parameters", "relationships",
                    "calculated_fields", "raw_fields", "inferred_relationships",
                    "dashboards", "total_filters") %in% names(ov)))
})
