test_that("no-parens properties work and originals are preserved", {
  demo <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  if (!nzchar(demo) || !file.exists(demo)) skip("demo .twb not available")

  p <- TwbParser$new(demo)

  # summary is an active-binding that returns a printable report
  expect_s3_class(p$summary, "twbparser_report")
  expect_s3_class(p$report, "twbparser_report")
  expect_output(print(p$summary), "TWB PARSER SUMMARY")
  expect_output(print(p$summary), "Worksheet Shelves")

  # read-only properties
  expect_s3_class(p$overview, "tbl_df")
  expect_equal(nrow(p$overview), 1L)

  expect_s3_class(p$pages, "tbl_df")
  expect_s3_class(p$pages_summary, "tbl_df")
  expect_s3_class(p$charts, "tbl_df")
  expect_s3_class(p$colors, "tbl_df")
  expect_s3_class(p$dashboards, "tbl_df")
  expect_s3_class(p$dashboard_filters, "tbl_df")
  expect_named(p$dashboard_filters, c("dashboard", "zone_id", "zone_type",
                                      "field", "presentation", "x", "y", "w", "h"),
               ignore.order = TRUE)

  # convenience snapshot properties (present in normal workbooks)
  if ("fields_tbl" %in% ls(p)) {
    expect_s3_class(p$fields_tbl, "tbl_df")
  }
  if ("datasources" %in% ls(p)) {
    expect_s3_class(p$datasources, "tbl_df")
  }
  if ("parameters_tbl" %in% ls(p)) {
    expect_s3_class(p$parameters_tbl, "tbl_df")
  }
  if ("datasources_all" %in% ls(p)) {
    expect_s3_class(p$datasources_all, "tbl_df")
  }
  expect_s3_class(p$twbx_manifest, "tbl_df")

  # callable methods remain functions
  expect_true(is.function(p$get_fields))
  expect_s3_class(p$get_fields(), "tbl_df")

  expect_true(is.function(p$get_datasources))
  expect_s3_class(p$get_datasources(), "tbl_df")

  expect_true(is.function(p$get_parameters))
  expect_s3_class(p$get_parameters(), "tbl_df")

  expect_type(p$validation, "list")
  expect_named(p$validation, c("ok", "issues"))
})

test_that("safe_call is quiet unless warnings are requested", {
  expect_warning(
    out <- safe_call(stop("recoverable parser miss"), "fallback"),
    NA
  )
  expect_equal(out, "fallback")

  expect_warning(
    safe_call(stop("recoverable parser miss"), "fallback", warn = TRUE),
    "recoverable parser miss"
  )
})
