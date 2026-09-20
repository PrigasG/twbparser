test_that("twb_sheet_spec() returns a full viz spec per worksheet", {
  demo <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  if (!nzchar(demo) || !file.exists(demo)) skip("demo .twb not available")

  parser <- TwbParser$new(demo)
  spec <- twb_sheet_spec(parser)

  expect_s3_class(spec, "twb_sheet_spec")
  expect_true(length(spec) >= 1L)
  expect_true("Sheet 1" %in% names(spec))

  s <- spec[["Sheet 1"]]
  for (f in c("sheet", "mark_type", "mark_source", "datasources", "rows",
              "cols", "dimensions", "measures", "encodings", "shelves",
              "tooltip", "filters", "sorts", "axes")) {
    expect_true(f %in% names(s), info = f)
  }

  # the fixture's sheet is a map with explicit mark styling
  expect_equal(s$mark_type, "map")
  expect_equal(s$mark_source, "explicit")

  # encodings tibble shape and content
  expect_s3_class(s$encodings, "tbl_df")
  expect_true(all(c("channel", "field", "aggregation", "used_as") %in% names(s$encodings)))
  expect_true("color" %in% s$encodings$channel)
  expect_true(all(s$encodings$used_as %in% c("dimension", "measure")))

  # shelves tibble shape
  expect_s3_class(s$shelves, "tbl_df")
  expect_true(all(c("shelf", "field", "aggregation", "used_as") %in% names(s$shelves)))

  # tooltip summary is a well-formed list
  expect_true(is.list(s$tooltip))
  expect_true(all(c("has_tooltip", "customized", "text", "fields") %in% names(s$tooltip)))

  # every field used is classified as a dimension or a measure
  used <- unique(c(s$dimensions, s$measures))
  expect_true(length(used) > 0L)
  expect_equal(length(intersect(s$dimensions, s$measures)), 0L)

  # parser method and active binding agree with the function
  expect_identical(names(parser$get_sheet_spec()), names(spec))
  expect_identical(names(parser$sheet_spec), names(spec))
})

test_that("twb_sheet_spec() print method summarizes each sheet", {
  demo <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  if (!nzchar(demo) || !file.exists(demo)) skip("demo .twb not available")

  spec <- twb_sheet_spec(TwbParser$new(demo))
  expect_output(print(spec), "Sheet: Sheet 1")
  expect_output(print(spec), "Mark type: map")

  empty <- twb_sheet_spec(TwbParser$new(demo), sheet = "No such sheet")
  expect_s3_class(empty, "twb_sheet_spec")
  expect_equal(length(empty), 0L)
  expect_output(print(empty), "no worksheets matched")
})

test_that("twb_sheet_spec() validates its inputs", {
  expect_error(twb_sheet_spec(123), "TwbParser or an xml2 document")
  expect_error(twb_sheet_spec(TwbParser$new(
    system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  ), sheet = 123))
})

test_that("twb_dashboard_charts() inventories charts placed on dashboards", {
  demo <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  if (!nzchar(demo) || !file.exists(demo)) skip("demo .twb not available")

  parser <- TwbParser$new(demo)
  charts <- twb_dashboard_charts(parser)

  expect_s3_class(charts, "tbl_df")
  expected <- c("dashboard", "sheet", "zone_id", "mark_type", "mark_source",
                "rows", "cols", "dimensions", "measures", "tooltip_fields",
                "n_tooltip_fields", "has_tooltip", "n_filters",
                "x", "y", "w", "h")
  for (col in expected) {
    expect_true(col %in% names(charts), info = col)
  }
  # the fixture has no dashboards: empty but well-shaped
  expect_equal(nrow(charts), 0L)

  # parser method and active binding agree with the function
  expect_identical(names(parser$get_dashboard_charts()), names(charts))
  expect_identical(names(parser$dashboard_charts), names(charts))
})
