test_that("twb_unused_fields() finds exactly the unused fields in the fixture", {
  twb <- system.file("extdata", "rebuild_kit.twb", package = "twbparser")
  if (!nzchar(twb) || !file.exists(twb)) skip("rebuild_kit.twb not available")

  xml <- xml2::read_xml(twb)
  unused <- twb_unused_fields(xml)

  expect_s3_class(unused, "tbl_df")
  expect_true(all(c("datasource", "field_type", "name", "tableau_internal_name",
                    "datatype", "role", "is_hidden") %in% names(unused)))

  # exactly the three planted unused fields, correctly typed
  expect_equal(nrow(unused), 3L)
  expect_setequal(unused$name, c("Unused Field", "Unused Calc", "Unused Param"))
  got_type <- unused$field_type[match(
    c("Unused Field", "Unused Calc", "Unused Param"), unused$name
  )]
  expect_equal(got_type, c("raw", "calculated", "parameter"))

  # fields referenced on shelves, filters, tooltips, or formulas are not reported
  for (nm in c("Region", "Sales", "Profit", "Order ID", "Profit Ratio",
               "Adjusted Ratio", "Top N", "Cycle A", "Cycle B")) {
    expect_false(nm %in% unused$name, info = nm)
  }

  # parser method and active binding agree with the exported function
  parser <- TwbParser$new(twb)
  expect_identical(parser$get_unused_fields()$name, unused$name)
  expect_identical(parser$unused_fields$name, unused$name)
})

test_that("twb_calc_build_order() orders calculations and flags cycles", {
  twb <- system.file("extdata", "rebuild_kit.twb", package = "twbparser")
  if (!nzchar(twb) || !file.exists(twb)) skip("rebuild_kit.twb not available")

  xml <- xml2::read_xml(twb)
  bo <- suppressWarnings(twb_calc_build_order(xml))

  expect_s3_class(bo, "tbl_df")
  expect_true(all(c("build_order", "datasource", "name", "tableau_internal_name",
                    "formula", "depends_on", "n_calc_deps", "is_cyclic") %in% names(bo)))
  expect_equal(nrow(bo), 5L)

  # dependency chain respected: Profit Ratio is built before Adjusted Ratio
  pr <- bo$build_order[bo$name == "Profit Ratio"]
  ar <- bo$build_order[bo$name == "Adjusted Ratio"]
  expect_true(pr < ar)
  expect_equal(bo$depends_on[bo$name == "Adjusted Ratio"], "Profit Ratio")
  expect_equal(bo$n_calc_deps[bo$name == "Adjusted Ratio"], 1L)
  expect_true(is.na(bo$depends_on[bo$name == "Profit Ratio"]))

  # the circular pair cannot be ordered and is flagged
  cyc <- bo[bo$is_cyclic, ]
  expect_setequal(cyc$name, c("Cycle A", "Cycle B"))
  expect_true(all(is.na(cyc$build_order)))
  expect_equal(bo$depends_on[bo$name == "Cycle A"], "Cycle B")

  # a warning names the cyclic fields
  expect_warning(twb_calc_build_order(xml), "Circular")

  # parser method and active binding agree with the exported function
  parser <- TwbParser$new(twb)
  expect_identical(suppressWarnings(parser$get_calc_build_order())$name, bo$name)
  expect_identical(suppressWarnings(parser$calc_build_order)$name, bo$name)
})

test_that("twb_parameter_usage() maps parameter consumption", {
  twb <- system.file("extdata", "rebuild_kit.twb", package = "twbparser")
  if (!nzchar(twb) || !file.exists(twb)) skip("rebuild_kit.twb not available")

  xml <- xml2::read_xml(twb)
  pu <- twb_parameter_usage(xml)

  expect_s3_class(pu, "tbl_df")
  expect_true(all(c("parameter", "datasource", "datatype",
                    "current_value", "context", "location") %in% names(pu)))

  # "Top N" is consumed by a formula and a worksheet filter
  topn <- pu[pu$parameter == "Top N", ]
  expect_true(nrow(topn) >= 2L)
  expect_true("formula" %in% topn$context)
  expect_true("filter" %in% topn$context)
  expect_true("Adjusted Ratio" %in% topn$location[topn$context == "formula"])
  expect_true("Profit Detail" %in% topn$location[topn$context == "filter"])
  expect_equal(topn$current_value[topn$context == "formula"], "5")

  # parameters with no usages do not appear (see twb_unused_fields())
  expect_false("Unused Param" %in% pu$parameter)

  # parser method and active binding agree with the exported function
  parser <- TwbParser$new(twb)
  expect_identical(nrow(parser$get_parameter_usage()), nrow(pu))
  expect_identical(nrow(parser$parameter_usage), nrow(pu))
})

test_that("rebuild kit handles workbooks without parameters or chains", {
  demo <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  if (!nzchar(demo) || !file.exists(demo)) skip("demo .twb not available")

  xml <- xml2::read_xml(demo)

  pu <- twb_parameter_usage(xml)
  expect_s3_class(pu, "tbl_df")
  expect_equal(nrow(pu), 0L)
  expect_true(all(c("parameter", "datasource", "datatype",
                    "current_value", "context", "location") %in% names(pu)))

  bo <- twb_calc_build_order(xml)
  expect_equal(nrow(bo), 1L)
  expect_equal(bo$build_order, 1L)
  expect_false(bo$is_cyclic)

  unused <- twb_unused_fields(xml)
  expect_s3_class(unused, "tbl_df")
  expect_true(all(c("datasource", "field_type", "name") %in% names(unused)))
  # fields on the sheet / in the calc formula are not reported as unused
  expect_false("counts" %in% unused$name)
  expect_false("no data" %in% unused$name)
  expect_false("Geometry" %in% unused$name)
})
