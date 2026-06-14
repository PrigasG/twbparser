# ---- helpers -----------------------------------------------------------------

make_dashboard_xml <- function() {
  xml2::read_xml(
    '<workbook>
      <actions>
        <action caption="Filter 1" name="act1" type="filter">
          <source-sheets>
            <source-sheet name="Sheet1"/>
          </source-sheets>
          <target-sheets>
            <target-sheet name="Sheet2"/>
          </target-sheets>
          <run-on type="select"/>
        </action>
        <action caption="Open URL" name="act2" type="url">
          <source-sheets>
            <source-sheet name="Sheet1"/>
          </source-sheets>
          <url value="https://example.com"/>
          <run-on type="menu"/>
        </action>
      </actions>
      <dashboards>
        <dashboard name="Overview">
          <zones>
            <zone id="1" type="layoutV">
              <zone id="2" worksheet="Sheet1" x="0"   y="0"   w="600" h="400"/>
              <zone id="3" type="filter"      x="0"   y="400" w="600" h="50"/>
            </zone>
            <zone id="4" worksheet="Sheet2"   x="620" y="0"   w="300" h="450"/>
          </zones>
        </dashboard>
      </dashboards>
    </workbook>'
  )
}

# ---- twb_dashboard_sheets ----------------------------------------------------

test_that("twb_dashboard_sheets returns expected columns", {
  xml <- make_dashboard_xml()
  out <- twb_dashboard_sheets(xml)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("dashboard", "sheet", "zone_id", "x", "y", "w", "h"),
               ignore.order = TRUE)
})

test_that("twb_dashboard_sheets lists all worksheets in dashboard", {
  xml <- make_dashboard_xml()
  out <- twb_dashboard_sheets(xml)
  expect_setequal(out$sheet, c("Sheet1", "Sheet2"))
})

test_that("twb_dashboard_sheets position attributes are integers", {
  xml <- make_dashboard_xml()
  out <- twb_dashboard_sheets(xml)
  sheet1 <- out[out$sheet == "Sheet1", ]
  expect_equal(sheet1$x, 0L)
  expect_equal(sheet1$y, 0L)
  expect_equal(sheet1$w, 600L)
  expect_equal(sheet1$h, 400L)
})

test_that("twb_dashboard_sheets filters to a named dashboard", {
  xml <- make_dashboard_xml()
  out <- twb_dashboard_sheets(xml, dashboard = "Overview")
  expect_true(all(out$dashboard == "Overview"))
})

test_that("twb_dashboard_sheets returns empty tibble for missing dashboard", {
  xml <- make_dashboard_xml()
  out <- twb_dashboard_sheets(xml, dashboard = "NoSuch")
  expect_equal(nrow(out), 0L)
})

# ---- twb_dashboard_layout ----------------------------------------------------

test_that("twb_dashboard_layout returns expected columns", {
  xml <- make_dashboard_xml()
  out <- twb_dashboard_layout(xml)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("dashboard", "zone_id", "parent_zone_id",
                       "component_type", "target", "layout_type",
                       "x", "y", "w", "h"),
               ignore.order = TRUE)
})

test_that("twb_dashboard_layout captures parent_zone_id correctly", {
  xml <- make_dashboard_xml()
  out <- twb_dashboard_layout(xml)
  # zone 2 and zone 3 are children of zone 1
  z2 <- out[out$zone_id == "2", ]
  z3 <- out[out$zone_id == "3", ]
  expect_equal(z2$parent_zone_id, "1")
  expect_equal(z3$parent_zone_id, "1")
  # zone 1 is a root (parent is <zones>, not a <zone>)
  z1 <- out[out$zone_id == "1", ]
  expect_true(is.na(z1$parent_zone_id))
})

test_that("twb_dashboard_layout classifies component types", {
  xml <- make_dashboard_xml()
  out <- twb_dashboard_layout(xml)
  ws_types <- out$component_type[out$zone_id %in% c("2", "4")]
  expect_true(all(ws_types == "worksheet"))
  filt_type <- out$component_type[out$zone_id == "3"]
  expect_equal(filt_type, "filter")
})

test_that("twb_dashboard_layout marks layout zones as container", {
  xml <- make_dashboard_xml()
  out <- twb_dashboard_layout(xml)
  z1 <- out[out$zone_id == "1", ]
  expect_equal(z1$component_type, "container")
})

# ---- twb_dashboard_actions ---------------------------------------------------

test_that("twb_dashboard_actions returns expected columns", {
  xml <- make_dashboard_xml()
  out <- twb_dashboard_actions(xml)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("action_name", "action_type", "source_sheets",
                       "target_sheet", "run_on", "url"),
               ignore.order = TRUE)
})

test_that("twb_dashboard_actions extracts filter and URL actions", {
  xml <- make_dashboard_xml()
  out <- twb_dashboard_actions(xml)
  expect_true("filter" %in% out$action_type)
  expect_true("url"    %in% out$action_type)
})

test_that("twb_dashboard_actions captures URL value", {
  xml <- make_dashboard_xml()
  out <- twb_dashboard_actions(xml)
  url_row <- out[out$action_type == "url", ]
  expect_equal(url_row$url, "https://example.com")
})

test_that("twb_dashboard_actions captures run_on trigger", {
  xml <- make_dashboard_xml()
  out <- twb_dashboard_actions(xml)
  filter_row <- out[out$action_type == "filter", ]
  expect_equal(filter_row$run_on, "select")
})

test_that("twb_dashboard_actions returns empty tibble when no actions", {
  xml <- xml2::read_xml("<workbook><dashboards/></workbook>")
  out <- twb_dashboard_actions(xml)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
})

test_that("twb_dashboard_filters keeps schema when no dashboards exist", {
  xml <- xml2::read_xml("<workbook><dashboards/></workbook>")
  out <- twb_dashboard_filters(xml)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("dashboard", "zone_id", "zone_type", "field",
                      "presentation", "x", "y", "w", "h"),
               ignore.order = TRUE)
})

# ---- TwbParser integration ---------------------------------------------------

test_that("TwbParser exposes new get_* methods without error", {
  demo <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  if (!nzchar(demo) || !file.exists(demo)) skip("demo .twb not available")

  p <- TwbParser$new(demo)
  expect_s3_class(p$get_sheet_shelves(),   "tbl_df")
  expect_s3_class(p$get_sheet_filters(),   "tbl_df")
  expect_s3_class(p$get_sheet_axes(),      "tbl_df")
  expect_s3_class(p$get_sheet_sorts(),     "tbl_df")
  expect_s3_class(p$get_dashboard_sheets(),"tbl_df")
  expect_s3_class(p$get_dashboard_layout(),"tbl_df")
  expect_s3_class(p$get_dashboard_actions(),"tbl_df")
})
