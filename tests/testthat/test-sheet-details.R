# ---- helpers -----------------------------------------------------------------

make_sheet_xml <- function() {
  xml2::read_xml(
    '<workbook>
      <worksheets>
        <worksheet name="Sales">
          <table>
            <view>
              <filter class="categorical" column="[ds].[Category]">
                <groupfilter function="union">
                  <groupfilter function="member" member="[Furniture]"/>
                  <groupfilter function="member" member="[Technology]"/>
                </groupfilter>
              </filter>
              <filter class="range" column="[ds].[Sales]">
                <min>0</min>
                <max>9999</max>
              </filter>
              <sort class="sum" column="[ds].[Sales]" direction="descending"/>
            </view>
            <rows>[ds].[Category]</rows>
            <cols>[ds].[Sales]</cols>
            <style>
              <style-rule element="axis">
                <format attr="reverse" value="false"/>
                <format attr="scale-include-zero" value="true"/>
              </style-rule>
            </style>
            <panes>
              <pane>
                <mark class="Bar"/>
                <encodings>
                  <color column="[ds].[Category]"/>
                  <size  column="[ds].[Sales]"/>
                  <label column="[ds].[Sales]"/>
                </encodings>
              </pane>
            </panes>
          </table>
        </worksheet>
      </worksheets>
    </workbook>'
  )
}

# ---- twb_sheet_shelves -------------------------------------------------------

test_that("twb_sheet_shelves returns tibble with expected columns", {
  xml  <- make_sheet_xml()
  out  <- twb_sheet_shelves(xml)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("sheet", "shelf", "field_ref", "field_instance",
                       "field_clean", "datasource", "aggregation"),
               ignore.order = TRUE)
})

test_that("twb_sheet_shelves detects rows, cols and encoding shelves", {
  xml  <- make_sheet_xml()
  out  <- twb_sheet_shelves(xml)
  expect_true("rows"  %in% out$shelf)
  expect_true("cols"  %in% out$shelf)
  expect_true("color" %in% out$shelf)
  expect_true("size"  %in% out$shelf)
  expect_true("label" %in% out$shelf)
})

test_that("twb_sheet_shelves filters to a single sheet", {
  xml  <- make_sheet_xml()
  out  <- twb_sheet_shelves(xml, sheet = "Sales")
  expect_true(all(out$sheet == "Sales"))

  # non-existent sheet returns empty
  empty <- twb_sheet_shelves(xml, sheet = "NoSuchSheet")
  expect_equal(nrow(empty), 0L)
})

test_that("twb_sheet_shelves returns empty tibble for empty workbook", {
  xml <- xml2::read_xml("<workbook><worksheets/></workbook>")
  out <- twb_sheet_shelves(xml)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
})

# ---- twb_sheet_filters -------------------------------------------------------

test_that("twb_sheet_filters returns expected columns", {
  xml <- make_sheet_xml()
  out <- twb_sheet_filters(xml)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("sheet", "field_ref", "field_clean", "datasource",
                       "filter_class", "include_mode", "members",
                       "range_min", "range_max"),
               ignore.order = TRUE)
})

test_that("twb_sheet_filters extracts categorical members", {
  xml <- make_sheet_xml()
  out <- twb_sheet_filters(xml)
  cat_row <- out[out$filter_class == "categorical", ]
  expect_equal(nrow(cat_row), 1L)
  expect_true(grepl("Furniture",   cat_row$members, fixed = TRUE))
  expect_true(grepl("Technology",  cat_row$members, fixed = TRUE))
  expect_equal(cat_row$include_mode, "include")
})

test_that("twb_sheet_filters extracts range min/max", {
  xml <- make_sheet_xml()
  out <- twb_sheet_filters(xml)
  rng_row <- out[out$filter_class == "range", ]
  expect_equal(nrow(rng_row), 1L)
  expect_equal(rng_row$range_min, "0")
  expect_equal(rng_row$range_max, "9999")
})

test_that("twb_sheet_filters returns empty tibble when no filters present", {
  xml <- xml2::read_xml(
    '<workbook><worksheets>
       <worksheet name="A"><table><view/></table></worksheet>
     </worksheets></workbook>'
  )
  out <- twb_sheet_filters(xml)
  expect_equal(nrow(out), 0L)
})

# ---- twb_sheet_axes ----------------------------------------------------------

test_that("twb_sheet_axes returns expected columns", {
  xml <- make_sheet_xml()
  out <- twb_sheet_axes(xml)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("sheet", "axis", "field_ref", "field_clean",
                       "scale_type", "reversed", "include_zero"),
               ignore.order = TRUE)
})

test_that("twb_sheet_axes parses reverse and include-zero flags", {
  xml <- make_sheet_xml()
  out <- twb_sheet_axes(xml)
  expect_equal(nrow(out), 1L)
  expect_false(out$reversed[[1L]])
  expect_true(out$include_zero[[1L]])
})

test_that("twb_sheet_axes returns empty tibble when no axis rules present", {
  xml <- xml2::read_xml(
    '<workbook><worksheets>
       <worksheet name="A"><table/></worksheet>
     </worksheets></workbook>'
  )
  out <- twb_sheet_axes(xml)
  expect_equal(nrow(out), 0L)
})

# ---- twb_sheet_sorts ---------------------------------------------------------

test_that("twb_sheet_sorts returns expected columns", {
  xml <- make_sheet_xml()
  out <- twb_sheet_sorts(xml)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("sheet", "field_ref", "field_clean", "datasource",
                       "sort_order", "sort_by"),
               ignore.order = TRUE)
})

test_that("twb_sheet_sorts extracts sort direction and method", {
  xml <- make_sheet_xml()
  out <- twb_sheet_sorts(xml)
  expect_equal(nrow(out), 1L)
  expect_equal(out$sort_order[[1L]], "descending")
  expect_equal(out$sort_by[[1L]],   "field")
})

test_that("twb_sheet_sorts returns empty tibble when no sorts present", {
  xml <- xml2::read_xml(
    '<workbook><worksheets>
       <worksheet name="A"><table><view/></table></worksheet>
     </worksheets></workbook>'
  )
  out <- twb_sheet_sorts(xml)
  expect_equal(nrow(out), 0L)
})
