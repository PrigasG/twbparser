test_that("twb_dashboard_size reads fixed and automatic sizing", {
  xml <- xml2::read_xml(
    '<workbook><dashboards>
      <dashboard name="Fixed">
        <size sizing-mode="fixed" maxwidth="1200" maxheight="800"
              minwidth="1200" minheight="800"/>
      </dashboard>
      <dashboard name="Auto">
        <size sizing-mode="automatic"/>
      </dashboard>
      <dashboard name="NoSize"/>
    </dashboards></workbook>'
  )
  sz <- twb_dashboard_size(xml)

  expect_s3_class(sz, "tbl_df")
  expect_equal(nrow(sz), 3L)
  expect_setequal(sz$dashboard, c("Auto", "Fixed", "NoSize"))

  fixed <- sz[sz$dashboard == "Fixed", ]
  expect_equal(fixed$sizing_mode, "fixed")
  expect_equal(fixed$max_width, 1200L)
  expect_equal(fixed$max_height, 800L)

  auto <- sz[sz$dashboard == "Auto", ]
  expect_equal(auto$sizing_mode, "automatic")
  expect_true(is.na(auto$max_width))

  nosize <- sz[sz$dashboard == "NoSize", ]
  expect_true(is.na(nosize$sizing_mode))
})

test_that("twb_dashboard_size returns typed empty tibble when no dashboards", {
  xml <- xml2::read_xml("<workbook></workbook>")
  sz <- twb_dashboard_size(xml)
  expect_s3_class(sz, "tbl_df")
  expect_equal(nrow(sz), 0L)
  expect_true(all(c("dashboard", "sizing_mode", "max_width", "max_height") %in% names(sz)))
})

test_that("twb_formatting extracts style-rule formats with scope and element", {
  xml <- xml2::read_xml(
    '<workbook><worksheets><worksheet name="Sales"><table><style>
      <style-rule element="mark">
        <format attr="number-format" value="$#,##0"/>
        <format attr="mark-labels-cull" value="true"/>
      </style-rule>
      <style-rule element="axis">
        <format attr="font-family" value="Tableau Bold"/>
      </style-rule>
    </style></table></worksheet></worksheets></workbook>'
  )
  fmt <- twb_formatting(xml)

  expect_s3_class(fmt, "tbl_df")
  expect_equal(nrow(fmt), 3L)
  expect_true(all(fmt$scope == "worksheet"))
  expect_true(all(fmt$container == "Sales"))
  expect_true("number-format" %in% fmt$attr)
  expect_equal(fmt$value[fmt$attr == "number-format"], "$#,##0")
  expect_true("axis" %in% fmt$element)

  # scope filter
  expect_equal(nrow(twb_formatting(xml, scope = "dashboard")), 0L)
})

test_that("twb_formatting extracts style-rule encoding palette maps", {
  xml <- xml2::read_xml(
    '<workbook><datasources><datasource name="ds"><style>
      <style-rule element="mark">
        <encoding attr="color" field="[none:Status:nk]" type="palette">
          <map to="#4e79a7"><bucket>&quot;available&quot;</bucket></map>
          <map to="#c2bfc0"><bucket>&quot;missing&quot;</bucket></map>
        </encoding>
      </style-rule>
    </style></datasource></datasources></workbook>'
  )
  fmt <- twb_formatting(xml)

  expect_equal(nrow(fmt), 2L)
  expect_true(all(fmt$attr == "encoding-color"))
  expect_setequal(fmt$value, c("#4e79a7", "#c2bfc0"))
  expect_setequal(fmt$target, c('"available"', '"missing"'))
  expect_true(all(fmt$field == "[none:Status:nk]"))
})

test_that("twb_formatting matches the bundled workbook palette schema", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if(!nzchar(twb))
  xml <- xml2::read_xml(twb)
  fmt <- twb_formatting(xml)

  expect_true("washout" %in% fmt$attr)
  expect_true("encoding-color" %in% fmt$attr)
  expect_true("#4e79a7" %in% fmt$value)
  expect_true("#c2bfc0" %in% fmt$value)
})

test_that("twb_tooltips returns plain text and flags customisation", {
  xml <- xml2::read_xml(
    '<workbook><worksheets>
      <worksheet name="WithTip">
        <customized-tooltip><formatted-text>
          <run>Sales: </run><run>value</run>
        </formatted-text></customized-tooltip>
      </worksheet>
      <worksheet name="NoTip"/>
    </worksheets></workbook>'
  )
  tt <- twb_tooltips(xml)

  expect_s3_class(tt, "tbl_df")
  expect_equal(nrow(tt), 2L)

  withtip <- tt[tt$sheet == "WithTip", ]
  expect_true(withtip$is_customized)
  expect_match(withtip$tooltip_text, "Sales:")

  notip <- tt[tt$sheet == "NoTip", ]
  expect_false(notip$is_customized)
  expect_true(is.na(notip$tooltip_text))
})

test_that("parser getters expose the fidelity extractors", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if(!nzchar(twb))
  p <- TwbParser$new(twb)
  expect_s3_class(p$get_dashboard_size(), "tbl_df")
  expect_s3_class(p$get_formatting(), "tbl_df")
  expect_s3_class(p$get_tooltips(), "tbl_df")
})
