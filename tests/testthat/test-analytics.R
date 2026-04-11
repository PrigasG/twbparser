## Tests for Phase 4: twb_calc_complexity, twb_field_usage, twb_replication_brief

# ---- helpers -----------------------------------------------------------------

.make_calc_xml <- function(...) {
  calcs <- list(...)
  col_nodes <- vapply(calcs, function(c) {
    caption_attr <- if (!is.null(c$caption)) sprintf(' caption="%s"', c$caption) else ""
    sprintf(
      '<column name="%s"%s datatype="%s" role="%s">
         <calculation class="tableau" formula="%s"/>
       </column>',
      c$name, caption_attr,
      c$datatype %||% "string", c$role %||% "measure",
      gsub('"', "&quot;", c$formula %||% "")
    )
  }, character(1L))
  xml2::read_xml(sprintf(
    '<workbook>
       <datasources>
         <datasource name="ds1">%s</datasource>
       </datasources>
       <worksheets/>
     </workbook>',
    paste(col_nodes, collapse = "\n")
  ))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- twb_calc_complexity: columns -------------------------------------------

test_that("twb_calc_complexity returns expected columns", {
  xml <- .make_calc_xml(
    list(name = "[Sales]", formula = "SUM([Price])"),
    list(name = "[Count]", formula = "COUNT([Orders])")
  )
  out <- twb_calc_complexity(xml)
  expected_cols <- c("datasource", "name", "tableau_internal_name",
                     "datatype", "role", "calc_type", "lod_type",
                     "is_table_calc", "dep_depth", "n_deps", "formula")
  expect_true(all(expected_cols %in% names(out)))
})

# ---- twb_calc_complexity: LOD detection -------------------------------------

test_that("twb_calc_complexity detects FIXED LOD", {
  xml <- .make_calc_xml(
    list(name = "[FixedSales]", formula = "{ FIXED [Category] : SUM([Sales]) }")
  )
  out <- twb_calc_complexity(xml)
  expect_equal(out$calc_type, "lod")
  expect_equal(out$lod_type, "fixed")
})

test_that("twb_calc_complexity detects INCLUDE LOD", {
  xml <- .make_calc_xml(
    list(name = "[IncSales]", formula = "{ INCLUDE [Category] : SUM([Sales]) }")
  )
  out <- twb_calc_complexity(xml)
  expect_equal(out$calc_type, "lod")
  expect_equal(out$lod_type, "include")
})

test_that("twb_calc_complexity detects EXCLUDE LOD", {
  xml <- .make_calc_xml(
    list(name = "[ExclSales]", formula = "{ EXCLUDE [Category] : SUM([Sales]) }")
  )
  out <- twb_calc_complexity(xml)
  expect_equal(out$calc_type, "lod")
  expect_equal(out$lod_type, "exclude")
})

# ---- twb_calc_complexity: table_calc detection ------------------------------

test_that("twb_calc_complexity detects table calcs", {
  xml <- .make_calc_xml(
    list(name = "[Running]", formula = "RUNNING_SUM(SUM([Sales]))")
  )
  out <- twb_calc_complexity(xml)
  expect_equal(out$calc_type, "table_calc")
  expect_true(out$is_table_calc)
})

# ---- twb_calc_complexity: aggregate detection -------------------------------

test_that("twb_calc_complexity classifies SUM as aggregate", {
  xml <- .make_calc_xml(
    list(name = "[TotalSales]", formula = "SUM([Sales])")
  )
  out <- twb_calc_complexity(xml)
  expect_equal(out$calc_type, "aggregate")
  expect_true(is.na(out$lod_type))
})

test_that("twb_calc_complexity classifies COUNTD as aggregate", {
  xml <- .make_calc_xml(
    list(name = "[UniqueCustomers]", formula = "COUNTD([Customer ID])")
  )
  out <- twb_calc_complexity(xml)
  expect_equal(out$calc_type, "aggregate")
})

# ---- twb_calc_complexity: raw classification --------------------------------

test_that("twb_calc_complexity classifies string concat as raw", {
  xml <- .make_calc_xml(
    list(name = "[FullName]", formula = "[First] + &quot; &quot; + [Last]",
         datatype = "string")
  )
  out <- twb_calc_complexity(xml)
  expect_equal(out$calc_type, "raw")
  expect_true(is.na(out$lod_type))
})

# ---- twb_calc_complexity: LOD wins over table_calc precedence ---------------

test_that("LOD wins over table_calc when both patterns match", {
  # Contrived formula with both patterns
  xml <- .make_calc_xml(
    list(name = "[Weird]",
         formula = "{ FIXED [Cat] : RUNNING_SUM(SUM([Sales])) }")
  )
  out <- twb_calc_complexity(xml)
  expect_equal(out$calc_type, "lod")
})

# ---- twb_calc_complexity: dep_depth -----------------------------------------

test_that("dep_depth is 0 for direct formula with no calc deps", {
  xml <- .make_calc_xml(
    list(name = "[Revenue]", formula = "SUM([Sales])")
  )
  out <- twb_calc_complexity(xml)
  expect_equal(out$dep_depth, 0L)
})

test_that("dep_depth is 1 for calc depending on another calc", {
  xml <- .make_calc_xml(
    list(name = "[Revenue]",  formula = "SUM([Sales])"),
    list(name = "[Revenue2]", formula = "[Revenue] * 2")
  )
  out <- twb_calc_complexity(xml)
  revenue2 <- out[out$name == "Revenue2", ]
  expect_equal(revenue2$dep_depth, 1L)
})

test_that("dep_depth is 2 for three-level calc chain", {
  xml <- .make_calc_xml(
    list(name = "[A]", formula = "SUM([x])"),
    list(name = "[B]", formula = "[A] * 2"),
    list(name = "[C]", formula = "[B] + 1")
  )
  out <- twb_calc_complexity(xml)
  cc  <- out[out$name == "C", ]
  expect_equal(cc$dep_depth, 2L)
})

# ---- twb_calc_complexity: empty workbook ------------------------------------

test_that("twb_calc_complexity returns typed empty tibble for no calcs", {
  xml <- xml2::read_xml(
    '<workbook><datasources/><worksheets/></workbook>'
  )
  out <- twb_calc_complexity(xml)
  expect_equal(nrow(out), 0L)
  expect_true("calc_type" %in% names(out))
  expect_true("dep_depth" %in% names(out))
})

# ---- twb_field_usage: basic columns -----------------------------------------

test_that("twb_field_usage returns expected columns (long form)", {
  xml <- xml2::read_xml(
    '<workbook>
       <worksheets>
         <worksheet name="Sheet1">
           <table>
             <view>
               <datasources>
                 <datasource name="ds1" caption="ds1"/>
               </datasources>
               <columns>
                 <column worksheet="Sheet1" name="[none:Sales:qk]"
                         datasource-name="ds1">
                   <encodings>
                     <rows column="[ds1].[none:Sales:qk]"/>
                   </encodings>
                 </column>
               </columns>
             </view>
             <rows>[ds1].[none:Sales:qk]</rows>
             <cols></cols>
           </table>
         </worksheet>
       </worksheets>
     </workbook>'
  )
  out <- twb_field_usage(xml)
  expect_true(all(c("field_clean", "datasource", "sheet",
                    "context", "n_appearances") %in% names(out)))
})

test_that("twb_field_usage wide=TRUE returns one row per field", {
  xml <- xml2::read_xml(
    '<workbook>
       <worksheets>
         <worksheet name="Sheet1">
           <table>
             <rows>[ds1].[none:Revenue:qk]</rows>
             <cols></cols>
           </table>
         </worksheet>
         <worksheet name="Sheet2">
           <table>
             <rows>[ds1].[none:Revenue:qk]</rows>
             <cols></cols>
           </table>
         </worksheet>
       </worksheets>
     </workbook>'
  )
  out_long <- twb_field_usage(xml, wide = FALSE)
  out_wide <- twb_field_usage(xml, wide = TRUE)
  # Wide has no sheet / context / n_appearances columns
  expect_false("context" %in% names(out_wide))
  expect_false("n_appearances" %in% names(out_wide))
  # Each unique (field_clean, datasource) becomes one row
  expect_equal(nrow(out_wide), nrow(dplyr::distinct(out_long, field_clean, datasource)))
})

test_that("twb_field_usage include_shelves=FALSE shows only filters", {
  xml <- xml2::read_xml(
    '<workbook>
       <worksheets>
         <worksheet name="Sheet1">
           <table>
             <rows>[ds1].[none:Revenue:qk]</rows>
             <filters>
               <filter class="categorical" column="[ds1].[none:Region:nk]"/>
             </filters>
           </table>
         </worksheet>
       </worksheets>
     </workbook>'
  )
  out <- twb_field_usage(xml, include_shelves = FALSE)
  if (nrow(out) > 0L)
    expect_true(all(out$context == "filter"))
})

test_that("twb_field_usage returns empty tibble when both FALSE", {
  xml <- xml2::read_xml('<workbook><worksheets/></workbook>')
  expect_message(
    out <- twb_field_usage(xml, include_filters = FALSE, include_shelves = FALSE),
    "both FALSE"
  )
  expect_equal(nrow(out), 0L)
})

test_that("twb_field_usage empty workbook returns empty tibble", {
  xml <- xml2::read_xml('<workbook><worksheets/></workbook>')
  out <- twb_field_usage(xml)
  expect_equal(nrow(out), 0L)
  expect_true(all(c("field_clean", "datasource", "sheet", "context",
                    "n_appearances") %in% names(out)))
})

# ---- twb_replication_brief: list structure ----------------------------------

test_that("twb_replication_brief returns a list with expected elements", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")
  xml  <- xml2::read_xml(twb)
  brief <- twb_replication_brief(xml)
  expect_type(brief, "list")
  expected_keys <- c("meta", "datasources", "parameters", "custom_sql",
                     "calculated_fields", "field_usage", "filters", "sorts",
                     "chart_types", "dashboard_layout", "actions")
  expect_true(all(expected_keys %in% names(brief)))
})

test_that("twb_replication_brief meta has correct columns and types", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")
  xml  <- xml2::read_xml(twb)
  meta <- twb_replication_brief(xml)$meta
  expect_equal(nrow(meta), 1L)
  expect_true(is.character(meta$workbook_file))
  expect_true(is.integer(meta$n_datasources))
  expect_true(is.integer(meta$n_worksheets))
  expect_true(is.character(meta$generated_at))
})

test_that("twb_replication_brief format=text returns character(1)", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")
  xml  <- xml2::read_xml(twb)
  txt  <- twb_replication_brief(xml, format = "text")
  expect_type(txt, "character")
  expect_length(txt, 1L)
  expect_match(txt, "REPLICATION BRIEF")
  expect_match(txt, "CALCULATED FIELDS")
})

test_that("twb_replication_brief include_sql=FALSE sets custom_sql to NULL", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")
  xml   <- xml2::read_xml(twb)
  brief <- twb_replication_brief(xml, include_sql = FALSE)
  expect_null(brief$custom_sql)
})

test_that("twb_replication_brief include_formulas=TRUE adds formula_pretty", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")
  xml   <- xml2::read_xml(twb)
  brief <- twb_replication_brief(xml, include_formulas = TRUE)
  cf    <- brief$calculated_fields
  if (nrow(cf) > 0L)
    expect_true("formula_pretty" %in% names(cf))
})

# ---- TwbParser integration --------------------------------------------------

test_that("TwbParser exposes get_calc_complexity and active binding", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")
  parser <- TwbParser$new(twb)
  method_out  <- parser$get_calc_complexity()
  binding_out <- parser$calc_complexity
  expect_s3_class(method_out, "tbl_df")
  expect_identical(binding_out, method_out)
})

test_that("TwbParser exposes get_field_usage and active binding", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")
  parser <- TwbParser$new(twb)
  method_out  <- parser$get_field_usage()
  binding_out <- parser$field_usage
  expect_s3_class(method_out, "tbl_df")
  expect_identical(binding_out, method_out)
})

test_that("TwbParser get_replication_brief returns a list", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")
  parser <- TwbParser$new(twb)
  brief  <- parser$get_replication_brief()
  expect_type(brief, "list")
  expect_true("meta" %in% names(brief))
  # workbook_file should be the basename of the .twb path
  expect_equal(brief$meta$workbook_file, basename(twb))
})
