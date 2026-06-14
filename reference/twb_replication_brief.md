# Replication brief for a Tableau workbook or dashboard

Assembles all extracted intelligence — datasources, parameters,
calculated fields with complexity classifications, field usage, filters,
sorts, chart types, dashboard layout, and actions — into a single named
list (or formatted text) ready for use when porting to another
visualisation tool.

## Usage

``` r
twb_replication_brief(
  x,
  dashboard = NULL,
  include_sql = TRUE,
  include_formulas = TRUE,
  format = c("list", "text")
)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- dashboard:

  Optional character scalar. When supplied, sheet-level sections
  (filters, sorts, chart types, field usage, layout) are scoped to the
  sheets that belong to this dashboard.

- include_sql:

  Logical; include custom SQL blocks in `$custom_sql`. Default `TRUE`.

- include_formulas:

  Logical; when `TRUE`, a `formula_pretty` column is added to
  `$calculated_fields`. Default `TRUE`.

- format:

  Either `"list"` (default) to return a named R list, or `"text"` to
  return a single formatted character string suitable for printing or
  writing to a file.

## Value

**`format = "list"`**: a named list with elements:

- meta:

  1-row tibble: file name, counts, generation timestamp.

- datasources:

  Datasource connection details.

- parameters:

  Parameter fields with current values.

- custom_sql:

  Custom SQL blocks, or `NULL` if `include_sql = FALSE`.

- calculated_fields:

  Tibble from
  [`twb_calc_complexity()`](https://prigasg.github.io/twbparser/reference/twb_calc_complexity.md),
  optionally with a `formula_pretty` column.

- field_usage:

  Tibble from
  [`twb_field_usage()`](https://prigasg.github.io/twbparser/reference/twb_field_usage.md).

- filters:

  Worksheet filters (scoped to `dashboard` if given).

- sorts:

  Worksheet sorts (scoped to `dashboard` if given).

- chart_types:

  Mark types per worksheet.

- dashboard_layout:

  Zone positions from
  [`twb_dashboard_sheets()`](https://prigasg.github.io/twbparser/reference/twb_dashboard_sheets.md).

- actions:

  Dashboard actions from
  [`twb_dashboard_actions()`](https://prigasg.github.io/twbparser/reference/twb_dashboard_actions.md).

**`format = "text"`**: a single Markdown `character(1)` with section
headers, compact tables, and fenced formula/SQL blocks.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)
brief <- twb_replication_brief(xml)
names(brief)
#>  [1] "meta"              "datasources"       "parameters"       
#>  [4] "custom_sql"        "calculated_fields" "field_usage"      
#>  [7] "filters"           "sorts"             "chart_types"      
#> [10] "dashboard_layout"  "actions"          
brief$meta
#> # A tibble: 1 × 7
#>   workbook_file n_datasources n_worksheets n_dashboards n_calculated_fields
#>   <chr>                 <int>        <int>        <int>               <int>
#> 1 <inline>                  2            1            0                   1
#> # ℹ 2 more variables: n_parameters <int>, generated_at <chr>
```
