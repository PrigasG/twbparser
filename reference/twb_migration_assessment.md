# Assess Tableau workbook migration readiness

Scores workbook complexity and summarizes likely migration risks for a
target visualization tool.

## Usage

``` r
twb_migration_assessment(
  x,
  target = c("powerbi", "shiny", "quarto", "looker", "superset")
)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- target:

  Target tool. One of `"powerbi"`, `"shiny"`, `"quarto"`, `"looker"`, or
  `"superset"`.

## Value

A named list with `summary`, `compatibility`, and `recommendations`.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
if (nzchar(twb) && file.exists(twb)) {
  parser <- TwbParser$new(twb)
  twb_migration_assessment(parser, target = "shiny")$summary
}
#> TWB loaded: test_for_wenjie.twb
#> TWB parsed and ready
#> # A tibble: 1 × 10
#>   target workbook_file       worksheets dashboards datasources calculated_fields
#>   <chr>  <chr>                    <int>      <int>       <int>             <int>
#> 1 shiny  test_for_wenjie.twb          1          0           2                 1
#> # ℹ 4 more variables: lod_calculations <int>, table_calculations <int>,
#> #   migration_score <int>, migration_effort <chr>
```
