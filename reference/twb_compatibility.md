# Report feature compatibility for migration targets

Detects Tableau workbook features that commonly affect migrations and
maps them to target-tool support levels.

## Usage

``` r
twb_compatibility(
  x,
  targets = c("powerbi", "shiny", "quarto", "looker", "superset")
)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- targets:

  Character vector of targets. Supported values are `"powerbi"`,
  `"shiny"`, `"quarto"`, `"looker"`, and `"superset"`.

## Value

A tibble with one row per detected/checked feature and target.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
if (nzchar(twb) && file.exists(twb)) {
  parser <- TwbParser$new(twb)
  twb_compatibility(parser, targets = c("powerbi", "shiny"))
}
#> TWB loaded: test_for_wenjie.twb
#> TWB parsed and ready
#> # A tibble: 18 × 6
#>    target  support           feature               detected impact note         
#>    <chr>   <chr>             <chr>                 <lgl>    <chr>  <chr>        
#>  1 powerbi manual_rebuild    lod_calculations      FALSE    high   LOD expressi…
#>  2 powerbi manual_rebuild    table_calculations    FALSE    high   Table calcul…
#>  3 powerbi partial           parameters            FALSE    medium Parameters m…
#>  4 powerbi partial           dashboard_actions     FALSE    medium Dashboard ac…
#>  5 powerbi supported_review  custom_sql            FALSE    medium Custom SQL s…
#>  6 powerbi manual_reconnect  published_datasources TRUE     medium Published da…
#>  7 powerbi unsupported       stories               FALSE    high   Stories rare…
#>  8 powerbi manual_redesign   floating_layout       FALSE    medium Floating lay…
#>  9 powerbi manual_replatform packaged_extracts     FALSE    medium Packaged ext…
#> 10 shiny   code_rebuild      lod_calculations      FALSE    high   LOD expressi…
#> 11 shiny   code_rebuild      table_calculations    FALSE    high   Table calcul…
#> 12 shiny   supported         parameters            FALSE    medium Parameters m…
#> 13 shiny   code_rebuild      dashboard_actions     FALSE    medium Dashboard ac…
#> 14 shiny   supported_review  custom_sql            FALSE    medium Custom SQL s…
#> 15 shiny   manual_reconnect  published_datasources TRUE     medium Published da…
#> 16 shiny   manual_rebuild    stories               FALSE    high   Stories rare…
#> 17 shiny   manual_redesign   floating_layout       FALSE    medium Floating lay…
#> 18 shiny   manual_replatform packaged_extracts     FALSE    medium Packaged ext…
```
