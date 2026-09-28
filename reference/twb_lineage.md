# Build workbook lineage for migration analysis

Produces a graph-shaped representation of workbook dependencies from
datasources and tables through fields, calculated fields, worksheets,
and dashboards.

## Usage

``` r
twb_lineage(
  x,
  format = c("tables", "igraph", "mermaid"),
  include_calc_dependencies = TRUE
)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- format:

  Output format: `"tables"` returns `list(nodes, edges)`; `"igraph"`
  returns an igraph object; `"mermaid"` returns a Mermaid flowchart
  string.

- include_calc_dependencies:

  Logical; include field-to-calculation and calculation-to-calculation
  dependencies parsed from formulas. Default `TRUE`.

## Value

Depends on `format`. The default is a list with:

- nodes:

  Tibble with `id`, `label`, and `type`.

- edges:

  Tibble with `from`, `to`, and `relationship`.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
if (nzchar(twb) && file.exists(twb)) {
  parser <- TwbParser$new(twb)
  lineage <- twb_lineage(parser)
  lineage$nodes
  lineage$edges
}
#> TWB loaded: test_for_wenjie.twb
#> TWB parsed and ready
#> # A tibble: 7 × 4
#>   from                                               to    relationship workbook
#>   <chr>                                              <chr> <chr>        <chr>   
#> 1 field::federated.0grgaor1pd01yy1f0yr380of1ags::co… calc… used_by_cal… test_fo…
#> 2 field::federated.0grgaor1pd01yy1f0yr380of1ags::Ca… work… shelf:color  test_fo…
#> 3 field::federated.0grgaor1pd01yy1f0yr380of1ags::Ge… work… shelf:geome… test_fo…
#> 4 field::federated.0grgaor1pd01yy1f0yr380of1ags::Ge… work… shelf:lod    test_fo…
#> 5 field::federated.0grgaor1pd01yy1f0yr380of1ags::La… work… shelf:rows   test_fo…
#> 6 field::federated.0grgaor1pd01yy1f0yr380of1ags::Lo… work… shelf:cols   test_fo…
#> 7 field::federated.0grgaor1pd01yy1f0yr380of1ags::co… work… shelf:lod    test_fo…
```
