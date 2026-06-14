# Classify calculated fields by complexity

Returns every calculated field in the workbook enriched with a
computation category (`calc_type`), LOD sub-type, dependency count, and
dependency depth — the maximum number of calc-on-calc hops in the
field's dependency chain.

## Usage

``` r
twb_calc_complexity(x, include_parameters = FALSE)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- include_parameters:

  Logical; if `TRUE`, include parameter fields (they always land in
  `calc_type = "raw"` and `dep_depth = 0`). Default `FALSE`.

## Value

A tibble with columns:

- datasource:

  Datasource the field belongs to.

- name:

  Human-readable field name.

- tableau_internal_name:

  Bracketed internal Tableau name.

- datatype:

  Field data type.

- role:

  `"measure"` or `"dimension"`.

- calc_type:

  One of `"lod"`, `"table_calc"`, `"aggregate"`, `"raw"`. Tested in that
  precedence order.

- lod_type:

  `"fixed"`, `"include"`, or `"exclude"`; `NA` if not LOD.

- is_table_calc:

  Logical; existing heuristic flag preserved for backward compatibility.

- dep_depth:

  Integer; longest chain of calc-on-calc dependencies. `0` means the
  field only references raw fields (or has no references).

- n_deps:

  Integer; count of distinct bracketed tokens in the formula.

- formula:

  Raw formula string.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)
twb_calc_complexity(xml)
#> # A tibble: 1 × 11
#>   datasource       name  tableau_internal_name datatype role  calc_type lod_type
#>   <chr>            <chr> <chr>                 <chr>    <chr> <chr>     <chr>   
#> 1 federated.0grga… no d… [Calculation_2139209… string   dime… raw       NA      
#> # ℹ 4 more variables: is_table_calc <lgl>, dep_depth <int>, n_deps <int>,
#> #   formula <chr>
```
