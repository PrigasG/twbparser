# Calculated fields in rebuild dependency order

Returns every calculated field topologically sorted so that each field
appears *after* the calculated fields its formula depends on. Recreate
the fields in `build_order` sequence and every reference will already
exist. Fields caught in a dependency cycle cannot be ordered: they get
`build_order = NA` and `is_cyclic = TRUE` (plus a warning naming them).

## Usage

``` r
twb_calc_build_order(x)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

## Value

A tibble with columns:

- build_order:

  Integer creation sequence (`NA` for cyclic fields).

- datasource:

  Datasource the field belongs to.

- name:

  Human-readable field name / caption.

- tableau_internal_name:

  Bracketed internal Tableau name.

- formula:

  Raw formula string.

- depends_on:

  Comma-separated names of the calculated fields this formula directly
  depends on (`NA` when none).

- n_calc_deps:

  Integer count of direct calc-on-calc dependencies.

- is_cyclic:

  Logical; `TRUE` when the field is part of a dependency cycle and could
  not be ordered.

## Details

A formula token is treated as a calc-on-calc dependency only when it
matches a calculated field *and not* a raw field or parameter, so
`SUM([Sales])` inside a calc captioned `"Sales"` does not create a false
self-loop. This is a rebuild-ordering aid, not a duplicate of
[`twb_calc_complexity()`](https://prigasg.github.io/twbparser/reference/twb_calc_complexity.md):
that function classifies complexity and counts dependencies, while this
one gives the concrete creation sequence plus the human-readable
dependency list.

## See also

[`twb_calc_complexity()`](https://prigasg.github.io/twbparser/reference/twb_calc_complexity.md)
for complexity classification,
[`twb_unused_fields()`](https://prigasg.github.io/twbparser/reference/twb_unused_fields.md)
for calculations nothing references.

## Examples

``` r
twb <- system.file("extdata", "rebuild_kit.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)

# "Adjusted Ratio" is built after "Profit Ratio"; the Cycle A/B pair is flagged
twb_calc_build_order(xml)
#> Warning: Circular dependencies detected among calculated fields: Cycle A, Cycle B. `build_order` is NA for these fields; break the cycle before rebuilding.
#> # A tibble: 5 × 8
#>   build_order datasource name           tableau_internal_name formula depends_on
#>         <int> <chr>      <chr>          <chr>                 <chr>   <chr>     
#> 1           1 sales-data Profit Ratio   [Calculation_0001]    [Profi… NA        
#> 2           2 sales-data Unused Calc    [Calculation_0003]    [Sales… NA        
#> 3           3 sales-data Adjusted Ratio [Calculation_0002]    [Profi… Profit Ra…
#> 4          NA sales-data Cycle A        [Calculation_0004]    [Cycle… Cycle B   
#> 5          NA sales-data Cycle B        [Calculation_0005]    [Cycle… Cycle A   
#> # ℹ 2 more variables: n_calc_deps <int>, is_cyclic <lgl>

parser <- TwbParser$new(twb)
#> TWB loaded: rebuild_kit.twb
#> TWB parsed and ready
parser$get_calc_build_order()
#> Warning: Circular dependencies detected among calculated fields: Cycle A, Cycle B. `build_order` is NA for these fields; break the cycle before rebuilding.
#> # A tibble: 5 × 8
#>   build_order datasource name           tableau_internal_name formula depends_on
#>         <int> <chr>      <chr>          <chr>                 <chr>   <chr>     
#> 1           1 sales-data Profit Ratio   [Calculation_0001]    [Profi… NA        
#> 2           2 sales-data Unused Calc    [Calculation_0003]    [Sales… NA        
#> 3           3 sales-data Adjusted Ratio [Calculation_0002]    [Profi… Profit Ra…
#> 4          NA sales-data Cycle A        [Calculation_0004]    [Cycle… Cycle B   
#> 5          NA sales-data Cycle B        [Calculation_0005]    [Cycle… Cycle A   
#> # ℹ 2 more variables: n_calc_deps <int>, is_cyclic <lgl>
parser$calc_build_order
#> Warning: Circular dependencies detected among calculated fields: Cycle A, Cycle B. `build_order` is NA for these fields; break the cycle before rebuilding.
#> # A tibble: 5 × 8
#>   build_order datasource name           tableau_internal_name formula depends_on
#>         <int> <chr>      <chr>          <chr>                 <chr>   <chr>     
#> 1           1 sales-data Profit Ratio   [Calculation_0001]    [Profi… NA        
#> 2           2 sales-data Unused Calc    [Calculation_0003]    [Sales… NA        
#> 3           3 sales-data Adjusted Ratio [Calculation_0002]    [Profi… Profit Ra…
#> 4          NA sales-data Cycle A        [Calculation_0004]    [Cycle… Cycle B   
#> 5          NA sales-data Cycle B        [Calculation_0005]    [Cycle… Cycle A   
#> # ℹ 2 more variables: n_calc_deps <int>, is_cyclic <lgl>
```
