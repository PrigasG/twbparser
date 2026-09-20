# Fields defined in the workbook but never used

Lists every raw field, calculated field, and parameter that is defined
in a datasource but referenced nowhere: not on any worksheet shelf,
filter, or sort, not in any tooltip, not inside any other
calculated-field formula, and not in any dashboard filter zone. When
rebuilding a workbook in another tool, these are the fields that can
safely be left behind.

## Usage

``` r
twb_unused_fields(x)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

## Value

A tibble with columns:

- datasource:

  Datasource the field belongs to.

- field_type:

  `"raw"`, `"calculated"`, or `"parameter"`.

- name:

  Human-readable field name / caption.

- tableau_internal_name:

  Bracketed internal Tableau name.

- datatype:

  Field data type.

- role:

  `"measure"` or `"dimension"`.

- is_hidden:

  Whether a raw field is hidden (`NA` for calculated fields and
  parameters).

## Details

Matching is deliberately conservative: a field is reported as unused
only when none of its name forms (internal name or display caption, in
any bracket/derivation wrapping) appears anywhere. A field whose name
merely collides with a used field in another datasource is treated as
used.

This complements
[`twb_field_usage()`](https://prigasg.github.io/twbparser/reference/twb_field_usage.md),
which shows where each *used* field appears across worksheets.

## See also

[`twb_field_usage()`](https://prigasg.github.io/twbparser/reference/twb_field_usage.md)
for where used fields appear,
[`twb_calc_build_order()`](https://prigasg.github.io/twbparser/reference/twb_calc_build_order.md)
for rebuilding calculations in dependency order.

## Examples

``` r
twb <- system.file("extdata", "rebuild_kit.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)

# the fixture leaves exactly one raw field, one calc, and one parameter unused
twb_unused_fields(xml)
#> # A tibble: 3 × 7
#>   datasource field_type name      tableau_internal_name datatype role  is_hidden
#>   <chr>      <chr>      <chr>     <chr>                 <chr>    <chr> <lgl>    
#> 1 sales-data calculated Unused C… [Calculation_0003]    real     meas… NA       
#> 2 sales-data parameter  Unused P… [Parameter 2]         string   dime… NA       
#> 3 sales-data raw        Unused F… [Unused Field]        string   dime… FALSE    

# same result through the parser object
parser <- TwbParser$new(twb)
#> TWB loaded: rebuild_kit.twb
#> TWB parsed and ready
parser$get_unused_fields()
#> # A tibble: 3 × 7
#>   datasource field_type name      tableau_internal_name datatype role  is_hidden
#>   <chr>      <chr>      <chr>     <chr>                 <chr>    <chr> <lgl>    
#> 1 sales-data calculated Unused C… [Calculation_0003]    real     meas… NA       
#> 2 sales-data parameter  Unused P… [Parameter 2]         string   dime… NA       
#> 3 sales-data raw        Unused F… [Unused Field]        string   dime… FALSE    
parser$unused_fields
#> # A tibble: 3 × 7
#>   datasource field_type name      tableau_internal_name datatype role  is_hidden
#>   <chr>      <chr>      <chr>     <chr>                 <chr>    <chr> <lgl>    
#> 1 sales-data calculated Unused C… [Calculation_0003]    real     meas… NA       
#> 2 sales-data parameter  Unused P… [Parameter 2]         string   dime… NA       
#> 3 sales-data raw        Unused F… [Unused Field]        string   dime… FALSE    
```
