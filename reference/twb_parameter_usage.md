# Where each parameter is consumed

One row per parameter usage: calculated-field formulas that reference
the parameter, worksheet shelves and filters it appears on, and
dashboard filter zones bound to it. Parameters become variables when
rebuilding in another tool, so this is the map of everywhere each
variable's value flows.

## Usage

``` r
twb_parameter_usage(x)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

## Value

A tibble with columns:

- parameter:

  Human-readable parameter name / caption.

- datasource:

  Datasource the parameter belongs to.

- datatype:

  Parameter data type.

- current_value:

  Current value if specified in the workbook.

- context:

  Where it is used: `"formula"`, `"shelf:<shelf>"` (e.g.
  `"shelf:rows"`), `"filter"`, or `"dashboard_filter"`.

- location:

  Calculated field name, worksheet name, or dashboard name, depending on
  context.

## Details

Parameters with no usages do not appear here; find them with
[`twb_unused_fields()`](https://prigasg.github.io/twbparser/reference/twb_unused_fields.md).
There is no overlap with
[`twb_field_usage()`](https://prigasg.github.io/twbparser/reference/twb_field_usage.md):
that function maps *fields* to worksheets, while this one maps
*parameters* to every consumption point including formulas.

## See also

[`twb_unused_fields()`](https://prigasg.github.io/twbparser/reference/twb_unused_fields.md)
for parameters nothing references,
[`extract_parameters()`](https://prigasg.github.io/twbparser/reference/extract_parameters.md)
for parameter definitions.

## Examples

``` r
twb <- system.file("extdata", "rebuild_kit.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)

# "Top N" is used in the "Adjusted Ratio" formula and on a worksheet filter
twb_parameter_usage(xml)
#> # A tibble: 2 × 6
#>   parameter datasource datatype current_value context location      
#>   <chr>     <chr>      <chr>    <chr>         <chr>   <chr>         
#> 1 Top N     sales-data integer  5             filter  Profit Detail 
#> 2 Top N     sales-data integer  5             formula Adjusted Ratio

parser <- TwbParser$new(twb)
#> TWB loaded: rebuild_kit.twb
#> TWB parsed and ready
parser$get_parameter_usage()
#> # A tibble: 2 × 6
#>   parameter datasource datatype current_value context location      
#>   <chr>     <chr>      <chr>    <chr>         <chr>   <chr>         
#> 1 Top N     sales-data integer  5             filter  Profit Detail 
#> 2 Top N     sales-data integer  5             formula Adjusted Ratio
parser$parameter_usage
#> # A tibble: 2 × 6
#>   parameter datasource datatype current_value context location      
#>   <chr>     <chr>      <chr>    <chr>         <chr>   <chr>         
#> 1 Top N     sales-data integer  5             filter  Profit Detail 
#> 2 Top N     sales-data integer  5             formula Adjusted Ratio
```
