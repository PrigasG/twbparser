# Extract calculated fields from a TWB

Finds columns that contain `<calculation>`nodes and returns metadata and
formulas, with a heuristic flag for table calculations.

## Usage

``` r
extract_calculated_fields(xml_doc, include_parameters = FALSE)
```

## Arguments

- xml_doc:

  An xml2 document for a Tableau .twb.

- include_parameters:

  Logical. If TRUE, include items from the "Parameters" datasource or
  columns with @param-domain-type. Default FALSE.

## Value

A tibble with columns:

- datasource:

  Datasource name.

- name:

  User-visible caption or cleaned internal name.

- tableau_internal_name:

  Internal Tableau name (often bracketed).

- datatype:

  Tableau datatype.

- role:

  Tableau role.

- formula:

  Calculation formula string.

- calc_class:

  Tableau calc class (often "tableau").

- is_table_calc:

  Heuristic flag for table calcs (e.g., WINDOW\_, LOOKUP).

- table:

  Raw table reference.

- table_clean:

  Cleaned table name.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)
calcs <- extract_calculated_fields(xml)
head(calcs)
#> # A tibble: 1 × 10
#>   datasource       name  tableau_internal_name datatype role  formula calc_class
#>   <chr>            <chr> <chr>                 <chr>    <chr> <chr>   <chr>     
#> 1 federated.0grga… no d… [Calculation_2139209… string   dime… "if IS… tableau   
#> # ℹ 3 more variables: is_table_calc <lgl>, table <chr>, table_clean <chr>
```
