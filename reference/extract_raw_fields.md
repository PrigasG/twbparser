# Extract non-calculated, non-parameter fields from a TWB

Returns raw columns excluding calculated fields and parameters.

## Usage

``` r
extract_raw_fields(xml_doc)
```

## Arguments

- xml_doc:

  An `xml2` document for a Tableau `.twb`.

## Value

A tibble with columns:

- datasource:

  Datasource name.

- name:

  User-visible caption or cleaned internal name.

- tableau_internal_name:

  Internal Tableau name.

- datatype:

  Tableau datatype.

- role:

  Tableau role.

- is_hidden:

  Whether the field is hidden.

- is_parameter:

  Always `FALSE`.

- table:

  Raw table reference.

- table_clean:

  Cleaned table name.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)
raw_fields <- extract_raw_fields(xml)
head(raw_fields)
#> # A tibble: 6 × 9
#>   datasource   name  tableau_internal_name datatype role  is_hidden is_parameter
#>   <chr>        <chr> <chr>                 <chr>    <chr> <lgl>     <lgl>       
#> 1 federated.0… OBJE… OBJECTID              integer  NA    FALSE     FALSE       
#> 2 federated.0… MUN   MUN                   string   NA    FALSE     FALSE       
#> 3 federated.0… COUN… COUNTY                string   NA    FALSE     FALSE       
#> 4 federated.0… MUN_… MUN_LABEL             string   NA    FALSE     FALSE       
#> 5 federated.0… MUN_… MUN_TYPE              string   NA    FALSE     FALSE       
#> 6 federated.0… NAME  NAME                  string   NA    FALSE     FALSE       
#> # ℹ 2 more variables: table <chr>, table_clean <chr>

```
