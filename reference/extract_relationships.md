# Extract modern relationships from a Tableau TWB

Parses Tableau "relationships" (introduced in 2020.2) between logical
tables, including the join predicate fields and operator.

## Usage

``` r
extract_relationships(xml_doc)
```

## Arguments

- xml_doc:

  An `xml2` document for a Tableau `.twb`.

## Value

A tibble with columns:

- relationship_type:

  Always "Relationship"

- left_table:

  Left table name

- right_table:

  Right table name

- left_field:

  Field name on left side

- operator:

  Join operator (e.g., "=")

- right_field:

  Field name on right side

- left_is_calc:

  Logical, whether left field is a calculation

- right_is_calc:

  Logical, whether right field is a calculation

## Examples

``` r

twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)
extract_relationships(xml)
#> # A tibble: 1 × 8
#>   relationship_type left_table right_table       left_field operator right_field
#>   <chr>             <chr>      <chr>             <chr>      <chr>    <chr>      
#> 1 Relationship      Sheet1     Municipal_Bounda… County     =        COUNTY     
#> # ℹ 2 more variables: left_is_calc <lgl>, right_is_calc <lgl>

```
