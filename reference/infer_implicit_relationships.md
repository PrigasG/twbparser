# Infer implicit relationships between tables from field metadata

Generates candidate join pairs by:

- Matching `semantic_role` across different tables.

- Matching field names (case-insensitive) across different tables.

## Usage

``` r
infer_implicit_relationships(fields_df, max_pairs = 50000L)
```

## Arguments

- fields_df:

  A data frame like the output of
  [`extract_columns_with_table_source()`](https://PrigasG.github.io/twbparser/reference/extract_columns_with_table_source.md).

- max_pairs:

  Maximum number of candidate pairs to return (default 50,000).

## Value

A tibble with columns:

- left_table:

  Left table name.

- left_field:

  Left field name.

- right_table:

  Right table name.

- right_field:

  Right field name.

- reason:

  Why the pair was suggested.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)
fields <- extract_columns_with_table_source(xml)
inferred <- infer_implicit_relationships(fields)
head(inferred)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: left_table <chr>, left_field <chr>, right_table <chr>,
#> #   right_field <chr>, reason <chr>

```
