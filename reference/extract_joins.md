# Extract Tableau join clauses from `<relation type="join">` nodes

Handles both column-based clauses (`<clause><column/></clause>`) and
expression-based predicates (`<expression op=...>`) found in TWB XML.

## Usage

``` r
extract_joins(xml_doc)
```

## Arguments

- xml_doc:

  An `xml2` document for a Tableau `.twb`.

## Value

A tibble with columns:

- join_type:

  Join kind (e.g., `inner`, `left`), if available.

- left_table:

  Left table name (cleaned).

- left_field:

  Left field name.

- operator:

  Predicate operator (defaults to `"="` when missing).

- right_table:

  Right table name (cleaned).

- right_field:

  Right field name.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)
extract_joins(xml)
#> # A tibble: 0 × 6
#> # ℹ 6 variables: join_type <chr>, left_table <chr>, left_field <chr>,
#> #   operator <chr>, right_table <chr>, right_field <chr>

```
