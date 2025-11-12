# Extract all `<relation>` tags from a TWB

Returns a tibble of `<relation>` elements found in a Tableau TWB XML,
with key attributes and any custom SQL text.

## Usage

``` r
extract_relations(xml_doc)
```

## Arguments

- xml_doc:

  An `xml2` document for a Tableau `.twb`.

## Value

A tibble with columns:

- name:

  Relation name

- table:

  Table reference

- connection:

  Connection ID

- type:

  Relation type (table, join, etc.)

- join:

  Join type if applicable

- custom_sql:

  Inline SQL text if present

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
