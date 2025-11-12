# Extract columns with their source tables from a TWB

Scans top-level `<datasource>` nodes (excluding view-specific
references) and returns fields with raw names/captions, cleaned
table/field names, and basic metadata.

## Usage

``` r
extract_columns_with_table_source(xml_doc)
```

## Arguments

- xml_doc:

  An `xml2` document for a Tableau `.twb`.

## Value

A tibble with columns:

- datasource:

  Datasource name.

- name:

  Raw column name (may include brackets/qualifiers).

- caption:

  Column caption if present.

- datatype:

  Tableau datatype.

- role:

  Tableau role.

- semantic_role:

  Semantic role if present.

- table:

  Raw table reference.

- table_clean:

  Cleaned table name (no brackets/suffix).

- field_clean:

  Cleaned field name.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)
fields <- extract_columns_with_table_source(xml)

```
