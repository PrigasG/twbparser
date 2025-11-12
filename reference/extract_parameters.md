# Extract parameter fields from a TWB

Returns parameter columns (those with `param-domain-type`) and basic
metadata, including a best-effort current value if present.

## Usage

``` r
extract_parameters(xml_doc)
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

- parameter_type:

  Tableau parameter domain type.

- allowable_type:

  Underlying data-type (if present).

- current_value:

  Current value if specified.

- is_parameter:

  Always `TRUE`.

- table:

  Raw table reference.

- table_clean:

  Cleaned table name.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)
params <- extract_parameters(xml)
head(params)
#> # A tibble: 0 × 0

```
