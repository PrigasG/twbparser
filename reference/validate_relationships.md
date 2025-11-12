# Validate relationships against available datasources and fields

Checks that relationship endpoints reference known datasource tables and
that the predicate fields appear somewhere in the workbook (calculated,
raw, or parameter fields), using a lenient token match (e.g.,
`INT([GEOID])` = `GEOID`).

## Usage

``` r
validate_relationships(parser, strict = FALSE)
```

## Arguments

- parser:

  A `TwbParser`-like object that exposes: `get_relationships()`,
  `get_datasources()`, `get_fields()`, and `get_calculated_fields()`.
  (S3/R6 both fine.)

- strict:

  Logical. Reserved for future table-scoped checks that can be overly
  conservative with federated sources. Currently not used.

## Value

A list with:

- ok:

  `TRUE` if no issues; `FALSE` otherwise.

- issues:

  A named list of tibbles. Possible elements:

  - `unknown_tables`: endpoints not found among known tables.

  - `unknown_fields`: predicate fields not found in the field pool.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
if (nzchar(twb) && file.exists(twb)) {
  parser <- TwbParser$new(twb)
  res <- validate_relationships(parser)
  if (!res$ok) print(res$issues)
}
#> TWB loaded: test_for_wenjie.twb
#> TWB parsed and ready

```
