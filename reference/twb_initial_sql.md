# Extract Initial SQL statements from Tableau connections

Returns any `<initial-sql>` nodes found inside connection or
named-connection elements.

## Usage

``` r
twb_initial_sql(x)
```

## Arguments

- x:

  A `TwbParser` object **or** an `xml2` document.

## Value

A tibble with columns:

- connection_id:

  Name or caption of the parent connection element.

- initial_sql:

  SQL text of the initial statement.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)
twb_initial_sql(xml)
#> # A tibble: 0 × 2
#> # ℹ 2 variables: connection_id <chr>, initial_sql <chr>
```
