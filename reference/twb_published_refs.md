# Detect references to published data sources

Inspects datasource nodes and heuristically flags those that reference a
published (server-side) source rather than an embedded one.

## Usage

``` r
twb_published_refs(x)
```

## Arguments

- x:

  A `TwbParser` object **or** an `xml2` document.

## Value

A tibble with columns:

- name:

  Internal datasource name.

- caption:

  User-visible caption.

- hasconn:

  Value of the `hasconnection` attribute.

- likely_published:

  `TRUE` when `hasconnection = false` or when the node text contains
  published-source markers.

- hints:

  Short explanation of the classification.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)
twb_published_refs(xml)
#> # A tibble: 2 × 5
#>   name                                   caption  hasconn likely_published hints
#>   <chr>                                  <chr>    <chr>   <lgl>            <chr>
#> 1 federated.0grgaor1pd01yy1f0yr380of1ags Sheet1 … NA      FALSE            embe…
#> 2 federated.0grgaor1pd01yy1f0yr380of1ags Sheet1 … NA      FALSE            embe…
```
