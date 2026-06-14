# Field usage matrix across worksheets

Combines shelf placement and filter usage into a tidy long tibble
showing where each field appears and in what capacity across all (or
selected) worksheets.

## Usage

``` r
twb_field_usage(
  x,
  include_filters = TRUE,
  include_shelves = TRUE,
  wide = FALSE
)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- include_filters:

  Logical; include filter appearances. Default `TRUE`.

- include_shelves:

  Logical; include shelf appearances (rows, cols, color, size, etc.).
  Default `TRUE`.

- wide:

  Logical; if `TRUE`, pivot to one row per field with one column per
  sheet containing a comma-separated list of contexts, or `NA` if the
  field does not appear on that sheet. Default `FALSE`.

## Value

**Long form** (`wide = FALSE`): a tibble with columns:

- field_clean:

  Human-readable field name.

- datasource:

  Datasource the field belongs to.

- sheet:

  Worksheet name.

- context:

  Usage context, e.g. `"shelf:rows"`, `"shelf:color"`, `"filter"`.

- n_appearances:

  Number of times the field appears in this context on this sheet
  (handles multi-pill rows/cols).

**Wide form** (`wide = TRUE`): one row per `(field_clean, datasource)`,
one column per sheet, cell value is a comma-separated context string or
`NA`.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)
twb_field_usage(xml)
#> # A tibble: 6 × 5
#>   field_clean                     datasource         sheet context n_appearances
#>   <chr>                           <chr>              <chr> <chr>           <int>
#> 1 Calculation_2139209847776120832 federated.0grgaor… Shee… shelf:…             1
#> 2 Geometry                        federated.0grgaor… Shee… shelf:…             1
#> 3 Geometry                        federated.0grgaor… Shee… shelf:…             1
#> 4 Latitude (generated)            federated.0grgaor… Shee… shelf:…             1
#> 5 Longitude (generated)           federated.0grgaor… Shee… shelf:…             1
#> 6 counts                          federated.0grgaor… Shee… shelf:…             1
twb_field_usage(xml, wide = TRUE)
#> # A tibble: 5 × 3
#>   field_clean                     datasource                           `Sheet 1`
#>   <chr>                           <chr>                                <chr>    
#> 1 Calculation_2139209847776120832 federated.0grgaor1pd01yy1f0yr380of1… shelf:co…
#> 2 Geometry                        federated.0grgaor1pd01yy1f0yr380of1… shelf:ge…
#> 3 Latitude (generated)            federated.0grgaor1pd01yy1f0yr380of1… shelf:ro…
#> 4 Longitude (generated)           federated.0grgaor1pd01yy1f0yr380of1… shelf:co…
#> 5 counts                          federated.0grgaor1pd01yy1f0yr380of1… shelf:lod
```
