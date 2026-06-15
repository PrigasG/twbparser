# Worksheet tooltips

Extracts the (plain-text) tooltip configured for each worksheet. Tableau
stores customised tooltips as formatted-text runs; this returns the
concatenated text with markup stripped.

## Usage

``` r
twb_tooltips(x, sheet = NULL)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- sheet:

  Optional character scalar to restrict output to one worksheet.

## Value

A tibble with columns:

- sheet:

  Worksheet name.

- is_customized:

  `TRUE` if a non-empty customised tooltip is present.

- tooltip_text:

  Plain-text tooltip content, or `NA`.

## Examples

``` r
xml <- xml2::read_xml(
  '<workbook><worksheets><worksheet name="Sales">
    <customized-tooltip><formatted-text>
      <run>Sales: </run><run>&lt;SUM(Sales)&gt;</run>
    </formatted-text></customized-tooltip>
  </worksheet></worksheets></workbook>'
)
twb_tooltips(xml)
#> # A tibble: 1 × 3
#>   sheet is_customized tooltip_text       
#>   <chr> <lgl>         <chr>              
#> 1 Sales TRUE          Sales: <SUM(Sales)>
```
