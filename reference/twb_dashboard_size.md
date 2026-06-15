# Dashboard page size and sizing mode

Reads the `<size>` element of each dashboard, which records how the page
is sized: a fixed pixel size, an automatic (fit-to-window) size, or a
range.

## Usage

``` r
twb_dashboard_size(x, dashboard = NULL)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- dashboard:

  Optional character scalar to restrict output to one dashboard.

## Value

A tibble with columns:

- dashboard:

  Dashboard name.

- sizing_mode:

  `"fixed"`, `"automatic"`, `"range"`, or another mode string; `NA` if
  no `<size>` element is present.

- min_width:

  Minimum width in pixels, or `NA`.

- min_height:

  Minimum height in pixels, or `NA`.

- max_width:

  Maximum (or fixed) width in pixels, or `NA`.

- max_height:

  Maximum (or fixed) height in pixels, or `NA`.

## Examples

``` r
xml <- xml2::read_xml(
  '<workbook><dashboards>
    <dashboard name="Overview">
      <size sizing-mode="fixed" maxwidth="1200" maxheight="800"
            minwidth="1200" minheight="800"/>
    </dashboard>
  </dashboards></workbook>'
)
twb_dashboard_size(xml)
#> # A tibble: 1 × 6
#>   dashboard sizing_mode min_width min_height max_width max_height
#>   <chr>     <chr>           <int>      <int>     <int>      <int>
#> 1 Overview  fixed            1200        800      1200        800
```
