# List worksheets embedded in each dashboard

Returns one row per worksheet per dashboard, with the zone's position on
the canvas.

## Usage

``` r
twb_dashboard_sheets(x, dashboard = NULL)
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

- sheet:

  Referenced worksheet name.

- zone_id:

  Zone identifier.

- x:

  Horizontal offset (pixels), or `NA`.

- y:

  Vertical offset (pixels), or `NA`.

- w:

  Width (pixels), or `NA`.

- h:

  Height (pixels), or `NA`.

## Examples

``` r
xml <- xml2::read_xml(
  '<workbook>
    <dashboards>
      <dashboard name="Overview">
        <zones>
          <zone id="1" worksheet="Sheet1" x="0" y="0" w="600" h="400"/>
        </zones>
      </dashboard>
    </dashboards>
  </workbook>'
)
twb_dashboard_sheets(xml)
#> # A tibble: 1 × 7
#>   dashboard sheet  zone_id     x     y     w     h
#>   <chr>     <chr>  <chr>   <int> <int> <int> <int>
#> 1 Overview  Sheet1 1           0     0   600   400
```
