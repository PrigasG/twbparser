# Full layout of dashboard zones with container hierarchy

Returns one row per zone per dashboard, including the parent-zone
relationship and a tiled/floating classification.

## Usage

``` r
twb_dashboard_layout(x, dashboard = NULL)
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

- zone_id:

  Zone identifier.

- parent_zone_id:

  Parent zone identifier (`NA` for root zones).

- component_type:

  `"worksheet"`, `"filter"`, `"legend"`, `"parameter_control"`,
  `"text"`, `"image"`, `"container"`, or `"blank"`.

- target:

  Referenced worksheet name or object, if applicable.

- layout_type:

  `"floating"` or `"tiled"`.

- x:

  Horizontal offset, or `NA`.

- y:

  Vertical offset, or `NA`.

- w:

  Width, or `NA`.

- h:

  Height, or `NA`.

## Examples

``` r
xml <- xml2::read_xml(
  '<workbook>
    <dashboards>
      <dashboard name="Overview">
        <zones>
          <zone id="1" type="layoutV">
            <zone id="2" worksheet="Sheet1" x="0" y="0" w="600" h="400"/>
            <zone id="3" type="filter" x="0" y="400" w="600" h="50"/>
          </zone>
        </zones>
      </dashboard>
    </dashboards>
  </workbook>'
)
twb_dashboard_layout(xml)
#> # A tibble: 3 × 10
#>   dashboard zone_id parent_zone_id component_type target layout_type     x     y
#>   <chr>     <chr>   <chr>          <chr>          <chr>  <chr>       <int> <int>
#> 1 Overview  1       NA             container      NA     tiled          NA    NA
#> 2 Overview  2       1              worksheet      Sheet1 tiled           0     0
#> 3 Overview  3       1              filter         NA     tiled           0   400
#> # ℹ 2 more variables: w <int>, h <int>
```
