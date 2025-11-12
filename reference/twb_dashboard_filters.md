# Filters found on dashboards and their positions.

Filters found on dashboards and their positions.

## Usage

``` r
twb_dashboard_filters(x, dashboard = NULL)
```

## Arguments

- x:

  TwbParser or xml2 document.

- dashboard:

  Optional dashboard name to filter to.

## Value

Tibble with columns: dashboard, zone_id, zone_type, field, presentation,
x, y, w, h.
