# Show what a specific page is composed of.

For a dashboard: one row per zone with component type, target (worksheet
or field), filter presentation (if applicable), and x/y/w/h when
present. For a worksheet: mark types, filters, legends, parameter
controls. For a story: one row per story point with its referenced
target.

## Usage

``` r
twb_page_composition(x, name)
```

## Arguments

- x:

  TwbParser or xml2 document.

- name:

  Page name (character scalar).

## Value

Tibble with columns: page_type, page_name, component_type, zone_id,
target, field, presentation, x, y, w, h.
