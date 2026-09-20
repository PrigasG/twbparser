# Worksheet visualization specs and dashboard chart inventories

`twb_sheet_spec()` reduces a worksheet to everything needed to
understand — and rebuild — its visualization in another tool: the mark
type, the fields on the rows and columns shelves (in order), the
dimensions and measures in play, every marks-card encoding (color, size,
label, detail, shape, tooltip, ...), tooltip configuration, and the
worksheet's filters, sorts, and axes.

`twb_dashboard_charts()` answers "what graphs are on this dashboard?":
one row per worksheet placed on each dashboard, with its mark type,
fields, tooltip summary, and layout position.

## Usage

``` r
twb_sheet_spec(x, sheet = NULL)

twb_dashboard_charts(x, dashboard = NULL)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- sheet:

  Optional character scalar to restrict `twb_sheet_spec()` to one
  worksheet.

- dashboard:

  Optional character scalar to restrict `twb_dashboard_charts()` to one
  dashboard.

## Value

`twb_sheet_spec()` returns a named list (one element per worksheet) of
class `twb_sheet_spec`. Each spec is a list with:

- `sheet`:

  Worksheet name.

- `mark_type`:

  Detected mark type, e.g. `"bar"`, `"line"`, `"map"`, `"text"`.
  `"automatic"` when the workbook leaves it to Tableau.

- `mark_source`:

  `"explicit"` when the workbook names the mark type, `"inferred"` for
  `"automatic"`.

- `datasources`:

  Character vector of datasources referenced.

- `rows`, `cols`:

  Character vectors of clean field names on the rows and columns
  shelves, in shelf order.

- `dimensions`, `measures`:

  Character vectors of the dimension and measure fields used anywhere in
  the visualization. A pill counts as a measure when it is aggregated;
  otherwise its declared field role wins.

- `encodings`:

  Tibble with `channel`, `field`, `aggregation`, `used_as`
  (`"dimension"`/`"measure"`) for every marks-card encoding.

- `shelves`:

  Tibble with `shelf` (`"rows"`/`"cols"`), `field`, `aggregation`,
  `used_as` for the row/column pills, in shelf order.

- `tooltip`:

  List with `has_tooltip`, `customized`, `text`, and `fields`.

- `filters`, `sorts`, `axes`:

  Tibbles from
  [`twb_sheet_filters()`](https://prigasg.github.io/twbparser/reference/twb_sheet_filters.md),
  [`twb_sheet_sorts()`](https://prigasg.github.io/twbparser/reference/twb_sheet_sorts.md),
  and
  [`twb_sheet_axes()`](https://prigasg.github.io/twbparser/reference/twb_sheet_axes.md).

`twb_dashboard_charts()` returns a tibble with one row per worksheet
placed on a dashboard: `dashboard`, `sheet`, `mark_type`, `mark_source`,
`rows`, `cols`, `dimensions`, `measures`, `tooltip_fields`
(list-columns), `n_tooltip_fields`, `has_tooltip`, `n_filters`,
`datasources`, and the layout `zone_id`, `x`, `y`, `w`, `h`.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
if (nzchar(twb) && file.exists(twb)) {
  parser <- TwbParser$new(twb)
  spec <- twb_sheet_spec(parser)
  spec

  twb_dashboard_charts(parser)
}
#> TWB loaded: test_for_wenjie.twb
#> TWB parsed and ready
#> # A tibble: 0 × 18
#> # ℹ 18 variables: dashboard <chr>, sheet <chr>, mark_type <chr>,
#> #   mark_source <chr>, rows <list>, cols <list>, dimensions <list>,
#> #   measures <list>, tooltip_fields <list>, n_tooltip_fields <int>,
#> #   has_tooltip <lgl>, n_filters <int>, datasources <list>, zone_id <chr>,
#> #   x <int>, y <int>, w <int>, h <int>
```
