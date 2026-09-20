# Tableau Workbook Parser (R6)

Create a parser for Tableau `.twb` / `.twbx` files. On initialization,
the parser reads the XML and precomputes relationships, joins, fields,
calculated fields, inferred relationships, and datasource details. For
`.twbx`, it also extracts the largest `.twb` and records a manifest.

## Format

An R6 class generator.

## Details

Results are read through **active-binding properties** (no parentheses),
e.g. `parser$summary`, `parser$overview`, `parser$datasources`. The
`get_*()` methods return the same data and are useful when you need to
pass arguments (e.g. `parser$get_sheet_shelves("My Sheet")`) or want an
explicit call.

## Fields

- path:

  Path to the `.twb` or `.twbx` file on disk.

- xml_doc:

  Parsed `xml2` document of the workbook.

- twbx_path:

  Original `.twbx` path if the workbook was packaged.

- twbx_dir:

  Directory where the `.twbx` was extracted.

- twbx_manifest:

  Tibble of `.twbx` contents from
  [`twbx_list()`](https://prigasg.github.io/twbparser/reference/twbx_list.md).

- relations:

  Tibble of `<relation>` nodes from
  [`extract_relations()`](https://prigasg.github.io/twbparser/reference/extract_relations.md).

- joins:

  Tibble of join clauses from
  [`extract_joins()`](https://prigasg.github.io/twbparser/reference/extract_joins.md).

- relationships:

  Tibble of modern relationships from
  [`extract_relationships()`](https://prigasg.github.io/twbparser/reference/extract_relationships.md).

- inferred_relationships:

  Tibble of inferred relationship pairs by name and role.

- datasource_details:

  List containing `data_sources`, `parameters`, and `all_sources`.

- fields:

  Tibble of raw fields with table information.

- calculated_fields:

  Tibble of calculated fields.

- custom_sql:

  Tibble of custom SQL relations from
  [`twb_custom_sql()`](https://prigasg.github.io/twbparser/reference/twb_custom_sql.md).

- initial_sql:

  Tibble of initial SQL statements from
  [`twb_initial_sql()`](https://prigasg.github.io/twbparser/reference/twb_initial_sql.md).

- published_refs:

  Tibble of published-datasource references from
  [`twb_published_refs()`](https://prigasg.github.io/twbparser/reference/twb_published_refs.md).

- last_validation:

  Result from `validate()` as list with `ok` and `issues` elements.

## Active bindings (read-only properties)

- summary:

  The workbook report (a `twbparser_report`); accessing it prints the
  summary. Note: this is a property, not a method — use
  `parser$summary`, not `parser$summary()`.

- report:

  Same as `summary`: the full structured workbook report.

- overview:

  One-row tibble with counts of datasources, parameters, relationships,
  fields, dashboards, and filters.

- pages:

  Tibble of workbook pages (worksheets, dashboards, stories).

- pages_summary:

  Tibble summarizing page counts by type.

- charts:

  Tibble of chart/mark information per worksheet.

- colors:

  Tibble of color encodings used across worksheets.

- dashboards:

  Tibble of dashboards in the workbook.

- dashboard_summary:

  Tibble summarizing dashboards and their filters.

- dashboard_filters:

  Tibble of dashboard filter configurations.

- datasources:

  Tibble of datasource details (see `get_datasources()`).

- parameters_tbl:

  Tibble of parameter fields (see `get_parameters()`).

- datasources_all:

  Tibble of all sources (see `get_datasources_all()`).

- fields_tbl:

  Tibble of raw fields (see `get_fields()`).

- custom_sql_tbl:

  Tibble of custom SQL (see `get_custom_sql()`).

- initial_sql_tbl:

  Tibble of initial SQL (see `get_initial_sql()`).

- published_refs_tbl:

  Tibble of published references (see `get_published_refs()`).

- twbx_manifest_tbl:

  Tibble of `.twbx` contents (see `get_twbx_manifest()`).

- twbx_extracts_tbl:

  Tibble of `.twbx` extract entries (see `get_twbx_extracts()`).

- twbx_images_tbl:

  Tibble of `.twbx` image entries (see `get_twbx_images()`).

- sheet_shelves:

  Tibble of shelf placement (see `get_sheet_shelves()`).

- sheet_filters:

  Tibble of worksheet filters (see `get_sheet_filters()`).

- sheet_axes:

  Tibble of axis configuration (see `get_sheet_axes()`).

- sheet_sorts:

  Tibble of sort directives (see `get_sheet_sorts()`).

- sheet_spec:

  Named list of per-worksheet visualization specs (see
  `get_sheet_spec()`).

- dashboard_sheets:

  Tibble of worksheets per dashboard (see `get_dashboard_sheets()`).

- dashboard_layout:

  Tibble of the zone layout tree (see `get_dashboard_layout()`).

- dashboard_actions:

  Tibble of dashboard actions (see `get_dashboard_actions()`).

- dashboard_charts:

  Tibble of charts placed on dashboards (see `get_dashboard_charts()`).

- calc_complexity:

  Tibble of calculated-field complexity (see `get_calc_complexity()`).

- field_usage:

  Tibble of field usage across worksheets (see `get_field_usage()`).

- unused_fields:

  Tibble of defined-but-never-used fields (see `get_unused_fields()`).

- calc_build_order:

  Tibble of calculated fields in rebuild order (see
  `get_calc_build_order()`).

- parameter_usage:

  Tibble of parameter consumption points (see `get_parameter_usage()`).

- validation:

  Last validation result; runs `validate()` first if it has never been
  run.

Properties are cached after first access; most return the same tibbles
as the corresponding `get_*()` methods.

## Methods

- `new(path)`:

  Create a parser from a `.twb` or `.twbx` file.

- `get_twbx_manifest()`:

  Return `.twbx` manifest tibble.

- `get_twbx_extracts()`:

  Return `.twbx` extract entries.

- `get_twbx_images()`:

  Return `.twbx` image entries.

- `extract_twbx_assets(types = NULL, pattern = NULL, files = NULL, exdir = NULL)`:

  Extract files from the `.twbx` archive to disk.

- `get_relations()`:

  Return relations tibble.

- `get_joins()`:

  Return joins tibble.

- `get_relationships()`:

  Return modern relationships tibble.

- `get_inferred_relationships()`:

  Return inferred relationship pairs.

- `get_datasources()`:

  Return datasource details tibble.

- `get_parameters()`:

  Return parameters tibble.

- `get_datasources_all()`:

  Return all sources tibble.

- `get_fields()`:

  Return raw fields tibble.

- `get_calculated_fields(pretty = FALSE, strip_brackets = FALSE, wrap = 100L, include_parameters = FALSE)`:

  Return calculated fields tibble. When `pretty = TRUE`, includes a
  `formula_pretty` column with line breaks and indentation.

- `get_custom_sql()`:

  Return custom SQL tibble.

- `get_initial_sql()`:

  Return initial SQL tibble.

- `get_published_refs()`:

  Return published-datasource references tibble.

- `get_pages()`:

  Return workbook pages tibble.

- `get_pages_summary()`:

  Return page counts by type.

- `get_page_composition(name)`:

  Return zone/mark composition of one page.

- `get_charts()`:

  Return chart/mark information per worksheet.

- `get_colors()`:

  Return color encodings used across worksheets.

- `get_dashboards()`:

  Return dashboards tibble.

- `get_dashboard_filters(dashboard = NULL)`:

  Return dashboard filter configurations.

- `get_dashboard_summary()`:

  Return dashboard summary tibble.

- `get_sheet_shelves(sheet = NULL)`:

  Fields placed on visual shelves for one or all worksheets.

- `get_sheet_filters(sheet = NULL)`:

  Detailed filter configuration for one or all worksheets.

- `get_sheet_axes(sheet = NULL)`:

  Axis configuration for one or all worksheets.

- `get_sheet_sorts(sheet = NULL)`:

  Sort directives for one or all worksheets.

- `get_sheet_spec(sheet = NULL)`:

  Full visualization spec for one or all worksheets (mark type, shelves,
  dimensions/measures, encodings, tooltips, filters, sorts, axes).

- `get_dashboard_sheets(dashboard = NULL)`:

  Worksheets embedded in one or all dashboards.

- `get_dashboard_layout(dashboard = NULL)`:

  Full zone layout with container hierarchy.

- `get_dashboard_actions(dashboard = NULL)`:

  Dashboard and workbook actions.

- `get_dashboard_charts(dashboard = NULL)`:

  One row per worksheet placed on a dashboard: mark type, fields,
  tooltip summary, and layout position.

- `get_dashboard_size(dashboard = NULL)`:

  Dashboard page size and sizing mode.

- `get_formatting(scope = NULL)`:

  Formatting rules (fonts, colours, number formats, ...); `scope` is one
  of `"worksheet"`, `"dashboard"`, or `"workbook"`.

- `get_tooltips(sheet = NULL)`:

  Plain-text worksheet tooltips.

- `get_calc_complexity(include_parameters = FALSE)`:

  Calculated field complexity classifications.

- `get_field_usage(include_filters = TRUE, include_shelves = TRUE, wide = FALSE)`:

  Field usage matrix across worksheets.

- `get_unused_fields()`:

  Fields defined but never used anywhere in the workbook.

- `get_calc_build_order()`:

  Calculated fields topologically sorted for rebuilding.

- `get_parameter_usage()`:

  Where each parameter is consumed (formulas, shelves, filters).

- `get_replication_brief(dashboard = NULL, include_sql = TRUE, include_formulas = TRUE, format = c("list", "text"))`:

  Full replication brief for the workbook or a single dashboard.

- `get_workbook_report()`:

  Return the full structured workbook report.

- `get_overview()`:

  Return the one-row overview tibble.

- `validate(error = FALSE)`:

  Validate relationships. Stops execution if `error = TRUE`.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
if (nzchar(twb)) {
  parser <- TwbParser$new(twb)
  parser$overview
  parser$get_calculated_fields()
}
#> TWB loaded: test_for_wenjie.twb
#> TWB parsed and ready
#> # A tibble: 1 × 10
#>   datasource       name  tableau_internal_name datatype role  formula calc_class
#>   <chr>            <chr> <chr>                 <chr>    <chr> <chr>   <chr>     
#> 1 federated.0grga… no d… [Calculation_2139209… string   dime… "if IS… tableau   
#> # ℹ 3 more variables: is_table_calc <lgl>, table <chr>, table_clean <chr>
```
