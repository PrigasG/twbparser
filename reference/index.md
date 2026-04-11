# Package index

## Core

Main R6 class used to parse .twb/.twbx workbooks.

- [`TwbParser`](https://PrigasG.github.io/twbparser/reference/TwbParser.md)
  [`TWBParser`](https://PrigasG.github.io/twbparser/reference/TwbParser.md)
  : Tableau Workbook Parser (R6)

## Insights (dashboards, pages, charts, colors)

Page-centric helpers and summaries; work with TwbParser or xml2
documents.

- [`twb_pages()`](https://PrigasG.github.io/twbparser/reference/twb_pages.md)
  : List all pages (dashboards, worksheets, stories).
- [`twb_pages_summary()`](https://PrigasG.github.io/twbparser/reference/twb_pages_summary.md)
  : Summary of all pages (counts and quick descriptors).
- [`twb_page_composition()`](https://PrigasG.github.io/twbparser/reference/twb_page_composition.md)
  : Show what a specific page is composed of.
- [`twb_dashboards()`](https://PrigasG.github.io/twbparser/reference/twb_dashboards.md)
  : Dashboards overview (count of zones and referenced worksheets).
- [`twb_dashboard_filters()`](https://PrigasG.github.io/twbparser/reference/twb_dashboard_filters.md)
  : Filters found on dashboards and their positions.
- [`twb_charts()`](https://PrigasG.github.io/twbparser/reference/twb_charts.md)
  : Chart (mark) types per worksheet.
- [`twb_colors()`](https://PrigasG.github.io/twbparser/reference/twb_colors.md)
  : Colors and palettes referenced in the workbook.
- [`twb_dashboard_summary()`](https://PrigasG.github.io/twbparser/reference/twb_dashboard_summary.md)
  : Per-dashboard summary (filters count and chart types).

## Worksheet intelligence

Per-worksheet shelf, filter, axis, and sort details.

- [`twb_sheet_shelves()`](https://PrigasG.github.io/twbparser/reference/twb_sheet_shelves.md)
  : Extract field-to-shelf assignments for worksheets
- [`twb_sheet_filters()`](https://PrigasG.github.io/twbparser/reference/twb_sheet_filters.md)
  : Extract detailed filter configuration for worksheets
- [`twb_sheet_axes()`](https://PrigasG.github.io/twbparser/reference/twb_sheet_axes.md)
  : Extract axis configuration for worksheets
- [`twb_sheet_sorts()`](https://PrigasG.github.io/twbparser/reference/twb_sheet_sorts.md)
  : Extract sort configuration for worksheets

## Dashboard intelligence

Per-dashboard zone layout, sheet positions, and actions.

- [`twb_dashboard_sheets()`](https://PrigasG.github.io/twbparser/reference/twb_dashboard_sheets.md)
  : List worksheets embedded in each dashboard
- [`twb_dashboard_layout()`](https://PrigasG.github.io/twbparser/reference/twb_dashboard_layout.md)
  : Full layout of dashboard zones with container hierarchy
- [`twb_dashboard_actions()`](https://PrigasG.github.io/twbparser/reference/twb_dashboard_actions.md)
  : Extract dashboard and workbook actions

## TWBX helpers

- [`twbx_list()`](https://PrigasG.github.io/twbparser/reference/twbx_list.md)
  : List contents of a Tableau .twbx
- [`extract_twb_from_twbx()`](https://PrigasG.github.io/twbparser/reference/extract_twb_from_twbx.md)
  : Extract the .twb (and optionally all files) from a .twbx
- [`twbx_extract_files()`](https://PrigasG.github.io/twbparser/reference/twbx_extract_files.md)
  : Extract specific files from a .twbx

## Extraction

- [`extract_named_connections()`](https://PrigasG.github.io/twbparser/reference/extract_named_connections.md)
  :

  Extract `<named-connection>` entries from a TWB

- [`extract_datasource_details()`](https://PrigasG.github.io/twbparser/reference/extract_datasource_details.md)
  : Extract datasource details from a Tableau TWB

- [`extract_raw_fields()`](https://PrigasG.github.io/twbparser/reference/extract_raw_fields.md)
  : Extract non-calculated, non-parameter fields from a TWB

- [`extract_columns_with_table_source()`](https://PrigasG.github.io/twbparser/reference/extract_columns_with_table_source.md)
  : Extract columns with their source tables from a TWB

- [`extract_calculated_fields()`](https://PrigasG.github.io/twbparser/reference/extract_calculated_fields.md)
  : Extract calculated fields from a TWB

- [`extract_parameters()`](https://PrigasG.github.io/twbparser/reference/extract_parameters.md)
  : Extract parameter fields from a TWB

- [`extract_relations()`](https://PrigasG.github.io/twbparser/reference/extract_relations.md)
  :

  Extract all `<relation>` tags from a TWB

- [`extract_joins()`](https://PrigasG.github.io/twbparser/reference/extract_joins.md)
  :

  Extract Tableau join clauses from `<relation type="join">` nodes

- [`extract_relationships()`](https://PrigasG.github.io/twbparser/reference/extract_relationships.md)
  : Extract modern relationships from a Tableau TWB

- [`twb_initial_sql()`](https://PrigasG.github.io/twbparser/reference/twb_initial_sql.md)
  : Extract Initial SQL statements from connections (if present)

- [`twb_custom_sql()`](https://PrigasG.github.io/twbparser/reference/twb_custom_sql.md)
  : Extract Custom SQL relations from a TWB XML

- [`twb_published_refs()`](https://PrigasG.github.io/twbparser/reference/twb_published_refs.md)
  : Detect likely references to published data sources (vs embedded)

## Inference & validation

- [`infer_implicit_relationships()`](https://PrigasG.github.io/twbparser/reference/infer_implicit_relationships.md)
  : Infer implicit relationships between tables from field metadata
- [`validate_relationships()`](https://PrigasG.github.io/twbparser/reference/validate_relationships.md)
  : Validate relationships against available datasources and fields

## Lineage & graphs

- [`build_dependency_graph()`](https://PrigasG.github.io/twbparser/reference/build_dependency_graph.md)
  : Build a field dependency graph from calculated fields
- [`plot_dependency_graph()`](https://PrigasG.github.io/twbparser/reference/plot_dependency_graph.md)
  : Plot a field dependency graph
- [`plot_relationship_graph()`](https://PrigasG.github.io/twbparser/reference/plot_relationship_graph.md)
  : Plot a field-level relationship DAG (legacy)
- [`plot_source_join_graph()`](https://PrigasG.github.io/twbparser/reference/plot_source_join_graph.md)
  : Plot a source join graph

## Formatting & display

- [`tableau_formula_pretty()`](https://PrigasG.github.io/twbparser/reference/tableau_formula_pretty.md)
  : Prettify a Tableau calculation formula for display
- [`prettify_calculated_fields()`](https://PrigasG.github.io/twbparser/reference/prettify_calculated_fields.md)
  : Add a prettified formula column to calculated fields table

## Server/Cloud (optional)

- [`tbs_publish_info()`](https://PrigasG.github.io/twbparser/reference/tbs_publish_info.md)
  : Publish info for a workbook or datasource on 'Tableau' Server/Cloud
- [`tbs_custom_sql_graphql()`](https://PrigasG.github.io/twbparser/reference/tbs_custom_sql_graphql.md)
  : Custom SQL (Metadata API) for a published item
