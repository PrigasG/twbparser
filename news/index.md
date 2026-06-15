# Changelog

## twbparser 0.5.0

### New features

- New fidelity extractors for faithful replication:
  [`twb_dashboard_size()`](https://prigasg.github.io/twbparser/reference/twb_dashboard_size.md)
  (page size and sizing mode),
  [`twb_formatting()`](https://prigasg.github.io/twbparser/reference/twb_formatting.md)
  (fonts, colours, number formats, and other style-rule formats), and
  [`twb_tooltips()`](https://prigasg.github.io/twbparser/reference/twb_tooltips.md)
  (plain-text worksheet tooltips). Each has a matching `TwbParser`
  getter (`get_dashboard_size()`, `get_formatting()`, `get_tooltips()`).
- [`run_twbparser_app()`](https://prigasg.github.io/twbparser/reference/run_twbparser_app.md)
  launches a bundled Shiny workbook inspector with a to-scale dashboard
  layout view, chart/`ggplot2` hints, parameter and formatting tabs, a
  replication brief, and CSV / R-scaffold exports.

### Bug fixes

- `parser$get_parameters()` (and `datasource_details$parameters`) now
  return the actual parameter fields via
  [`extract_parameters()`](https://prigasg.github.io/twbparser/reference/extract_parameters.md).
  Previously it returned a single row of datasource-level metadata, so
  most parameters were dropped and the overview count was wrong.

## twbparser 0.4.1

### Release polish

- `parser$summary` now prints calculated fields as readable formula
  blocks instead of escaped tibble cells.
- Replication brief calculated fields now retain one clean row per
  calculation and render formulas in fenced Tableau blocks.
- Removed timestamped backup scripts from `R/` before release.
- Hugging Face Space deployment now installs the tagged package release
  for reproducible rebuilds.

------------------------------------------------------------------------

## twbparser 0.4.0

CRAN release: 2026-04-14

### New features

#### Interactive workbook inspector

- [`run_twbparser_app()`](https://prigasg.github.io/twbparser/reference/run_twbparser_app.md)
  launches the bundled Shiny app for inspecting `.twb` and `.twbx`
  workbooks.
- The app supports local uploads, the bundled demo workbook, loading
  overlays for parse/export work, report tabs, CSV table downloads, and
  a replication brief download.
- `deploy/huggingface/` records the Docker Space deployment files and
  notes: `Dockerfile`, Space `README.md`, and `DEPLOYING.md`.
- `parser$summary` and `parser$report` now expose a structured workbook
  report used by both console output and the Shiny app.

#### Per-worksheet intelligence

- [`twb_sheet_shelves()`](https://prigasg.github.io/twbparser/reference/twb_sheet_shelves.md)
  — extract every field placed on rows, cols, or an encoding shelf
  (color, size, label, detail, tooltip) for one or all worksheets.
- [`twb_sheet_filters()`](https://prigasg.github.io/twbparser/reference/twb_sheet_filters.md)
  — extract worksheet-level filters including categorical member lists,
  range min/max, and include/exclude mode.
- [`twb_sheet_axes()`](https://prigasg.github.io/twbparser/reference/twb_sheet_axes.md)
  — extract per-axis configuration: reversed, include-zero, and scale
  type (linear, log, …).
- [`twb_sheet_sorts()`](https://prigasg.github.io/twbparser/reference/twb_sheet_sorts.md)
  — extract sort directives with sort direction and method (field
  aggregate, alphabetic, manual, data-source order).
- All four are exposed on `TwbParser` as `get_sheet_shelves()`,
  `get_sheet_filters()`, `get_sheet_axes()`, `get_sheet_sorts()` and as
  no-parens active bindings (`parser$sheet_shelves`, etc.).

#### Per-dashboard intelligence

- [`twb_dashboard_sheets()`](https://prigasg.github.io/twbparser/reference/twb_dashboard_sheets.md)
  — list every worksheet placed on a dashboard with zone id and pixel
  position (x, y, w, h).
- [`twb_dashboard_layout()`](https://prigasg.github.io/twbparser/reference/twb_dashboard_layout.md)
  — full zone tree including parent zone id, component type (worksheet /
  filter / container / …), layout type (tiled / floating), and pixel
  bounds.
- [`twb_dashboard_actions()`](https://prigasg.github.io/twbparser/reference/twb_dashboard_actions.md)
  — extract filter and URL actions with source and target sheets, run-on
  trigger type, and URL value.
- All three are exposed on `TwbParser` as `get_dashboard_sheets()`,
  `get_dashboard_layout()`, `get_dashboard_actions()` and as active
  bindings.

### Bug fixes

- [`plot_relationship_graph()`](https://prigasg.github.io/twbparser/reference/plot_relationship_graph.md):
  fixed edge direction — the `from` vertex was incorrectly built from
  `right_field` instead of `left_field`.
- [`plot_source_join_graph()`](https://prigasg.github.io/twbparser/reference/plot_source_join_graph.md):
  fixed reference to non-existent columns `left_source` /
  `right_source`; replaced with `left_table` / `right_table`.
- [`infer_implicit_relationships()`](https://prigasg.github.io/twbparser/reference/infer_implicit_relationships.md):
  added deduplication before the field-name self-join to prevent
  Cartesian explosion when many tables share a field name; added
  `relationship = "many-to-many"` to suppress the dplyr 1.1+ warning.
- Fixed `integer_` typo in `insights.R` that caused errors when parsing
  dashboard zone dimensions.

### Internal

- Canonical `.twb_clean_table()` and `.twb_clean_field()` helpers added
  to `utils.R`, replacing four independent copies scattered across
  `fields.R`, `calculated_fields.R`, `relationships.R`, `joins.R`, and
  `dependency_graph.R`.
- `.twb_clean_field()` now correctly strips Tableau column-instance
  prefixes (`none:Category:nk` → `Category`) and returns unnamed
  vectors.

------------------------------------------------------------------------

## twbparser 0.3.1

CRAN release: 2025-12-10

- Remove use of [`unlockBinding()`](https://rdrr.io/r/base/bindenv.html)
  in internal TwbParser active-binding helpers. This avoids CRAN’s
  “possibly unsafe call” NOTE while keeping the same user-facing
  behaviour for no-parens properties (overview, pages, datasources,
  etc.).

------------------------------------------------------------------------

## twbparser 0.3.0

- Added a `NEWS.md` file to track changes to the package.
