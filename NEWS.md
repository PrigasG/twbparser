# twbparser 0.5.1

## New features

* New `twb_sheet_spec()`: a full per-worksheet visualization spec — mark type,
  rows/columns shelves in order, dimensions vs. measures, every marks-card
  encoding (color, size, label, detail, shape, tooltip, ...), tooltip
  configuration, plus the sheet's filters, sorts, and axes. It reduces a
  worksheet to everything needed to understand and rebuild its visualization
  in another tool (also available as `parser$sheet_spec` /
  `parser$get_sheet_spec()`).
* New `twb_dashboard_charts()`: one row per worksheet placed on each
  dashboard — mark type, fields, tooltip summary, and layout position — so you
  can see at a glance what graphs a dashboard page uses (also available as
  `parser$dashboard_charts` / `parser$get_dashboard_charts()`).
* New `parse_twb()` batch export: parse a `.twb`/`.twbx` workbook and write a
  structured report to disk — `report.txt`, one CSV per key table,
  per-worksheet visualization specs (`sheet_specs.txt`), a plain-text
  replication brief, and the field dependency graph as GraphML.
  This delivers the `parse_twb()` entry point the README previously documented
  but which did not exist.

## Breaking changes

* Removed the `tbs_publish_info()` and `tbs_custom_sql_graphql()` stubs. They
  were exported and documented as querying Tableau Server/Cloud, but never
  made a network request and always returned empty tibbles. Tableau
  Server/Cloud integration is planned as a real feature; the premature stubs
  are gone rather than silently returning no data.
* `validate_relationships()` loses its unused `strict` argument, which was
  documented as reserved and never implemented.

## Documentation

* `?TwbParser` rewritten from scratch: it now documents every active-binding
  property (`parser$summary`, `parser$overview`, `parser$datasources`, ...)
  as the primary API alongside every `get_*()` method. `summary` is correctly
  described as a read-only property — the previously documented
  `parser$summary()` call form never worked at runtime.
* README Quick Start no longer requires the `fs` package (uses base R), and
  the lifecycle badge is corrected to experimental while the API is settling.

## Bug fixes

* Sheet/dashboard/story lookup by name no longer interpolates the name into
  an XPath predicate. Names containing quotes or brackets (e.g. "Bob's
  Dashboard") now match exactly instead of being mangled or silently missing
  (new internal `.twb_find_named()` / `.twb_find_all_named()` helpers).
* Removed dead code: the never-firing "safe getter" rebind block in
  `twb_install_active_properties()` and the uncalled internal
  `print_datasource_summary()`.
* `parse_twb(overwrite = TRUE)` now removes parse_twb's own previous output
  files before writing, so the export directory reflects the current workbook
  instead of mixing stale and fresh files. Unrelated files are left alone.

## Tests

* New tests for the `parse_twb()` batch export and Shiny app smoke tests (the
  bundled app file parses cleanly and the app object builds headlessly).

# twbparser 0.5.0

## New features

* New fidelity extractors for faithful replication: `twb_dashboard_size()`
  (page size and sizing mode), `twb_formatting()` (fonts, colours, number
  formats, and other style-rule formats), and `twb_tooltips()` (plain-text
  worksheet tooltips). Each has a matching `TwbParser` getter
  (`get_dashboard_size()`, `get_formatting()`, `get_tooltips()`).
* `run_twbparser_app()` launches a bundled Shiny workbook inspector with a
  to-scale dashboard layout view, chart/`ggplot2` hints, parameter and
  formatting tabs, a replication brief, and CSV / R-scaffold exports.

## Bug fixes

* `parser$get_parameters()` (and `datasource_details$parameters`) now return
  the actual parameter fields via `extract_parameters()`. Previously it
  returned a single row of datasource-level metadata, so most parameters were
  dropped and the overview count was wrong.

# twbparser 0.4.1

## Release polish

* `parser$summary` now prints calculated fields as readable formula blocks
  instead of escaped tibble cells.
* Replication brief calculated fields now retain one clean row per calculation
  and render formulas in fenced Tableau blocks.
* Removed timestamped backup scripts from `R/` before release.
* Hugging Face Space deployment now installs the tagged package release for
  reproducible rebuilds.

---

# twbparser 0.4.0

## New features

### Interactive workbook inspector

* `run_twbparser_app()` launches the bundled Shiny app for inspecting `.twb`
  and `.twbx` workbooks.
* The app supports local uploads, the bundled demo workbook, loading overlays
  for parse/export work, report tabs, CSV table downloads, and a replication
  brief download.
* `deploy/huggingface/` records the Docker Space deployment files and notes:
  `Dockerfile`, Space `README.md`, and `DEPLOYING.md`.
* `parser$summary` and `parser$report` now expose a structured workbook report
  used by both console output and the Shiny app.

### Per-worksheet intelligence

* `twb_sheet_shelves()` — extract every field placed on rows, cols, or an
  encoding shelf (color, size, label, detail, tooltip) for one or all worksheets.
* `twb_sheet_filters()` — extract worksheet-level filters including categorical
  member lists, range min/max, and include/exclude mode.
* `twb_sheet_axes()` — extract per-axis configuration: reversed, include-zero,
  and scale type (linear, log, …).
* `twb_sheet_sorts()` — extract sort directives with sort direction and method
  (field aggregate, alphabetic, manual, data-source order).
* All four are exposed on `TwbParser` as `get_sheet_shelves()`,
  `get_sheet_filters()`, `get_sheet_axes()`, `get_sheet_sorts()` and as
  no-parens active bindings (`parser$sheet_shelves`, etc.).

### Per-dashboard intelligence

* `twb_dashboard_sheets()` — list every worksheet placed on a dashboard with
  zone id and pixel position (x, y, w, h).
* `twb_dashboard_layout()` — full zone tree including parent zone id, component
  type (worksheet / filter / container / …), layout type (tiled / floating),
  and pixel bounds.
* `twb_dashboard_actions()` — extract filter and URL actions with source and
  target sheets, run-on trigger type, and URL value.
* All three are exposed on `TwbParser` as `get_dashboard_sheets()`,
  `get_dashboard_layout()`, `get_dashboard_actions()` and as active bindings.

## Bug fixes

* `plot_relationship_graph()`: fixed edge direction — the `from` vertex was
  incorrectly built from `right_field` instead of `left_field`.
* `plot_source_join_graph()`: fixed reference to non-existent columns
  `left_source` / `right_source`; replaced with `left_table` / `right_table`.
* `infer_implicit_relationships()`: added deduplication before the field-name
  self-join to prevent Cartesian explosion when many tables share a field name;
  added `relationship = "many-to-many"` to suppress the dplyr 1.1+ warning.
* Fixed `integer_` typo in `insights.R` that caused errors when parsing
  dashboard zone dimensions.

## Internal

* Canonical `.twb_clean_table()` and `.twb_clean_field()` helpers added to
  `utils.R`, replacing four independent copies scattered across `fields.R`,
  `calculated_fields.R`, `relationships.R`, `joins.R`, and `dependency_graph.R`.
* `.twb_clean_field()` now correctly strips Tableau column-instance prefixes
  (`none:Category:nk` → `Category`) and returns unnamed vectors.

---

# twbparser 0.3.1

* Remove use of `unlockBinding()` in internal TwbParser active-binding helpers.
  This avoids CRAN's "possibly unsafe call" NOTE while keeping the same
  user-facing behaviour for no-parens properties (overview, pages, datasources, etc.).

---

# twbparser 0.3.0

* Added a `NEWS.md` file to track changes to the package.
