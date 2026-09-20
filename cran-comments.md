## twbparser 0.5.1

### Changes since 0.5.0

* New `twb_sheet_spec()`: a full per-worksheet visualization spec (mark type,
  rows/columns shelves, dimensions vs. measures, marks-card encodings,
  tooltips, filters, sorts, axes) for understanding and rebuilding a
  visualization in another tool.
* New `twb_dashboard_charts()`: one row per worksheet placed on each
  dashboard, with mark type, fields, tooltip summary, and layout position.
* New `parse_twb()` batch export: parse a `.twb`/`.twbx` workbook and write a
  structured report to disk (`report.txt`, one CSV per key table, a
  plain-text replication brief, and the field dependency graph as GraphML).
* Removed the non-functional `tbs_publish_info()` and
  `tbs_custom_sql_graphql()` stubs, which were exported and documented as
  Tableau Server/Cloud integration but never made a network request.
* `?TwbParser` rewritten to document the active-binding properties
  (`parser$summary`, `parser$overview`, ...) as the primary API and every
  `get_*()` method; `summary` is now correctly described as a read-only
  property.
* `validate_relationships()` loses its unused `strict` argument.
* Sheet/dashboard/story lookup by name no longer interpolates names into
  XPath predicates (names with quotes or brackets now match exactly).
* Removed dead internal code (`print_datasource_summary()`, the never-firing
  "safe getter" rebind block).
* README Quick Start uses base R instead of `fs`; lifecycle badge corrected
  to experimental.

## R CMD check results

To be verified with a full `R CMD check` before submission.
