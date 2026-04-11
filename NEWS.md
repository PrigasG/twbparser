# twbparser 0.4.0

## New features

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
