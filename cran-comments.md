## twbparser 0.4.0

### Changes since 0.3.1

* Added per-worksheet intelligence: `twb_sheet_shelves()`, `twb_sheet_filters()`,
  `twb_sheet_axes()`, `twb_sheet_sorts()` and matching `TwbParser` methods.
* Added per-dashboard intelligence: `twb_dashboard_sheets()`,
  `twb_dashboard_layout()`, `twb_dashboard_actions()` and matching `TwbParser`
  methods.
* Fixed graph edge direction bug in `plot_relationship_graph()`.
* Fixed column name bug in `plot_source_join_graph()`.
* Centralized duplicate field/table cleaning helpers into `utils.R`.

## R CMD check results

0 errors | 0 warnings | 0 notes
