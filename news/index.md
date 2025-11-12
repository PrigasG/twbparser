# Changelog

## twbparser 0.3.0

### New

- Page insights:
  - [`twb_pages()`](https://PrigasG.github.io/twbparser/reference/twb_pages.md):
    list all pages (dashboard / worksheet / story).
  - `twb_page_composition(x, name)`: what a page contains (worksheets,
    filters, legends, parameter controls, text, images) with `x/y/w/h`
    for dashboard zones when available.
  - [`twb_pages_summary()`](https://PrigasG.github.io/twbparser/reference/twb_pages_summary.md):
    per-page counts and quick descriptors.
  - [`twb_dashboards()`](https://PrigasG.github.io/twbparser/reference/twb_dashboards.md),
    [`twb_dashboard_filters()`](https://PrigasG.github.io/twbparser/reference/twb_dashboard_filters.md),
    [`twb_charts()`](https://PrigasG.github.io/twbparser/reference/twb_charts.md),
    [`twb_colors()`](https://PrigasG.github.io/twbparser/reference/twb_colors.md),
    [`twb_dashboard_summary()`](https://PrigasG.github.io/twbparser/reference/twb_dashboard_summary.md)
    helpers (also available as `TwbParser` getters).
- Ergonomics (active bindings; no API breaks):
  - `parser$summary` (no parentheses) prints the textual summary.
  - Read-only properties: `parser$overview`, `parser$pages`,
    `parser$pages_summary`, `parser$dashboard_summary`.
- Calculated fields:
  - Parameters are excluded by default in
    [`extract_calculated_fields()`](https://PrigasG.github.io/twbparser/reference/extract_calculated_fields.md)
    and `TwbParser$get_calculated_fields()`.
  - Opt-in via `include_parameters = TRUE`.

### Improvements

- [`summary()`](https://rdrr.io/r/base/summary.html) now includes
  dashboards and total filters; `get_overview()` returns a one-row
  tibble snapshot.
- Robust handling of dashboard zone positions and filter presentation
  hints.

### Internal

- New internal workers in `R/insights.R` (pure read-only functions) used
  by both R6 getters and functional wrappers.
- Documentation and vignette expanded with page-centric examples.

### Compatibility

- No breaking changes to existing exported functions or S3 classes.

------------------------------------------------------------------------

## twbparser 0.2.3 (2025-09-23)

CRAN release: 2025-09-30

### Fixes for CRAN resubmission

- Quoted software/API names in Title/Description per CRAN.
- Shipped tiny example files in inst/extdata and use system.file().
- Moved withr to Imports; removed fixed seeds inside functions.

------------------------------------------------------------------------

## twbparser 0.2.1 (2025-08-29)

### Fixes for CRAN resubmission

- Replaced a Unicode arrow in docs so the PDF manual builds on all
  platforms.
- Added `Depends: R (>= 4.2.0)` (native pipe placeholder usage).
- Ensured vignettes are built into the tarball (`inst/doc`) and vignette
  index is created at install.
- Spell-check pass; added `inst/WORDLIST` for domain terms (e.g., “TWB”,
  “TWBX”, “GraphQL”).

## twbparser 0.2.0 (2025-08-29)

- Initial CRAN submission.
- Core features: parse `.twb`/`.twbx`, extract datasources, parameters,
  fields & calculated fields, joins/relationships, and generate
  dependency graphs.

------------------------------------------------------------------------

## twbparser 0.2.0 (2025-08-14)

### New

- **Custom SQL extraction (TWB/TWBX)**  
  [`twb_custom_sql()`](https://PrigasG.github.io/twbparser/reference/twb_custom_sql.md)
  returns one row per custom-SQL relation with `relation_name`,
  `relation_type`, `custom_sql`, and a heuristic `is_custom_sql`.
- **Initial SQL extraction (TWB/TWBX)**  
  [`twb_initial_sql()`](https://PrigasG.github.io/twbparser/reference/twb_initial_sql.md)
  pulls connection-level *Initial SQL* text (when present).
- **Published datasource detection (offline)**  
  [`twb_published_refs()`](https://PrigasG.github.io/twbparser/reference/twb_published_refs.md)
  flags datasources that likely reference *published* sources and
  surfaces the evidence.
- **Pretty Tableau formulas**
  - [`tableau_formula_pretty()`](https://PrigasG.github.io/twbparser/reference/tableau_formula_pretty.md)
    reflows/indents IF/ELSEIF/ELSE/END and CASE/WHEN/THEN blocks.
  - [`prettify_calculated_fields()`](https://PrigasG.github.io/twbparser/reference/prettify_calculated_fields.md)
    adds a `formula_pretty` column to the calculated-fields tibble.
- **Optional Server/Cloud APIs (opt-in)**
  - [`tbs_publish_info()`](https://PrigasG.github.io/twbparser/reference/tbs_publish_info.md)
    (REST): site/project/URL/created/updated for a content item.
  - [`tbs_custom_sql_graphql()`](https://PrigasG.github.io/twbparser/reference/tbs_custom_sql_graphql.md)
    (Metadata API): returns Custom SQL text and context for published
    items.

### Changes

- **R6: `TwbParser`**
  - Now precomputes and exposes: `custom_sql`, `initial_sql`,
    `published_refs`.
  - `get_calculated_fields(pretty = FALSE, strip_brackets = FALSE, wrap = 100L)`  
    When `pretty = TRUE`, returns a cleaner table including
    `formula_pretty`.
- **Column rename for clarity**
  - In calculated fields output, `class` → **`calc_class`** (Tableau
    calculation class).

### Fixes & robustness

- Eliminated NSE NOTES by using
  [`rlang::.data`](https://rlang.r-lib.org/reference/dot-data.html).
- Removed `%||%` dependency; now use
  [`dplyr::coalesce()`](https://dplyr.tidyverse.org/reference/coalesce.html)
  where appropriate.
- Build hygiene: `.lintr` ignored via `.Rbuildignore`.

### Docs & site

- Roxygen Markdown used consistently (backticks for inline code;
  `\verb{}` for angle-bracket tags).
- `_pkgdown.yml` updated to include new topics (SQL extraction,
  formatting, and optional server helpers).
- Function usage/arguments fully documented; R CMD check now passes
  cleanly.

### Compatibility notes

- **Server features are optional.** They require a REST auth token
  (e.g., PAT) or an admin-configured alternative (Connected App JWT).
  When not configured, helpers return empty tibbles or informative
  errors—offline TWB/TWBX parsing remains fully functional.
- If you relied on the `class` column in calculated fields, update code
  to use `calc_class`.

------------------------------------------------------------------------

## twbparser 0.1.0

- Initial CRAN-ready release: TWB/TWBX parsing,
  relationships/joins/fields/parameters, lineage graphs, and vignettes.
