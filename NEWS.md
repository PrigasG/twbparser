# twbparser 0.2.0 (2025-08-14)

## New
- **Custom SQL extraction (TWB/TWBX)**  
  `twb_custom_sql()` returns one row per custom-SQL relation with `relation_name`, `relation_type`, `custom_sql`, and a heuristic `is_custom_sql`.
- **Initial SQL extraction (TWB/TWBX)**  
  `twb_initial_sql()` pulls connection-level *Initial SQL* text (when present).
- **Published datasource detection (offline)**  
  `twb_published_refs()` flags datasources that likely reference *published* sources and surfaces the evidence.
- **Pretty Tableau formulas**  
  - `tableau_formula_pretty()` reflows/indents IF/ELSEIF/ELSE/END and CASE/WHEN/THEN blocks.
  - `prettify_calculated_fields()` adds a `formula_pretty` column to the calculated-fields tibble.
- **Optional Server/Cloud APIs (opt-in)**  
  - `tbs_publish_info()` (REST): site/project/URL/created/updated for a content item.
  - `tbs_custom_sql_graphql()` (Metadata API): returns Custom SQL text and context for published items.

## Changes
- **R6: `TwbParser`**
  - Now precomputes and exposes: `custom_sql`, `initial_sql`, `published_refs`.
  - `get_calculated_fields(pretty = FALSE, strip_brackets = FALSE, wrap = 100L)`  
    When `pretty = TRUE`, returns a cleaner table including `formula_pretty`.
- **Column rename for clarity**
  - In calculated fields output, `class` → **`calc_class`** (Tableau calculation class).

## Fixes & robustness
- Eliminated NSE NOTES by using `rlang::.data`.
- Removed `%||%` dependency; now use `dplyr::coalesce()` where appropriate.
- Build hygiene: `.lintr` ignored via `.Rbuildignore`.

## Docs & site
- Roxygen Markdown used consistently (backticks for inline code; `\verb{}` for angle-bracket tags).
- `_pkgdown.yml` updated to include new topics (SQL extraction, formatting, and optional server helpers).
- Function usage/arguments fully documented; R CMD check now passes cleanly.

## Compatibility notes
- **Server features are optional.** They require a REST auth token (e.g., PAT) or an admin-configured alternative (Connected App JWT). When not configured, helpers return empty tibbles or informative errors—offline TWB/TWBX parsing remains fully functional.
- If you relied on the `class` column in calculated fields, update code to use `calc_class`.

---

# twbparser 0.1.0

- Initial CRAN-ready release: TWB/TWBX parsing, relationships/joins/fields/parameters, lineage graphs, and vignettes.

