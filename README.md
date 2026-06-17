# twbparser <a href="https://prigasg.github.io/twbparser/"><img src="man/figures/README-logo.png" alt="twbparser logo" align="right" height="120"/></a>

[![Tableau: TWB/TWBX](https://img.shields.io/badge/Tableau-TWB%2FTWBX-blue)](https://prigasg.github.io/twbparser/)

[![R-CMD-check](https://github.com/PrigasG/twbparser/actions/workflows/R-CMD-check.yaml/badge.svg?branch=master)](https://github.com/PrigasG/twbparser/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/PrigasG/twbparser/actions/workflows/pkgdown.yaml/badge.svg?branch=master)](https://prigasg.github.io/twbparser/)
[![Codecov](https://codecov.io/gh/PrigasG/twbparser/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PrigasG/twbparser)
[![License:MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://github.com/PrigasG/twbparser/blob/master/LICENSE)
[![Lifecycle:stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

Parse Tableau **TWB/TWBX** files in R: extract **datasources, joins, relationships, fields, calculated fields, worksheet configuration, and dashboard structure**, plus inspect and unpack **TWBX** assets. Built for large workbooks and Shiny integration.

## Features

-   **TWB/TWBX**: open packaged workbooks and auto‑extract the largest `.twb`
-   **Relationships & joins**: parse legacy joins and modern (2020.2+) relationships
-   **Calculated fields / parameters**: list formulas, datatypes, roles, and parameter metadata
-   **Datasources**: connection classes/targets, inferred locations, field counts
-   **Worksheet intelligence**: shelves (rows/cols/encodings), filters, axes, and sorts per worksheet
-   **Dashboard intelligence**: sheet positions, full zone layout tree, and filter/URL actions
-   **Replication fidelity**: dashboard sizing, style formatting, palette mappings, and worksheet tooltips
-   **Dependency graph**: build/plot field dependency DAGs
-   **TWBX assets**: list/extract images, extracts, text files, etc.
-   **Workbook report**: `parser$summary` / `parser$report` provide a structured inspection summary for console use or UI rendering
-   **Interactive app**: launch the bundled Shiny workbook inspector locally or deploy it as a Docker Space


## Installation

``` r
# Install from GitHub (using pak)
install.packages("pak")
pak::pak("PrigasG/twbparser")

# Or using devtools
install.packages("devtools")
devtools::install_github("PrigasG/twbparser")
```

## Quick Start

Summary for twb workbook

``` r
library(twbparser)
library(fs)

# Parse workbook
path <- fs::path_abs("path/to/workbook.twbx")
stopifnot(file.exists(path))

parser <- TwbParser$new(path)

# summary (prints)
parser$summary

# summary (one row tibble)
parser$overview
```

## Interactive Shiny App

Launch the bundled workbook inspector from R:

``` r
library(twbparser)

run_twbparser_app()
```

The app lets users upload `.twb` / `.twbx` files, shows a clean loading overlay
while parsing, and organizes the workbook report into tabs for overview, pages,
filters, shelves, fields, datasources, calculations, SQL, TWBX assets, and
validation, with dedicated views for dashboard layout, chart hints, formatting,
and tooltips.

For Hugging Face Docker Space deployment, use this access:
[`twbparser-inspector`](https://huggingface.co/spaces/Prigas89/twbparser-inspector):



``` r
parser <- TwbParser$new("path/to/workbook.twbx")

# Inspect manifest
parser$twbx_manifest

```

Peek inside

```r
# Datasources / parameters / all sources
parser$get_datasources()
parser$get_parameters()
parser$get_datasources_all()

# Fields and calculated fields (parameters excluded by default)
parser$get_fields()
parser$get_calculated_fields(pretty = TRUE, wrap = 120)

```

Page insights (dashboards, worksheets, stories)

```r
# What pages exist?
parser$pages
# Or functional: twb_pages(parser)

# One-line summaries per page
parser$pages_summary

# What is on a page and where? (filters include x/y/w/h on dashboards)
parser$get_page_composition("Executive Dashboard")

# Filters across dashboards (with positions)
twb_dashboard_filters(parser)

# Chart/mark types per worksheet
twb_charts(parser)

# Colors and palettes referenced
twb_colors(parser)

```

Worksheet-level detail (new in 0.4.0)

```r
# Fields on rows, cols, color, size, label … per worksheet
parser$get_sheet_shelves()
# or: twb_sheet_shelves(parser, sheet = "Sales")

# Worksheet filters — categorical members, range min/max
parser$get_sheet_filters()

# Axis configuration — reversed, include-zero, scale type
parser$get_sheet_axes()

# Sort directives — direction and method
parser$get_sheet_sorts()
```

Dashboard structure (new in 0.4.0)

```r
# Which sheets are on a dashboard and where?
parser$get_dashboard_sheets()
# or: twb_dashboard_sheets(parser, dashboard = "Overview")

# Full zone tree — parent zones, component types, tiled vs floating
parser$get_dashboard_layout()

# Filter and URL actions — source/target sheets, trigger, URL
parser$get_dashboard_actions()
```

Replication fidelity (new in 0.5.0)

```r
# Declared dashboard page size and sizing mode
parser$get_dashboard_size()
# or: twb_dashboard_size(parser, dashboard = "Overview")

# Style-rule formats: fonts, number/date formats, shading, and palette maps
parser$get_formatting()
# or: twb_formatting(parser, scope = "worksheet")

# Plain-text worksheet tooltip content
parser$get_tooltips()
```

Relationships/Joins 

```r
library(dplyr)
library(tidyr)

rel_df <- parser$get_relationships() |>
  mutate(
    op   = replace_na(operator, "="),
    left = paste0(left_table,  ".", left_field),
    right= paste0(right_table, ".", right_field),
    Join = paste(left, op, right)
  ) |>
  select(Join, datasource_left = left_table, datasource_right = right_table,
         operator, everything(), -left, -right, -left_is_calc, -right_is_calc)

rel_df

```

Nice tabular view for calculated fields

```r
library(dplyr)

calcs <- parser$get_calculated_fields(pretty = TRUE, wrap = 120) |>
  select(Name = name, Formula = formula_pretty, Datasource = datasource)

# DT example optional:
# DT::datatable(calcs, escape = FALSE, rownames = FALSE,
#   options = list(scrollX = TRUE, pageLength = 50)) |>
#   DT::formatStyle("Formula", `white-space` = "pre", `font-family` = "monospace")

```


And graph objects (via igraph or ggraph) for visualization:

``` r
Rscript -e "twbparser::parse_twb('my_dashboard.twb', output_dir = 'results/')"
```

## Integration Examples

-   R Shiny: Build an interactive dashboard showing calculations, filters, and DAG visualizations.
-   Power BI: Export calculated field logic to replicate measures in DAX.
-   Data lineage: Combine with DiagrammeR or visNetwork for workflow diagrams.

## What's new (0.5.0)

- **Fidelity extractors**: `twb_dashboard_size()`, `twb_formatting()`, and `twb_tooltips()` expose dashboard sizing, fonts, number/date formats, palette mappings, and worksheet tooltip text.
- **App fidelity tabs**: the bundled Shiny inspector now includes formatting and tooltip tables alongside the dashboard wireframe and chart hints.
- **Parameter fix**: datasource details now return actual parameter fields via `extract_parameters()`.

## What's new (0.4.0)

- **Worksheet intelligence**: `twb_sheet_shelves()`, `twb_sheet_filters()`, `twb_sheet_axes()`, `twb_sheet_sorts()` — extract the full shelf configuration, filter predicates, axis settings, and sort rules for every worksheet
- **Dashboard intelligence**: `twb_dashboard_sheets()`, `twb_dashboard_layout()`, `twb_dashboard_actions()` — inspect which sheets appear where, the full zone hierarchy, and all filter/URL actions
- **Interactive Shiny app**: `run_twbparser_app()` launches a bundled workbook inspector with upload support, loading overlays, report tabs, and CSV/brief export in the deployed app.
- **Docker Space deployment**: `deploy/huggingface/` contains the Dockerfile, Space card, and deployment notes for hosting the Shiny app.
- **Workbook report**: `parser$summary` and `parser$report` expose a structured workbook report that powers both console output and the app.
- **Bug fixes**: corrected edge direction in `plot_relationship_graph()`, fixed column references in `plot_source_join_graph()`, eliminated Cartesian-product explosion in `infer_implicit_relationships()`

## Contributing

1.  Fork the repo
2.  Create a feature branch (git checkout -b feature/new-feature)
3.  Commit changes (git commit -m 'Add new feature')
4.  Push branch (git push origin feature/new-feature)
5.  Open a Pull Request

## License

This package is licensed under the MIT License — see the LICENSE file for details.
