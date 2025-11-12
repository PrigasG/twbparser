# twbparser

[![Tableau:
TWB/TWBX](https://img.shields.io/badge/Tableau-TWB%2FTWBX-blue)](https://prigasg.github.io/twbparser/)

[![R-CMD-check](https://github.com/PrigasG/twbparser/actions/workflows/R-CMD-check.yaml/badge.svg?branch=master)](https://github.com/PrigasG/twbparser/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/PrigasG/twbparser/actions/workflows/pkgdown.yaml/badge.svg?branch=master)](https://prigasg.github.io/twbparser/)
[![Codecov](https://codecov.io/gh/PrigasG/twbparser/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PrigasG/twbparser)
[![License:MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://github.com/PrigasG/twbparser/blob/master/LICENSE)
[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)

Parse Tableau **TWB/TWBX** files in R: extract **datasources, joins,
relationships, fields, and calculated fields**, plus inspect and unpack
**TWBX** assets. Built for large workbooks and Shiny integration.

## Features

- **TWB/TWBX**: open packaged workbooks and auto‑extract the largest
  `.twb`
- **Relationships & joins**: parse legacy joins and modern (2020.2+)
  relationships
- **Calculated fields / parameters**: list formulas, datatypes, roles,
  and parameter metadata
- **Datasources**: connection classes/targets, inferred locations, field
  counts
- **Dependency graph**: build/plot field dependency DAGs
- **TWBX assets**: list/extract images, extracts, text files, etc.
- **Ergonomics**: `parser$summary` (no parens), `parser$overview`,
  `parser$pages`, `parser$pages_summary`

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

With a “.twbx” file

``` r
parser <- TWBParser$new("path/to/workbook.twbx")

# Inspect manifest
parser$twbx_manifest
```

Peek inside

``` r
# Datasources / parameters / all sources
parser$get_datasources()
parser$get_parameters()
parser$get_datasources_all()

# Fields and calculated fields (parameters excluded by default)
parser$get_fields()
parser$get_calculated_fields(pretty = TRUE, wrap = 120)
```

Page insights (dashboards, worksheets, stories)

``` r
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

Relationships/Joins

``` r
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

``` r
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

- R Shiny: Build an interactive dashboard showing calculations, filters,
  and DAG visualizations.
- Power BI: Export calculated field logic to replicate measures in DAX.
- Data lineage: Combine with DiagrammeR or visNetwork for workflow
  diagrams.

## What’s new (0.3.0)

- Page insights (pages, composition, summaries), filter positions on
  dashboards
- No-parens parser\$summary plus read-only properties (overview, pages,
  pages_summary, dashboard_summary)
- Calculated fields exclude parameters by default; opt-in with
  include_parameters = TRUE

## Contributing

1.  Fork the repo
2.  Create a feature branch (git checkout -b feature/new-feature)
3.  Commit changes (git commit -m ‘Add new feature’)
4.  Push branch (git push origin feature/new-feature)
5.  Open a Pull Request

## License

This package is licensed under the MIT License — see the LICENSE file
for details.
