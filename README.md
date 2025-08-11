# twbparser <img src="https://img.shields.io/badge/Tableau-TWB/TWBX-blue" align="right"/>

[![R-CMD-check](https://github.com/%3CUSER%3E/twbparser/actions/workflows/R-CMD-check.yaml/badge.svg?branch=master)](https://github.com/%3CUSER%3E/twbparser/actions/workflows/R-CMD-check.yaml) [![pkgdown](https://github.com/%3CUSER%3E/twbparser/actions/workflows/pkgdown.yaml/badge.svg?branch=master)](https://%3CUSER%3E.github.io/twbparser/) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)

Parse Tableau **TWB/TWBX** files in R: extract **datasources, joins, relationships, fields, and calculated fields**, plus inspect and unpack **TWBX** assets. Built for large workbooks and Shiny integration.

## Features

-   🧭 **TWB/TWBX**: open packaged workbooks and auto‑extract the largest `.twb`
-   🔗 **Relationships & joins**: parse legacy joins and modern (2020.2+) relationships
-   🧮 **Calculated fields / parameters**: list formulas, datatypes, roles, and parameter metadata
-   🗂️ **Datasources**: connection classes/targets, inferred locations, field counts
-   🧱 **Dependency graph**: build/plot field dependency DAGs
-   📦 **TWBX assets**: list/extract images, extracts, text files, etc.

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

Parse a ".twb" file

``` r
library(twbparser)

# Parse workbook
parser <- TWBParser$new("path/to/workbook.twb")

# Extract calculated fields
calc_fields <- parser$get_calculated_fields()

# View dependencies
deps <- parser$get_field_dependencies()

# Generate DAG
dag <- parser$generate_dag()
```

Parse a ".twbx" file

``` r
parser <- TWBParser$new("path/to/workbook.twbx")

# Inspect manifest
parser$twbx_manifest

# Extract data sources
sources <- parser$get_datasource_details()
```

And graph objects (via igraph or ggraph) for visualization:

``` r
Rscript -e "twbparser::parse_twb('my_dashboard.twb', output_dir = 'results/')"
```

## Integration Examples

-   R Shiny: Build an interactive dashboard showing calculations, filters, and DAG visualizations.
-   Power BI: Export calculated field logic to replicate measures in DAX.
-   Data lineage: Combine with DiagrammeR or visNetwork for workflow diagrams.


## Contributing

1.  Fork the repo
2.  Create a feature branch (git checkout -b feature/new-feature)
3.  Commit changes (git commit -m 'Add new feature')
4.  Push branch (git push origin feature/new-feature)
5.  Open a Pull Request

## License

This package is licensed under the MIT License — see the LICENSE file for details.
