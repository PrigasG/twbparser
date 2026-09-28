# Audit a folder of Tableau workbooks

Batch parses `.twb` and `.twbx` files and returns migration-oriented
inventory tables. The summary table is one row per workbook; detail
tables retain workbook identity so they can be filtered or joined
downstream.

## Usage

``` r
audit_tableau_folder(
  path,
  recursive = TRUE,
  pattern = NULL,
  write_csv = FALSE,
  output_dir = NULL
)
```

## Arguments

- path:

  Folder containing Tableau workbooks, or a character vector of `.twb` /
  `.twbx` files.

- recursive:

  Logical; search folders recursively. Default `TRUE`.

- pattern:

  Optional regular expression applied to file basenames.

- write_csv:

  Logical; write each returned table as a CSV file. Default `FALSE`.

- output_dir:

  Directory for CSV output when `write_csv = TRUE`.

## Value

A named list with class `twbparser_audit` containing:

- workbooks:

  One row per parsed workbook with counts and migration complexity
  flags.

- datasources:

  Datasource inventory across parsed workbooks.

- calculated_fields:

  Calculated field complexity across workbooks.

- field_usage:

  Worksheet field usage across workbooks.

- lineage_edges:

  Combined lineage edge table across workbooks.

- issues:

  Files that could not be parsed and their error messages.

## Examples

``` r
demo <- system.file("extdata", package = "twbparser")
if (nzchar(demo)) {
  audit <- audit_tableau_folder(demo)
  audit$workbooks
}
#> Warning: Circular dependencies detected in calculated fields. dep_depth is set to NA for all fields.
#> Warning: Circular dependencies detected in calculated fields. dep_depth is set to NA for all fields.
#> # A tibble: 3 × 28
#>   workbook  file  file_type worksheets dashboards stories datasources parameters
#>   <chr>     <chr> <chr>          <int>      <int>   <int>       <int>      <int>
#> 1 rebuild_… /hom… twb                2          1       0           0          2
#> 2 test_for… /hom… twb                1          0       0           2          0
#> 3 test_for… /hom… twbx               1          0       0           1          0
#> # ℹ 20 more variables: raw_fields <int>, calculated_fields <int>,
#> #   relationships <int>, inferred_relationships <int>, joins <int>,
#> #   worksheet_filters <int>, dashboard_actions <int>, custom_sql_blocks <int>,
#> #   initial_sql_blocks <int>, published_refs <int>, packaged_assets <int>,
#> #   has_lod_calcs <lgl>, has_table_calcs <lgl>, has_custom_sql <lgl>,
#> #   has_dashboard_actions <lgl>, has_published_refs <lgl>,
#> #   has_floating_layout <lgl>, has_packaged_extracts <lgl>, …
```
