# Parse a Tableau workbook and write a batch export to disk

`parse_twb()` is a convenience wrapper for non-interactive use: it
parses a `.twb`/`.twbx` workbook with
[TwbParser](https://prigasg.github.io/twbparser/reference/TwbParser.md)
and writes a structured set of outputs into `output_dir` — a
human-readable report, one CSV per key table (including the rebuild kit:
unused fields, calculation build order, and parameter usage),
per-worksheet visualization specs (`sheet_specs.txt`), a plain-text
replication brief, and the field dependency graph as GraphML (readable
with `igraph`/`ggraph` or any GraphML tool).

## Usage

``` r
parse_twb(path, output_dir = "results", overwrite = FALSE, quiet = FALSE)
```

## Arguments

- path:

  Path to a `.twb` or `.twbx` file.

- output_dir:

  Directory to write outputs into. Created if needed (including
  parents).

- overwrite:

  If `FALSE` (default), refuse to write into an existing non-empty
  directory instead of mixing outputs. If `TRUE`, the previous outputs
  written by parse_twb in the directory are removed first so the export
  reflects the current workbook; unrelated files are left alone.

- quiet:

  If `TRUE`, suppress progress messages.

## Value

The normalized `output_dir`, invisibly.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
if (nzchar(twb) && file.exists(twb)) {
  out <- parse_twb(twb, output_dir = file.path(tempdir(), "twbparser-demo"),
                   quiet = TRUE)
  list.files(out)
}
#>  [1] "calc_build_order.csv"     "calculated_fields.csv"   
#>  [3] "custom_sql.csv"           "dashboards.csv"          
#>  [5] "datasources.csv"          "dependency_graph.graphml"
#>  [7] "fields.csv"               "joins.csv"               
#>  [9] "overview.csv"             "pages.csv"               
#> [11] "parameter_usage.csv"      "parameters.csv"          
#> [13] "relationships.csv"        "replication_brief.txt"   
#> [15] "report.txt"               "sheet_specs.txt"         
#> [17] "unused_fields.csv"       
```
