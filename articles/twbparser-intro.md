# twbparser-intro

``` r

library(twbparser)
ok <- FALSE
twb_path <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
if (nzchar(twb_path) && file.exists(twb_path)) {
parser <- TwbParser$new(twb_path)
ok <- TRUE
} else {
cat("> Demo .twb not found in installed package. Skipping executable examples.\n")
}
#> TWB loaded: test_for_wenjie.twb
#> TWB parsed and ready
```

## Introduction

`twbparser` parses Tableau `.twb` and `.twbx` workbooks and exposes
datasources, relationships, joins, fields, calculated fields, and TWBX
assets. It also provides page-centric insights — dashboards, worksheets,
stories, their composition, filter positions, chart types, and
colors/palettes — as well as per-worksheet shelf/filter/axis/sort
details and per-dashboard zone layout and actions. This vignette
demonstrates common use cases.

## Parse a Tableau Workbook

``` r

parser$summary
#> TWB PARSER SUMMARY
#> ------------------
#> File:                test_for_wenjie.twb
#> Datasources:         2
#> Parameters:          0
#> Worksheets:          1
#> Dashboards:          0
#> Stories:             0
#> Relationships:       1
#> Calculated fields:   1
#> Raw fields:          55
#> Worksheet filters:   0
#> Dashboard filters:   0
#> Custom SQL blocks:   0
#> Initial SQL blocks:  0
#> 
#> Datasources
#> -----------
#> # A tibble: 2 × 4
#>   datasource_name                        connection_type field_count
#>   <chr>                                  <chr>                 <int>
#> 1 Municipal_Boundaries_of_NJ (1)         ogrdirect                 0
#> 2 federated.0grgaor1pd01yy1f0yr380of1ags federated               110
#>   location                                     
#>   <chr>                                        
#> 1 Shapefile: Municipal_Boundaries_of_NJ (1).zip
#> 2 Federated: <unknown>                         
#> 
#> Pages
#> -----
#> # A tibble: 1 × 6
#>   page_type name    mark_types n_filters n_legends n_parameter_controls
#>   <chr>     <chr>   <chr>          <int>     <int>                <int>
#> 1 worksheet Sheet 1 ""                 0         0                    0
#> 
#> Worksheet Shelves
#> -----------------
#> # A tibble: 1 × 3
#>   sheet   shelves                         
#>   <chr>   <chr>                           
#> 1 Sheet 1 color, cols, geometry, lod, rows
#>   fields                                                                        
#>   <chr>                                                                         
#> 1 Calculation_2139209847776120832, Longitude (generated), Geometry, counts, Lat…
#> 
#> Worksheet Filters
#> -----------------
#> No worksheet filters found.
#> 
#> Dashboard Filters
#> -----------------
#> No dashboard filters found.
#> 
#> Calculated Fields
#> -----------------
#> 1. no data
#> Datasource:          federated.0grgaor1pd01yy1f0yr380of1ags
#> Type:                string dimension
#> Calculation:         raw
#> Table calc:          FALSE
#> Dependencies:        1
#> Formula:
#>   if ISNULL([counts])
#>   THEN "missing"
#>   ELSE "available"
#>   END
#> 
#> 
#> SQL
#> ---
#> No custom or initial SQL found.
parser$overview
#> # A tibble: 1 × 9
#>   file         datasources parameters relationships calculated_fields raw_fields
#>   <chr>              <int>      <int>         <int>             <int>      <int>
#> 1 test_for_we…           2          0             1                 1         55
#> # ℹ 3 more variables: inferred_relationships <int>, dashboards <int>,
#> #   total_filters <int>
```

## Extracting Datasources and Parameters

``` r

datasources <- parser$get_datasources()
parameters <- parser$get_parameters()

print(head(datasources))
#> # A tibble: 2 × 10
#>   datasource     primary_table connection_id connection_caption connection_class
#>   <chr>          <chr>         <chr>         <chr>              <chr>           
#> 1 Municipal_Bou… [Municipal_B… ogrdirect.07… Municipal_Boundar… ogrdirect       
#> 2 Sheet1         [Sheet1$]     excel-direct… test_county        excel-direct    
#> # ℹ 5 more variables: connection_target <chr>, datasource_name <chr>,
#> #   field_count <int>, connection_type <chr>, location <chr>
print(head(parameters))
#> # A tibble: 0 × 0
```

## Fields and calculated fields

Parameters are excluded by default from calculated fields; opt-in via
`include_parameters = TRUE`.

``` r

head(parser$get_fields())
#> # A tibble: 6 × 10
#>   datasource        name  caption datatype role  semantic_role table table_clean
#>   <chr>             <chr> <chr>   <chr>    <chr> <chr>         <chr> <chr>      
#> 1 federated.0grgao… OBJE… NA      integer  NA    NA            NA    NA         
#> 2 federated.0grgao… MUN   NA      string   NA    NA            NA    NA         
#> 3 federated.0grgao… COUN… NA      string   NA    NA            NA    NA         
#> 4 federated.0grgao… MUN_… NA      string   NA    NA            NA    NA         
#> 5 federated.0grgao… MUN_… NA      string   NA    NA            NA    NA         
#> 6 federated.0grgao… NAME  NA      string   NA    NA            NA    NA         
#> # ℹ 2 more variables: field_clean <chr>, is_parameter <lgl>
head(parser$get_calculated_fields(pretty = TRUE, wrap = 120))
#> # A tibble: 1 × 9
#>   datasource        name  datatype role  is_table_calc calc_class formula_pretty
#>   <chr>             <chr> <chr>    <chr> <lgl>         <chr>      <chr>         
#> 1 federated.0grgao… no d… string   dime… FALSE         tableau    "if ISNULL([c…
#> # ℹ 2 more variables: tableau_internal_name <chr>, table_clean <chr>
```

## Page insights

List all pages and summarize each page

``` r

twb_pages(parser)
#> # A tibble: 1 × 2
#>   page_type name   
#>   <chr>     <chr>  
#> 1 worksheet Sheet 1
twb_pages_summary(parser)
#> # A tibble: 1 × 6
#>   page_type name    mark_types n_filters n_legends n_parameter_controls
#>   <chr>     <chr>   <chr>          <int>     <int>                <int>
#> 1 worksheet Sheet 1 ""                 0         0                    0
```

Inspect what a specific page contains

``` r



pg <- twb_pages(parser)
nm <- if (nrow(pg)) pg$name[[1]] else NA_character_
if (!is.na(nm)) {
  parser$get_page_composition(nm)
}
#> # A tibble: 1 × 11
#>   page_type page_name component_type zone_id target field presentation     x
#>   <chr>     <chr>     <chr>          <chr>   <chr>  <chr> <chr>        <int>
#> 1 worksheet Sheet 1   mark_type      NA      NA     NA    NA              NA
#> # ℹ 3 more variables: y <int>, w <int>, h <int>
```

Filters and their positions across dashboards

``` r

twb_dashboard_filters(parser)
#> # A tibble: 0 × 9
#> # ℹ 9 variables: dashboard <chr>, zone_id <chr>, zone_type <chr>, field <chr>,
#> #   presentation <chr>, x <int>, y <int>, w <int>, h <int>
```

Chart (mark) types per worksheet and colors/palettes

``` r

twb_charts(parser)
#> # A tibble: 1 × 2
#>   worksheet mark_types
#>   <chr>     <chr>     
#> 1 Sheet 1   ""
twb_colors(parser)
#> # A tibble: 0 × 4
#> # ℹ 4 variables: kind <chr>, detail <chr>, scope <chr>, label <chr>
```

## Worksheet intelligence

Each of the four functions below accepts an optional `sheet` argument to
restrict output to a single worksheet.

### Shelves — what fields are on rows, cols, and encodings?

``` r

shelves <- twb_sheet_shelves(parser)
head(shelves)
#> # A tibble: 6 × 7
#>   sheet   shelf    field_ref   field_instance field_clean datasource aggregation
#>   <chr>   <chr>    <chr>       <chr>          <chr>       <chr>      <chr>      
#> 1 Sheet 1 color    [federated… none:Calculat… Calculatio… federated… NA         
#> 2 Sheet 1 cols     [federated… Longitude (ge… Longitude … federated… NA         
#> 3 Sheet 1 geometry [federated… clct:Geometry… Geometry    federated… NA         
#> 4 Sheet 1 lod      [federated… clct:Geometry… Geometry    federated… NA         
#> 5 Sheet 1 lod      [federated… none:counts:qk counts      federated… NA         
#> 6 Sheet 1 rows     [federated… Latitude (gen… Latitude (… federated… NA
```

The `shelf` column distinguishes `"rows"`, `"cols"`, `"color"`,
`"size"`, `"label"`, `"detail"`, and `"tooltip"`.

### Filters

``` r

filters <- twb_sheet_filters(parser)
head(filters)
#> # A tibble: 0 × 9
#> # ℹ 9 variables: sheet <chr>, field_ref <chr>, field_clean <chr>,
#> #   datasource <chr>, filter_class <chr>, include_mode <chr>, members <chr>,
#> #   range_min <chr>, range_max <chr>
```

Categorical filters include a comma-separated `members` column; range
filters populate `range_min` / `range_max`.

### Axis configuration

``` r

axes <- twb_sheet_axes(parser)
head(axes)
#> # A tibble: 0 × 7
#> # ℹ 7 variables: sheet <chr>, axis <chr>, field_ref <chr>, field_clean <chr>,
#> #   scale_type <chr>, reversed <lgl>, include_zero <lgl>
```

### Sort directives

``` r

sorts <- twb_sheet_sorts(parser)
head(sorts)
#> # A tibble: 0 × 6
#> # ℹ 6 variables: sheet <chr>, field_ref <chr>, field_clean <chr>,
#> #   datasource <chr>, sort_order <chr>, sort_by <chr>
```

### Visualization spec — everything needed to rebuild a sheet

``` r

spec <- twb_sheet_spec(parser, sheet = "Sheet 1")
spec
#> Sheet: Sheet 1 
#>   Mark type: map
#>   Datasources: federated.0grgaor1pd01yy1f0yr380of1ags 
#>   Rows (1): Latitude (generated)
#>   Cols (1): Longitude (generated)
#>   Dimensions (5): Latitude (generated), Longitude (generated), Calculation_2139209847776120832, Geometry, counts
#>   Measures: (none)
#>   Encodings: color -> Calculation_2139209847776120832; geometry -> Geometry; lod -> Geometry; lod -> counts 
#>   Tooltips:none
#>   Filters: 0  Sorts: 0  Axes: 0
```

[`twb_sheet_spec()`](https://prigasg.github.io/twbparser/reference/twb_sheet_spec.md)
returns one spec per worksheet: the mark type, the rows/columns shelves
in order, the dimensions and measures in play, every marks-card
encoding, the tooltip configuration, and the sheet’s filters, sorts, and
axes — the full blueprint for rebuilding the visualization in another
tool. `parser$sheet_spec` exposes the same specs as a property.

### Rebuild kit — what do you actually need to recreate?

Three helpers answer the questions that come up when rebuilding a
workbook in another tool. They are demonstrated on the bundled
`rebuild_kit.twb` fixture, which contains chained calculations, a
circular pair, parameters, and deliberately unused fields.

``` r

kit_ok <- FALSE
kit_path <- system.file("extdata", "rebuild_kit.twb", package = "twbparser")
if (nzchar(kit_path) && file.exists(kit_path)) {
  kit <- TwbParser$new(kit_path)
  kit_ok <- TRUE
}
#> TWB loaded: rebuild_kit.twb
#> TWB parsed and ready
```

``` r

# Fields defined but never used anywhere: the safe-to-drop list
twb_unused_fields(kit)
#> # A tibble: 3 × 7
#>   datasource field_type name      tableau_internal_name datatype role  is_hidden
#>   <chr>      <chr>      <chr>     <chr>                 <chr>    <chr> <lgl>    
#> 1 sales-data calculated Unused C… [Calculation_0003]    real     meas… NA       
#> 2 sales-data parameter  Unused P… [Parameter 2]         string   dime… NA       
#> 3 sales-data raw        Unused F… [Unused Field]        string   dime… FALSE
```

``` r

# Calculations in creation order — "Adjusted Ratio" comes after "Profit Ratio";
# the Cycle A/B pair is flagged instead of silently misordered
twb_calc_build_order(kit)
#> Warning: Circular dependencies detected among calculated fields: Cycle A, Cycle
#> B. `build_order` is NA for these fields; break the cycle before rebuilding.
#> # A tibble: 5 × 8
#>   build_order datasource name           tableau_internal_name formula depends_on
#>         <int> <chr>      <chr>          <chr>                 <chr>   <chr>     
#> 1           1 sales-data Profit Ratio   [Calculation_0001]    [Profi… NA        
#> 2           2 sales-data Unused Calc    [Calculation_0003]    [Sales… NA        
#> 3           3 sales-data Adjusted Ratio [Calculation_0002]    [Profi… Profit Ra…
#> 4          NA sales-data Cycle A        [Calculation_0004]    [Cycle… Cycle B   
#> 5          NA sales-data Cycle B        [Calculation_0005]    [Cycle… Cycle A   
#> # ℹ 2 more variables: n_calc_deps <int>, is_cyclic <lgl>
```

``` r

# Where each parameter value flows: formulas, shelves, filters, dashboards
twb_parameter_usage(kit)
#> # A tibble: 2 × 6
#>   parameter datasource datatype current_value context location      
#>   <chr>     <chr>      <chr>    <chr>         <chr>   <chr>         
#> 1 Top N     sales-data integer  5             filter  Profit Detail 
#> 2 Top N     sales-data integer  5             formula Adjusted Ratio
```

[`twb_unused_fields()`](https://prigasg.github.io/twbparser/reference/twb_unused_fields.md)
covers raw fields, calculated fields, *and* parameters;
[`twb_calc_build_order()`](https://prigasg.github.io/twbparser/reference/twb_calc_build_order.md)
warns and marks `build_order = NA` for fields caught in a dependency
cycle; parameters with no usages are found via
[`twb_unused_fields()`](https://prigasg.github.io/twbparser/reference/twb_unused_fields.md)
rather than appearing empty in
[`twb_parameter_usage()`](https://prigasg.github.io/twbparser/reference/twb_parameter_usage.md).

## Dashboard intelligence

### Charts on a dashboard

``` r

charts <- twb_dashboard_charts(parser)
head(charts)
#> # A tibble: 0 × 18
#> # ℹ 18 variables: dashboard <chr>, sheet <chr>, mark_type <chr>,
#> #   mark_source <chr>, rows <list>, cols <list>, dimensions <list>,
#> #   measures <list>, tooltip_fields <list>, n_tooltip_fields <int>,
#> #   has_tooltip <lgl>, n_filters <int>, datasources <list>, zone_id <chr>,
#> #   x <int>, y <int>, w <int>, h <int>
```

One row per worksheet placed on each dashboard: mark type, fields
(`rows`, `cols`, `dimensions`, `measures` as list-columns), tooltip
summary, and layout position. (The bundled demo workbook has no
dashboards, so this is empty here.)

### Sheet positions

``` r

db_sheets <- twb_dashboard_sheets(parser)
head(db_sheets)
#> # A tibble: 0 × 7
#> # ℹ 7 variables: dashboard <chr>, sheet <chr>, zone_id <chr>, x <int>, y <int>,
#> #   w <int>, h <int>
```

`x`, `y`, `w`, `h` are pixel coordinates within the dashboard canvas.

### Zone layout tree

``` r

layout <- twb_dashboard_layout(parser)
head(layout)
#> # A tibble: 0 × 10
#> # ℹ 10 variables: dashboard <chr>, zone_id <chr>, parent_zone_id <chr>,
#> #   component_type <chr>, target <chr>, layout_type <chr>, x <int>, y <int>,
#> #   w <int>, h <int>
```

`parent_zone_id` links child zones to their container; root zones have
`NA`. `component_type` is one of `"worksheet"`, `"filter"`,
`"container"`, `"legend"`, `"parameter_control"`, `"text"`, `"image"`,
or `"blank"`.

### Actions

``` r

actions <- twb_dashboard_actions(parser)
head(actions)
#> # A tibble: 0 × 6
#> # ℹ 6 variables: action_name <chr>, action_type <chr>, source_sheets <chr>,
#> #   target_sheet <chr>, run_on <chr>, url <chr>
```

`action_type` is `"filter"`, `"url"`, `"highlight"`, or `"parameter"`.
`source_sheets` is a comma-separated list; `url` is populated for URL
actions.

## Relationships and Joins

``` r

relations <- parser$get_relationships()

head(relations)
#> # A tibble: 1 × 8
#>   relationship_type left_table right_table       left_field operator right_field
#>   <chr>             <chr>      <chr>             <chr>      <chr>    <chr>      
#> 1 Relationship      Sheet1     Municipal_Bounda… County     =        COUNTY     
#> # ℹ 2 more variables: left_is_calc <lgl>, right_is_calc <lgl>
```

## Working with TWBX Files (if applicable)

``` r

parser$get_twbx_manifest()
parser$get_twbx_extracts()
parser$get_twbx_images()
```

## Validation of Relationships

``` r

v <- parser$validate()
if (isTRUE(v$ok)) {
cat("Relationships validated successfully.\n")
} else {
print(v$issues)
}
#> Relationships validated successfully.
```

## Batch export

For non-interactive use,
[`parse_twb()`](https://prigasg.github.io/twbparser/reference/parse_twb.md)
parses the workbook and writes a structured report to disk — a
human-readable `report.txt`, one CSV per key table, a plain-text
replication brief, and the field dependency graph as GraphML:

``` r

out <- parse_twb(parser$path,
                 output_dir = file.path(tempdir(), "twbparser-vignette"),
                 overwrite = TRUE, quiet = TRUE)
list.files(out)
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

## Summary

This vignette overviewed how to use the `twbparser` package for detailed
inspection and extraction of Tableau workbook internals to assist in
analysis, replication, or integration workflows.
