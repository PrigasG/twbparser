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

## Dashboard intelligence

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

## Summary

This vignette overviewed how to use the `twbparser` package for detailed
inspection and extraction of Tableau workbook internals to assist in
analysis, replication, or integration workflows.
