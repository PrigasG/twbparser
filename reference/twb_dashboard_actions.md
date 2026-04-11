# Extract dashboard and workbook actions

Parses `<action>` nodes from dashboard `<actions>` sections and the
top-level workbook `<actions>` section. Returns one row per action.

## Usage

``` r
twb_dashboard_actions(x, dashboard = NULL)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- dashboard:

  Optional character scalar. When supplied, only actions whose
  `<source-sheets>` include a sheet from that dashboard are returned.
  Pass `NULL` (default) to return all actions.

## Value

A tibble with columns:

- action_name:

  Caption / display name of the action.

- action_type:

  `"filter"`, `"highlight"`, `"url"`, `"set"`, or another type string
  from the XML.

- source_sheets:

  Comma-separated list of source worksheet names.

- target_sheet:

  Target worksheet name, or `NA` for URL actions.

- run_on:

  Trigger: `"select"`, `"menu"`, or `"hover"`.

- url:

  URL value for URL-type actions; `NA` otherwise.

## Examples

``` r
xml <- xml2::read_xml(
  '<workbook>
    <actions>
      <action caption="Filter 1" name="act1" type="filter">
        <source-sheets>
          <source-sheet name="Sheet1"/>
        </source-sheets>
        <target-sheets>
          <target-sheet name="Sheet2"/>
        </target-sheets>
        <run-on type="select"/>
      </action>
    </actions>
  </workbook>'
)
twb_dashboard_actions(xml)
#> # A tibble: 1 × 6
#>   action_name action_type source_sheets target_sheet run_on url  
#>   <chr>       <chr>       <chr>         <chr>        <chr>  <chr>
#> 1 Filter 1    filter      Sheet1        Sheet2       select NA   
```
