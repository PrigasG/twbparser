# Extract axis configuration for worksheets

Reads per-axis style rules embedded in worksheet XML and returns one row
per axis per worksheet.

## Usage

``` r
twb_sheet_axes(x, sheet = NULL)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- sheet:

  Optional character scalar to restrict output to one worksheet.

## Value

A tibble with columns:

- sheet:

  Worksheet name.

- axis:

  Axis identifier (e.g., `"rows"`, `"cols"`, or `"automatic"`).

- field_ref:

  Column reference if axis is field-specific; `NA` otherwise.

- field_clean:

  Human-readable field name; `NA` if not field-specific.

- scale_type:

  Scale type (`"linear"`, `"log"`, …) if present; `NA` otherwise.

- reversed:

  Logical: `TRUE` if axis is reversed.

- include_zero:

  Logical: `TRUE` if zero is pinned on axis.

## Examples

``` r
xml <- xml2::read_xml(
  '<workbook>
    <worksheets>
      <worksheet name="Sheet1">
        <table>
          <style>
            <style-rule element="axis">
              <format attr="reverse" value="false"/>
              <format attr="scale-include-zero" value="true"/>
            </style-rule>
          </style>
        </table>
      </worksheet>
    </worksheets>
  </workbook>'
)
twb_sheet_axes(xml)
#> # A tibble: 1 × 7
#>   sheet  axis      field_ref field_clean scale_type reversed include_zero
#>   <chr>  <chr>     <chr>     <chr>       <chr>      <lgl>    <lgl>       
#> 1 Sheet1 automatic NA        NA          NA         FALSE    TRUE        
```
