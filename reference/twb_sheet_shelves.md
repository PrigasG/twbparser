# Extract field-to-shelf assignments for worksheets

Returns a tidy tibble describing which fields are placed on each visual
shelf (rows, cols, color, size, label, detail, tooltip, etc.) for every
worksheet in the workbook (or a single named sheet).

## Usage

``` r
twb_sheet_shelves(x, sheet = NULL)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- sheet:

  Optional character scalar. When supplied only that worksheet is
  returned; otherwise all worksheets are returned.

## Value

A tibble with columns:

- sheet:

  Worksheet name.

- shelf:

  Shelf name: `"rows"`, `"cols"`, or an encoding type such as `"color"`,
  `"size"`, `"label"`, `"detail"`, `"tooltip"`, `"shape"`, `"text"`,
  `"path"`, `"angle"`, `"lod"`, `"geometry"`, etc.

- field_ref:

  Raw column-reference attribute value.

- field_instance:

  Field instance name (after stripping datasource prefix).

- field_clean:

  Human-readable field name.

- datasource:

  Datasource name referenced.

- aggregation:

  Aggregation function (`"SUM"`, `"AVG"`, …) or `NA`.

## Examples

``` r
xml <- xml2::read_xml(
  '<workbook>
    <worksheets>
      <worksheet name="Sales">
        <table>
          <rows>[ds].[Category]</rows>
          <cols>[ds].[Sales]</cols>
          <panes>
            <pane>
              <mark class="Bar"/>
              <encodings>
                <color column="[ds].[Category]"/>
              </encodings>
            </pane>
          </panes>
        </table>
      </worksheet>
    </worksheets>
  </workbook>'
)
twb_sheet_shelves(xml)
#> # A tibble: 3 × 7
#>   sheet shelf field_ref       field_instance field_clean datasource aggregation
#>   <chr> <chr> <chr>           <chr>          <chr>       <chr>      <chr>      
#> 1 Sales color [ds].[Category] Category       Category    ds         NA         
#> 2 Sales cols  [ds].[Sales]    Sales          Sales       ds         NA         
#> 3 Sales rows  [ds].[Category] Category       Category    ds         NA         
```
