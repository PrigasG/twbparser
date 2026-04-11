# Extract sort configuration for worksheets

Returns one row per sort directive per worksheet.

## Usage

``` r
twb_sheet_sorts(x, sheet = NULL)
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

- field_ref:

  Raw column-reference attribute.

- field_clean:

  Human-readable field name.

- datasource:

  Datasource name.

- sort_order:

  `"ascending"` or `"descending"`.

- sort_by:

  Sort method: `"field"`, `"alphabetic"`, `"manual"`,
  `"data-source-order"`, etc.

## Examples

``` r
xml <- xml2::read_xml(
  '<workbook>
    <worksheets>
      <worksheet name="Sheet1">
        <table>
          <view>
            <sort class="sum" column="[ds].[Sales]" direction="descending"/>
          </view>
        </table>
      </worksheet>
    </worksheets>
  </workbook>'
)
twb_sheet_sorts(xml)
#> # A tibble: 1 × 6
#>   sheet  field_ref    field_clean datasource sort_order sort_by
#>   <chr>  <chr>        <chr>       <chr>      <chr>      <chr>  
#> 1 Sheet1 [ds].[Sales] Sales       ds         descending field  
```
