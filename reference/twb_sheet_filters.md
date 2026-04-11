# Extract detailed filter configuration for worksheets

Returns one row per filter per worksheet, with full details on filter
class, inclusion mode, categorical members, and numeric or date range
bounds.

## Usage

``` r
twb_sheet_filters(x, sheet = NULL)
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

  Raw column-reference attribute value.

- field_clean:

  Human-readable field name.

- datasource:

  Datasource name.

- filter_class:

  Tableau filter class: `"categorical"`, `"range"`, `"relative-date"`,
  `"date"`, `"set"`, `"top"`, etc.

- include_mode:

  `"include"` or `"exclude"`.

- members:

  Comma-separated member values for categorical filters; `NA` otherwise.

- range_min:

  Lower bound for range/quantitative filters; `NA` otherwise.

- range_max:

  Upper bound; `NA` otherwise.

## Examples

``` r
xml <- xml2::read_xml(
  '<workbook>
    <worksheets>
      <worksheet name="Sheet1">
        <table>
          <view>
            <filter class="categorical" column="[ds].[Category]">
              <groupfilter function="union">
                <groupfilter function="member" member="[Furniture]"/>
                <groupfilter function="member" member="[Technology]"/>
              </groupfilter>
            </filter>
          </view>
        </table>
      </worksheet>
    </worksheets>
  </workbook>'
)
twb_sheet_filters(xml)
#> # A tibble: 1 × 9
#>   sheet  field_ref      field_clean datasource filter_class include_mode members
#>   <chr>  <chr>          <chr>       <chr>      <chr>        <chr>        <chr>  
#> 1 Sheet1 [ds].[Categor… Category    ds         categorical  include      Furnit…
#> # ℹ 2 more variables: range_min <chr>, range_max <chr>
```
