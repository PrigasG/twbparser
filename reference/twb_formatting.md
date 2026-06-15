# Formatting rules (fonts, colours, number formats, …)

Walks every `<format>` entry inside a `<style-rule>` and returns a tidy
table of formatting attributes. This is where fonts, colours,
number/date formats, alignment, and shading live in a Tableau workbook.
Filter the `attr` column for what you need (e.g.
`attr == "number-format"`, or `startsWith(attr, "font")`).

## Usage

``` r
twb_formatting(x, scope = NULL)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- scope:

  Optional character scalar (`"worksheet"`, `"dashboard"`, or
  `"workbook"`) to restrict output.

## Value

A tibble with columns:

- scope:

  `"worksheet"`, `"dashboard"`, or `"workbook"`.

- container:

  Worksheet or dashboard name (`NA` for workbook scope).

- element:

  The styled element, e.g. `"mark"`, `"axis"`, `"pane"`, `"cell"`,
  `"title"`.

- field:

  Field the rule targets, if any; `NA` otherwise.

- target:

  Palette bucket or other target value, if any; `NA` otherwise.

- attr:

  Formatting attribute, e.g. `"font-family"`, `"font-size"`, `"color"`,
  `"number-format"`, `"encoding-color"`.

- value:

  Attribute value.

## Examples

``` r
xml <- xml2::read_xml(
  '<workbook><worksheets><worksheet name="Sales"><table><style>
    <style-rule element="mark">
      <format attr="mark-labels-cull" value="true"/>
      <format attr="number-format" value="$#,##0"/>
    </style-rule>
  </style></table></worksheet></worksheets></workbook>'
)
twb_formatting(xml)
#> # A tibble: 2 × 7
#>   scope     container element field target attr             value 
#>   <chr>     <chr>     <chr>   <chr> <chr>  <chr>            <chr> 
#> 1 worksheet Sales     mark    NA    NA     mark-labels-cull true  
#> 2 worksheet Sales     mark    NA    NA     number-format    $#,##0
```
