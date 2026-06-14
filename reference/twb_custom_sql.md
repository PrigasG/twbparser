# Extract Custom SQL relations from a Tableau workbook

Finds every `<relation formula="...">` node that looks like a SQL
statement and returns its name, type, raw SQL text, and a flag for
whether it starts with `SELECT` or `WITH`.

## Usage

``` r
twb_custom_sql(x)
```

## Arguments

- x:

  A `TwbParser` object **or** an `xml2` document.

## Value

A tibble with columns:

- relation_name:

  Name attribute of the relation node.

- relation_type:

  Type attribute (e.g. `"text"`, `"table"`).

- custom_sql:

  Full SQL text.

- is_custom_sql:

  `TRUE` when the text begins with `SELECT` or `WITH`.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
stopifnot(nzchar(twb), file.exists(twb))
xml <- xml2::read_xml(twb)
twb_custom_sql(xml)
#> # A tibble: 0 × 4
#> # ℹ 4 variables: relation_name <chr>, relation_type <chr>, custom_sql <chr>,
#> #   is_custom_sql <lgl>
```
