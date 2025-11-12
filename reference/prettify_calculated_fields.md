# Add a prettified formula column to calculated fields table

Add a prettified formula column to calculated fields table

## Usage

``` r
prettify_calculated_fields(calcs, strip_brackets = FALSE, wrap = 100L)
```

## Arguments

- calcs:

  tibble from extract_calculated_fields()

- strip_brackets:

  logical

- wrap:

  integer wrap width; default 100

## Value

tibble with extra column `formula_pretty`
