# Translate simple Tableau calculated fields

Performs deterministic, best-effort formula rewrites for common Tableau
functions. Complex Tableau features such as LOD expressions and table
calculations are flagged for manual review.

## Usage

``` r
translate_tableau_calc(formula, target = c("dax", "sql", "r"))
```

## Arguments

- formula:

  Character vector of Tableau formulas.

- target:

  Target language: `"dax"`, `"sql"`, or `"r"`.

## Value

A tibble with source formula, translated formula, confidence, and review
notes.

## Examples

``` r
translate_tableau_calc("IF [Sales] > 0 THEN [Profit] ELSE 0 END", target = "sql")
#> # A tibble: 1 × 5
#>   target tableau_formula                     translated_formula confidence notes
#>   <chr>  <chr>                               <chr>              <chr>      <chr>
#> 1 sql    IF [Sales] > 0 THEN [Profit] ELSE … "CASE WHEN \"Sale… medium     Best…
```
