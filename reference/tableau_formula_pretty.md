# Prettify a Tableau calculation formula for display

Prettify a Tableau calculation formula for display

## Usage

``` r
tableau_formula_pretty(formula, strip_brackets = FALSE, wrap = NA_integer_)
```

## Arguments

- formula:

  character scalar

- strip_brackets:

  logical; remove \[ \] around field names (default FALSE)

  \[ \]: R:%20

- wrap:

  optional integer to hard-wrap lines (use NA to disable)

## Value

character scalar (multi-line, indented)
