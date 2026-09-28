# Scaffold a Shiny dashboard rebuild

Writes a minimal `app.R` with one tab per Tableau dashboard and
placeholders for worksheets detected in the workbook.

## Usage

``` r
scaffold_shiny_dashboard(x, path = "shiny-scaffold")
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- path:

  Output directory.

## Value

Path to the written `app.R`.
