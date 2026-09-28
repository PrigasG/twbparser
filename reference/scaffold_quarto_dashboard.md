# Scaffold a Quarto dashboard rebuild

Writes a minimal Quarto dashboard with one section per Tableau dashboard
and worksheet placeholders.

## Usage

``` r
scaffold_quarto_dashboard(x, path = "quarto-scaffold")
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- path:

  Output directory.

## Value

Path to the written `.qmd` file.
