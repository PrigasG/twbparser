# Export a Tableau migration bundle

Writes a folder of migration artifacts such as inventory CSVs, lineage,
compatibility results, formula translation candidates, and a Markdown
brief.

## Usage

``` r
export_migration_bundle(
  x,
  target = c("powerbi", "shiny", "quarto", "looker", "superset"),
  path = "twbparser-migration-bundle",
  include_scaffold = TRUE
)
```

## Arguments

- x:

  A `TwbParser` object or path to a `.twb` / `.twbx` file.

- target:

  Target tool. One of `"powerbi"`, `"shiny"`, `"quarto"`, `"looker"`, or
  `"superset"`.

- path:

  Output directory.

- include_scaffold:

  Logical; include a Shiny or Quarto scaffold when `target` is `"shiny"`
  or `"quarto"`. Default `TRUE`.

## Value

Invisibly returns a tibble of written files.

## Examples

``` r
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
if (nzchar(twb) && file.exists(twb)) {
  parser <- TwbParser$new(twb)
  out <- file.path(tempdir(), "twbparser-bundle")
  export_migration_bundle(parser, target = "shiny", path = out)
}
#> TWB loaded: test_for_wenjie.twb
#> TWB parsed and ready
```
