# Render a migration brief

Creates a Markdown migration brief from parsed workbook metadata and
optional target-specific compatibility notes.

## Usage

``` r
render_migration_brief(
  x,
  target = c("powerbi", "shiny", "quarto", "looker", "superset"),
  output = NULL
)
```

## Arguments

- x:

  A `TwbParser` object or an `xml2` document.

- target:

  Target tool.

- output:

  Optional file path. When supplied, the brief is written to disk.

## Value

A character scalar containing Markdown.
