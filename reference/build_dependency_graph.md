# Build a field dependency graph from calculated fields

Creates a directed graph where edges point from input fields used in a
formula to the calculated output field. Tokens are extracted from
bracketed references like `[Table].[Field]` or `[Field]`.

## Usage

``` r
build_dependency_graph(fields_df)
```

## Arguments

- fields_df:

  A data frame with at least columns `name` and `formula`.

## Value

An `igraph` directed graph where vertices are field names and edges
represent dependencies (input → output).

## Examples

``` r
fields <- tibble::tibble(
name = c("X_plus_Y", "Z"),
formula = c("[X] + [Y]", "[X_plus_Y] * 2")
)
g <- build_dependency_graph(fields)

```
