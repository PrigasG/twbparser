# Plot a field dependency graph

Draws a quick base-graphics plot of a dependency graph. Vertices that
are calculated fields (present in `fields_df$name`) are drawn
differently.

## Usage

``` r
plot_dependency_graph(g, fields_df = NULL, seed = NULL)
```

## Arguments

- g:

  An `igraph` directed graph from
  [`build_dependency_graph()`](https://PrigasG.github.io/twbparser/reference/build_dependency_graph.md).

- fields_df:

  Optional data frame with a `name` column to mark calculated outputs.

- seed:

  Optional integer seed to make the layout reproducible. If `NULL`
  (default), the function will not alter the caller's RNG state.

## Value

Invisibly returns `g`.

## Examples

``` r
fields <- tibble::tibble(
name = c("X_plus_Y", "Z"),
formula = c("[X] + [Y]", "[X_plus_Y] * 2")
)
g <- build_dependency_graph(fields)
plot_dependency_graph(g, fields)           # nondeterministic layout

plot_dependency_graph(g, fields, seed = 1) # deterministic layout

```
