# Plot a source join graph

Visualizes joins between sources. Expects `joins_df` with columns
`left_table`, `right_table`, `left_field`, `right_field`. If
`relationships_df` is provided (modern relationships), it will render a
second graph highlighting those relationships.

## Usage

``` r
plot_source_join_graph(joins_df, relationships_df = NULL, seed = NULL)
```

## Arguments

- joins_df:

  Data frame with join edges.

- relationships_df:

  Optional data frame with modern relationships.

- seed:

  Optional integer seed to make layouts reproducible. If `NULL`
  (default), the function preserves the caller's RNG state.

## Value

Invisibly returns the join graph, or a list
`list(joins = g, relationships = gr)` if `relationships_df` is provided.
