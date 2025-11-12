# Plot a field-level relationship DAG (legacy)

Uses `relationships_df` with columns `left_table`, `right_table`,
`left_field`, `right_field`, and optional `operator`.

## Usage

``` r
plot_relationship_graph(relationships_df, seed = NULL)
```

## Arguments

- relationships_df:

  Data frame of field-level relationships.

- seed:

  Optional integer seed to make the layout reproducible. If `NULL`
  (default), the function preserves the caller's RNG state.

## Value

Invisibly returns the plotted graph.
