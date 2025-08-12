#' @keywords internal
.clean_name <- function(x) {
  x <- gsub("\\[|\\]", "", x) # strip brackets
  x <- sub("_[0-9A-Fa-f]{32}$", "", x) # drop 32-char Tableau suffix
  x
}

#' @keywords internal
.normalize_token <- function(tok) {
  if (is.na(tok) || !nzchar(tok)) {
    return(NA_character_)
  }
  tok <- gsub("^\\[|\\]$", "", tok) # remove outer [ ]
  tok <- gsub("\\[|\\]", "", tok) # just in case double-wrapped
  tok <- sub("^([^:]+:)+", "", tok) # remove prefixes like "none:" or "clct:"

  # Split table-qualified references: [Table].[Field] -> take Field
  parts <- strsplit(tok, "\\.?", fixed = FALSE)[[1]]
  parts <- parts[nzchar(parts)]
  parts <- gsub("^\\s+|\\s+$", "", parts)
  if (length(parts) == 0) {
    return(NA_character_)
  }
  tail(parts, 1)
}

#' @keywords internal
.extract_tokens <- function(formula) {
  if (is.null(formula) || length(formula) == 0 || is.na(formula)) {
    return(character())
  }
  raw <- stringr::str_extract_all(formula, "\\[[^\\]]+\\]")[[1]]
  if (length(raw) == 0) {
    return(character())
  }
  toks <- vapply(raw, .normalize_token, FUN.VALUE = character(1))
  toks[!is.na(toks) & nzchar(toks)]
}

#' Build a field dependency graph from calculated fields
#'
#' Creates a directed graph where edges point from input fields used in a
#' formula to the calculated output field. Tokens are extracted from bracketed
#' references like `[Table].[Field]` or `[Field]`.
#'
#' @param fields_df A data frame with at least columns `name` and `formula`.
#'
#' @return An `igraph` directed graph where vertices are field names and edges
#'   represent dependencies (input ➜ output).
#'
#' @examples
#' \dontrun{
#' fields <- tibble::tibble(
#'   name = c("X_plus_Y", "Z"),
#'   formula = c("[X] + [Y]", "[X_plus_Y] * 2")
#' )
#' g <- build_dependency_graph(fields)
#' }
#'
#' @export
#' @importFrom dplyr mutate transmute filter distinct
#' @importFrom tidyr unnest_longer replace_na
#' @importFrom purrr map
#' @importFrom igraph graph_from_data_frame make_empty_graph
build_dependency_graph <- function(fields_df) {
  if (is.null(fields_df) || nrow(fields_df) == 0) {
    return(igraph::make_empty_graph(directed = TRUE))
  }
  if (!all(c("name", "formula") %in% names(fields_df))) {
    stop("fields_df must contain columns: 'name' and 'formula'")
  }

  edge_list <- fields_df |>
    dplyr::mutate(
      inputs = purrr::map(formula, .extract_tokens),
      output = name
    ) |>
    tidyr::unnest_longer(inputs, values_to = "from", indices_include = FALSE) |>
    dplyr::transmute(from = from, to = output) |>
    dplyr::filter(!is.na(from), !is.na(to), from != to) |>
    dplyr::distinct()

  if (nrow(edge_list) == 0) {
    return(igraph::make_empty_graph(directed = TRUE))
  }

  igraph::graph_from_data_frame(edge_list, directed = TRUE)
}

#' Plot a field dependency graph
#'
#' Draws a quick base-graphics plot of a dependency graph. Vertices that are
#' calculated fields (present in `fields_df$name`) are drawn differently.
#'
#' @param g An `igraph` directed graph from [build_dependency_graph()].
#' @param fields_df Optional data frame with a `name` column to mark calculated
#'   outputs.
#'
#' @return Invisibly returns `g`.
#'
#' @examples
#' \dontrun{
#' g <- build_dependency_graph(fields)
#' plot_dependency_graph(g, fields)
#' }
#'
#' @export
#' @importFrom igraph V gorder layout_with_fr
#' @importFrom graphics plot
plot_dependency_graph <- function(g, fields_df) {
  if (is.null(g) || igraph::gorder(g) == 0) {
    message("Nothing to plot: empty graph.")
    return(invisible(NULL))
  }

  calculated <- if (!is.null(fields_df) && "name" %in% names(fields_df)) {
    unique(fields_df$name)
  } else {
    character()
  }

  igraph::V(g)$label <- igraph::V(g)$name
  igraph::V(g)$color <- ifelse(igraph::V(g)$name %in% calculated, "orange", "lightblue")
  igraph::V(g)$shape <- ifelse(igraph::V(g)$name %in% calculated, "square", "circle")

  set.seed(42)
  lay <- igraph::layout_with_fr(g)

  graphics::plot(
    g,
    layout = lay,
    vertex.label.cex = 0.9,
    vertex.label.color = "black",
    vertex.size = 22,
    edge.arrow.size = 0.4,
    main = "Tableau Field Dependency DAG"
  )
  invisible(g)
}

#' Plot a source join graph
#'
#' Visualizes joins between sources. Expects `joins_df` with columns
#' `left_table`, `right_table`, `left_field`, `right_field`. If
#' `relationships_df` is provided (modern relationships), it will render a
#' second graph highlighting those relationships.
#'
#' @param joins_df Data frame with join edges.
#' @param relationships_df Optional data frame with modern relationships.
#'
#' @return Invisibly returns the join graph, or a list `list(joins = g, relationships = gr)`
#'   if `relationships_df` is provided.
#'
#' @export
#' @importFrom dplyr transmute distinct
#' @importFrom igraph graph_from_data_frame layout_with_fr
#' @importFrom graphics plot
plot_source_join_graph <- function(joins_df, relationships_df = NULL) {
  if (is.null(joins_df) || nrow(joins_df) == 0) {
    message("No join relationships found.")
    return(invisible(NULL))
  }

  edges <- joins_df |>
    dplyr::transmute(
      from = left_table,
      to = right_table,
      label = paste0(left_field, " = ", right_field)
    )

  g <- igraph::graph_from_data_frame(edges, directed = TRUE)

  set.seed(7)
  lay <- igraph::layout_with_fr(g)

  graphics::plot(
    g,
    layout = lay,
    vertex.color = "lightblue",
    vertex.size = 26,
    edge.label = edges$label,
    vertex.label.cex = 0.95,
    edge.arrow.size = 0.4,
    main = "Data Source Join Structure"
  )

  if (!is.null(relationships_df) && nrow(relationships_df) > 0) {
    has_operator <- "operator" %in% names(relationships_df)

    rel_edges <- if (has_operator) {
      relationships_df |>
        dplyr::transmute(from = left_source, to = right_source, label = operator) |>
        dplyr::distinct()
    } else {
      relationships_df |>
        dplyr::transmute(
          from = left_source,
          to = right_source,
          label = paste0(left_field, " = ", right_field)
        ) |>
        dplyr::distinct()
    }

    gr <- igraph::graph_from_data_frame(rel_edges, directed = TRUE)

    set.seed(8)
    lay2 <- igraph::layout_with_fr(gr)

    graphics::plot(
      gr,
      layout = lay2,
      vertex.color = "lightgreen",
      vertex.size = 26,
      edge.label = rel_edges$label,
      vertex.label.cex = 0.95,
      edge.arrow.size = 0.4,
      main = "Data Source Relationships (2020.2+)"
    )
    return(invisible(list(joins = g, relationships = gr)))
  }

  invisible(g)
}

#' Plot a field-level relationship DAG (legacy)
#'
#' Uses `relationships_df` with columns `left_table`, `right_table`,
#' `left_field`, `right_field`, and optional `operator`.
#'
#' @param relationships_df Data frame of field-level relationships.
#'
#' @return Invisibly returns the plotted graph.
#'
#' @export
#' @importFrom dplyr mutate select distinct
#' @importFrom igraph graph_from_data_frame layout_with_fr E
#' @importFrom graphics plot
plot_relationship_graph <- function(relationships_df) {
  if (is.null(relationships_df) || nrow(relationships_df) == 0) {
    message("No relationships to plot.")
    return(invisible(NULL))
  }

  edges <- relationships_df |>
    dplyr::mutate(
      from = paste0(.clean_name(left_table), ".", right_field),
      to = paste0(.clean_name(right_table), ".", right_field),
      label = if ("operator" %in% names(relationships_df)) operator else "="
    ) |>
    dplyr::select(from, to, label) |>
    dplyr::distinct()

  g <- igraph::graph_from_data_frame(edges, directed = TRUE)

  set.seed(9)
  lay <- igraph::layout_with_fr(g)

  graphics::plot(
    g,
    layout = lay,
    vertex.label.cex = 0.75,
    edge.label = igraph::E(g)$label,
    edge.label.cex = 0.75,
    edge.arrow.size = 0.3,
    vertex.size = 18,
    main = "Relationship DAG"
  )

  invisible(g)
}

#' @keywords internal
print_field_summary <- function(fields_df) {
  n <- if (!is.null(fields_df)) nrow(fields_df) else 0L
  cat("Found", n, "calculated fields\n")
  if (n > 0) {
    cols <- intersect(c("name", "role", "datatype"), names(fields_df))
    print(utils::head(fields_df[, cols, drop = FALSE]))
  }
  invisible(n)
}
