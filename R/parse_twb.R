#' Every file parse_twb() can write into `output_dir`.
#' @keywords internal
#' @noRd
.parse_twb_outputs <- c(
  "overview.csv", "datasources.csv", "parameters.csv", "fields.csv",
  "calculated_fields.csv", "relationships.csv", "joins.csv",
  "custom_sql.csv", "pages.csv", "dashboards.csv",
  "unused_fields.csv", "calc_build_order.csv", "parameter_usage.csv",
  "report.txt", "sheet_specs.txt", "replication_brief.txt",
  "dependency_graph.graphml"
)

#' Parse a Tableau workbook and write a batch export to disk
#'
#' `parse_twb()` is a convenience wrapper for non-interactive use: it parses a
#' `.twb`/`.twbx` workbook with [TwbParser] and writes a structured set of
#' outputs into `output_dir` — a human-readable report, one CSV per key table
#' (including the rebuild kit: unused fields, calculation build order, and
#' parameter usage), per-worksheet visualization specs, a plain-text
#' replication brief, and the field dependency graph as GraphML (readable with
#' `igraph`/`ggraph` or any GraphML tool).
#'
#' @param path Path to a `.twb` or `.twbx` file.
#' @param output_dir Directory to write outputs into. Created if needed
#'   (including parents).
#' @param overwrite If `FALSE` (default), refuse to write into an existing
#'   non-empty directory instead of mixing outputs. If `TRUE`, the previous
#'   outputs written by parse_twb in the directory are removed first so the
#'   export reflects the current workbook; unrelated files are left alone.
#' @param quiet If `TRUE`, suppress progress messages.
#'
#' @return The normalized `output_dir`, invisibly.
#'
#' @examples
#' twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
#' if (nzchar(twb) && file.exists(twb)) {
#'   out <- parse_twb(twb, output_dir = file.path(tempdir(), "twbparser-demo"),
#'                    quiet = TRUE)
#'   list.files(out)
#' }
#'
#' @export
parse_twb <- function(path, output_dir = "results",
                      overwrite = FALSE, quiet = FALSE) {
  msg <- function(...) if (!quiet) message(...)

  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be a single non-empty file path.", call. = FALSE)
  }
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) {
    stop("`output_dir` must be a single non-empty directory path.", call. = FALSE)
  }

  # Fail fast on the destination before paying for the parse.
  if (dir.exists(output_dir) &&
      length(list.files(output_dir, all.files = TRUE, no.. = TRUE)) > 0L &&
      !isTRUE(overwrite)) {
    stop(
      "output_dir '", output_dir, "' already exists and is not empty; ",
      "use overwrite = TRUE to replace it.",
      call. = FALSE
    )
  }

  msg("Parsing workbook: ", path)
  parser <- if (quiet) {
    suppressMessages(TwbParser$new(path))
  } else {
    TwbParser$new(path)
  }

  # With overwrite = TRUE, clear previous outputs written by parse_twb so the
  # directory reflects this export instead of mixing old and new files.
  # Unrelated user files are left alone.
  if (isTRUE(overwrite) && dir.exists(output_dir)) {
    stale <- file.path(output_dir, .parse_twb_outputs)
    stale <- stale[file.exists(stale)]
    if (length(stale) > 0L) {
      msg("Removing ", length(stale), " previous output file(s).")
      unlink(stale)
    }
  }

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  msg("Writing batch export to: ", output_dir)

  # CSV tables ---------------------------------------------------------------
  write_table <- function(df, name) {
    if (is.null(df)) return(invisible(FALSE))
    df <- as.data.frame(df)
    # write.csv() cannot handle list columns; drop them loudly rather than
    # failing the whole batch on one exotic column.
    is_list_col <- vapply(df, is.list, logical(1))
    if (any(is_list_col)) {
      msg(
        "Dropping list column(s) from '", name, "': ",
        paste(names(df)[is_list_col], collapse = ", ")
      )
      df <- df[, !is_list_col, drop = FALSE]
    }
    utils::write.csv(df, file.path(output_dir, name), row.names = FALSE)
    invisible(TRUE)
  }

  write_table(parser$get_overview(), "overview.csv")
  write_table(parser$get_datasources(), "datasources.csv")
  write_table(parser$get_parameters(), "parameters.csv")
  write_table(parser$get_fields(), "fields.csv")
  write_table(parser$get_calculated_fields(), "calculated_fields.csv")
  write_table(parser$get_relationships(), "relationships.csv")
  write_table(parser$get_joins(), "joins.csv")
  write_table(parser$get_custom_sql(), "custom_sql.csv")
  write_table(parser$get_pages(), "pages.csv")
  write_table(parser$get_dashboards(), "dashboards.csv")
  write_table(parser$get_unused_fields(), "unused_fields.csv")
  write_table(parser$get_calc_build_order(), "calc_build_order.csv")
  write_table(parser$get_parameter_usage(), "parameter_usage.csv")

  # Human-readable report ----------------------------------------------------
  msg("Writing report.txt")
  report <- parser$report
  utils::capture.output(print(report), file = file.path(output_dir, "report.txt"))

  # Per-worksheet visualization specs ----------------------------------------
  msg("Writing sheet_specs.txt")
  viz <- parser$get_sheet_spec()
  utils::capture.output(print(viz), file = file.path(output_dir, "sheet_specs.txt"))

  # Replication brief --------------------------------------------------------
  msg("Writing replication_brief.txt")
  brief <- parser$get_replication_brief(format = "text")
  writeLines(as.character(brief), file.path(output_dir, "replication_brief.txt"))

  # Dependency graph ---------------------------------------------------------
  calcs <- parser$get_calculated_fields()
  if (all(c("name", "formula") %in% names(calcs)) && nrow(calcs) > 0L) {
    g <- build_dependency_graph(calcs[, c("name", "formula")])
    if (igraph::gorder(g) > 0L) {
      msg("Writing dependency_graph.graphml")
      igraph::write_graph(
        g,
        file.path(output_dir, "dependency_graph.graphml"),
        format = "graphml"
      )
    } else {
      msg("No field dependencies found; skipping dependency graph.")
    }
  }

  msg("Done.")
  invisible(normalizePath(output_dir, mustWork = FALSE))
}
