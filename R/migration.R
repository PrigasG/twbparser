#' Audit a folder of Tableau workbooks
#'
#' Batch parses `.twb` and `.twbx` files and returns migration-oriented
#' inventory tables. The summary table is one row per workbook; detail tables
#' retain workbook identity so they can be filtered or joined downstream.
#'
#' @param path Folder containing Tableau workbooks, or a character vector of
#'   `.twb` / `.twbx` files.
#' @param recursive Logical; search folders recursively. Default `TRUE`.
#' @param pattern Optional regular expression applied to file basenames.
#' @param write_csv Logical; write each returned table as a CSV file.
#'   Default `FALSE`.
#' @param output_dir Directory for CSV output when `write_csv = TRUE`.
#'
#' @return A named list with class `twbparser_audit` containing:
#' \describe{
#'   \item{workbooks}{One row per parsed workbook with counts and migration
#'     complexity flags.}
#'   \item{datasources}{Datasource inventory across parsed workbooks.}
#'   \item{calculated_fields}{Calculated field complexity across workbooks.}
#'   \item{field_usage}{Worksheet field usage across workbooks.}
#'   \item{lineage_edges}{Combined lineage edge table across workbooks.}
#'   \item{issues}{Files that could not be parsed and their error messages.}
#' }
#'
#' @examples
#' demo <- system.file("extdata", package = "twbparser")
#' if (nzchar(demo)) {
#'   audit <- audit_tableau_folder(demo)
#'   audit$workbooks
#' }
#'
#' @export
audit_tableau_folder <- function(path,
                                 recursive = TRUE,
                                 pattern = NULL,
                                 write_csv = FALSE,
                                 output_dir = NULL) {
  stopifnot(
    is.character(path), length(path) >= 1L,
    is.logical(recursive), length(recursive) == 1L,
    is.logical(write_csv), length(write_csv) == 1L
  )

  files <- .audit_resolve_files(path, recursive = recursive, pattern = pattern)
  if (!length(files)) {
    out <- .empty_audit_result()
    if (isTRUE(write_csv)) .write_audit_csv(out, output_dir)
    return(out)
  }

  workbook_rows <- list()
  datasource_rows <- list()
  calc_rows <- list()
  usage_rows <- list()
  edge_rows <- list()
  issue_rows <- list()

  for (file in files) {
    workbook_id <- tools::file_path_sans_ext(basename(file))
    parsed <- tryCatch(
      suppressMessages(TwbParser$new(file)),
      error = function(e) e
    )

    if (inherits(parsed, "error")) {
      issue_rows[[length(issue_rows) + 1L]] <- tibble::tibble(
        workbook = workbook_id,
        file = normalizePath(file, winslash = "/", mustWork = FALSE),
        issue = conditionMessage(parsed)
      )
      next
    }

    workbook_rows[[length(workbook_rows) + 1L]] <- .audit_one_workbook(parsed, file, workbook_id)
    datasource_rows[[length(datasource_rows) + 1L]] <- .tag_workbook(parsed$get_datasources(), workbook_id, file)
    calc_rows[[length(calc_rows) + 1L]] <- .tag_workbook(parsed$get_calc_complexity(), workbook_id, file)
    usage_rows[[length(usage_rows) + 1L]] <- .tag_workbook(parsed$get_field_usage(), workbook_id, file)

    lineage <- twb_lineage(parsed)
    edge_rows[[length(edge_rows) + 1L]] <- .tag_workbook(lineage$edges, workbook_id, file)
  }

  out <- structure(
    list(
      workbooks = dplyr::bind_rows(workbook_rows),
      datasources = dplyr::bind_rows(datasource_rows),
      calculated_fields = dplyr::bind_rows(calc_rows),
      field_usage = dplyr::bind_rows(usage_rows),
      lineage_edges = dplyr::bind_rows(edge_rows),
      issues = dplyr::bind_rows(issue_rows)
    ),
    class = "twbparser_audit"
  )

  if (isTRUE(write_csv)) .write_audit_csv(out, output_dir)
  out
}

#' Build workbook lineage for migration analysis
#'
#' Produces a graph-shaped representation of workbook dependencies from
#' datasources and tables through fields, calculated fields, worksheets, and
#' dashboards.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param format Output format: `"tables"` returns `list(nodes, edges)`;
#'   `"igraph"` returns an igraph object; `"mermaid"` returns a Mermaid flowchart
#'   string.
#' @param include_calc_dependencies Logical; include field-to-calculation and
#'   calculation-to-calculation dependencies parsed from formulas. Default
#'   `TRUE`.
#'
#' @return Depends on `format`. The default is a list with:
#' \describe{
#'   \item{nodes}{Tibble with `id`, `label`, and `type`.}
#'   \item{edges}{Tibble with `from`, `to`, and `relationship`.}
#' }
#'
#' @examples
#' twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
#' if (nzchar(twb) && file.exists(twb)) {
#'   parser <- TwbParser$new(twb)
#'   lineage <- twb_lineage(parser)
#'   lineage$nodes
#'   lineage$edges
#' }
#'
#' @export
twb_lineage <- function(x,
                        format = c("tables", "igraph", "mermaid"),
                        include_calc_dependencies = TRUE) {
  format <- match.arg(format)
  stopifnot(
    is.logical(include_calc_dependencies),
    length(include_calc_dependencies) == 1L
  )

  parser <- if (inherits(x, "TwbParser")) x else NULL
  xml_doc <- .twb_resolve_xml(x)
  workbook <- if (!is.null(parser)) basename(parser$path %||% "") else "<inline>"

  nodes <- list()
  edges <- list()

  add_nodes <- function(tbl) {
    if (!is.null(tbl) && nrow(tbl)) nodes[[length(nodes) + 1L]] <<- tbl
  }
  add_edges <- function(tbl) {
    if (!is.null(tbl) && nrow(tbl)) edges[[length(edges) + 1L]] <<- tbl
  }

  fields <- safe_call(extract_columns_with_table_source(xml_doc), tibble::tibble())
  calcs <- safe_call(extract_calculated_fields(xml_doc), tibble::tibble())
  field_usage <- safe_call(twb_field_usage(xml_doc), .empty_field_usage())
  dashboard_sheets <- safe_call(twb_dashboard_sheets(xml_doc), tibble::tibble())
  custom_sql <- safe_call(twb_custom_sql(xml_doc), tibble::tibble())

  field_lookup <- .lineage_field_lookup(fields, calcs)

  if (nrow(fields)) {
    ds_tbl <- fields |>
      dplyr::filter(!is.na(.data$datasource), nzchar(.data$datasource)) |>
      dplyr::distinct(.data$datasource) |>
      dplyr::transmute(
        id = .lineage_id("datasource", .data$datasource),
        label = .data$datasource,
        type = "datasource"
      )
    add_nodes(ds_tbl)

    table_tbl <- fields |>
      dplyr::filter(!is.na(.data$table_clean), nzchar(.data$table_clean)) |>
      dplyr::distinct(.data$datasource, .data$table_clean) |>
      dplyr::transmute(
        id = .lineage_id("table", .data$datasource, .data$table_clean),
        label = .data$table_clean,
        type = "table"
      )
    add_nodes(table_tbl)

    field_tbl <- field_lookup |>
      dplyr::transmute(
        id = .data$field_id,
        label = field_clean,
        type = "field"
      )
    add_nodes(field_tbl)

    add_edges(fields |>
                dplyr::filter(!is.na(.data$datasource), nzchar(.data$datasource),
                              !is.na(.data$table_clean), nzchar(.data$table_clean)) |>
                dplyr::distinct(.data$datasource, .data$table_clean) |>
                dplyr::transmute(
                  from = .lineage_id("datasource", .data$datasource),
                  to = .lineage_id("table", .data$datasource, .data$table_clean),
                  relationship = "contains"
                ))

    add_edges(fields |>
                dplyr::filter(!is.na(.data$table_clean), nzchar(.data$table_clean),
                              !is.na(.data$field_clean), nzchar(.data$field_clean)) |>
                dplyr::distinct(.data$datasource, .data$table_clean, .data$field_clean) |>
                dplyr::transmute(
                  from = .lineage_id("table", .data$datasource, .data$table_clean),
                  to = .lineage_id("field", .data$datasource, .data$field_clean),
                  relationship = "contains"
                ))
  }

  if (nrow(calcs)) {
    calc_nodes <- calcs |>
      dplyr::filter(!is.na(.data$name), nzchar(.data$name)) |>
      dplyr::distinct(.data$datasource, .data$name) |>
      dplyr::transmute(
        id = .lineage_id("calc", .data$datasource, .data$name),
        label = .data$name,
        type = "calculated_field"
      )
    add_nodes(calc_nodes)

    add_edges(calcs |>
                dplyr::filter(!is.na(.data$table_clean), nzchar(.data$table_clean),
                              !is.na(.data$name), nzchar(.data$name)) |>
                dplyr::distinct(.data$datasource, .data$table_clean, .data$name) |>
                dplyr::transmute(
                  from = .lineage_id("table", .data$datasource, .data$table_clean),
                  to = .lineage_id("calc", .data$datasource, .data$name),
                  relationship = "defines"
                ))

    if (isTRUE(include_calc_dependencies)) {
      add_edges(.calc_dependency_edges(calcs, field_lookup))
    }
  }

  if (nrow(field_usage)) {
    sheet_nodes <- field_usage |>
      dplyr::filter(!is.na(.data$sheet), nzchar(.data$sheet)) |>
      dplyr::distinct(.data$sheet) |>
      dplyr::transmute(
        id = .lineage_id("worksheet", .data$sheet),
        label = .data$sheet,
        type = "worksheet"
      )
    add_nodes(sheet_nodes)

    add_edges(.usage_edges(field_usage, field_lookup, calcs))
  }

  if (nrow(dashboard_sheets)) {
    dash_nodes <- dashboard_sheets |>
      dplyr::filter(!is.na(.data$dashboard), nzchar(.data$dashboard)) |>
      dplyr::distinct(.data$dashboard) |>
      dplyr::transmute(
        id = .lineage_id("dashboard", .data$dashboard),
        label = .data$dashboard,
        type = "dashboard"
      )
    add_nodes(dash_nodes)

    add_edges(dashboard_sheets |>
                dplyr::filter(!is.na(.data$sheet), nzchar(.data$sheet),
                              !is.na(.data$dashboard), nzchar(.data$dashboard)) |>
                dplyr::distinct(.data$sheet, .data$dashboard) |>
                dplyr::transmute(
                  from = .lineage_id("worksheet", .data$sheet),
                  to = .lineage_id("dashboard", .data$dashboard),
                  relationship = "appears_on"
                ))
  }

  if (nrow(custom_sql)) {
    sql_nodes <- custom_sql |>
      dplyr::filter(.data$is_custom_sql) |>
      dplyr::mutate(sql_name = dplyr::coalesce(.data$relation_name, "Custom SQL")) |>
      dplyr::distinct(.data$sql_name) |>
      dplyr::transmute(
        id = .lineage_id("custom_sql", .data$sql_name),
        label = .data$sql_name,
        type = "custom_sql"
      )
    add_nodes(sql_nodes)
  }

  out <- list(
    nodes = dplyr::bind_rows(nodes) |> dplyr::distinct(.data$id, .keep_all = TRUE),
    edges = dplyr::bind_rows(edges) |> dplyr::distinct()
  )

  if (nrow(out$edges)) {
    missing_ids <- setdiff(unique(c(out$edges$from, out$edges$to)), out$nodes$id)
    if (length(missing_ids)) {
      out$nodes <- dplyr::bind_rows(
        out$nodes,
        tibble::tibble(
          id = missing_ids,
          label = vapply(missing_ids, .lineage_label_from_id, character(1L)),
          type = "unresolved_field"
        )
      )
    }
  }

  out$nodes <- dplyr::mutate(out$nodes, workbook = workbook)
  out$edges <- dplyr::mutate(out$edges, workbook = workbook)

  if (identical(format, "igraph")) {
    return(igraph::graph_from_data_frame(out$edges, directed = TRUE, vertices = out$nodes))
  }
  if (identical(format, "mermaid")) {
    return(.lineage_to_mermaid(out))
  }
  out
}

#' Assess Tableau workbook migration readiness
#'
#' Scores workbook complexity and summarizes likely migration risks for a target
#' visualization tool.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param target Target tool. One of `"powerbi"`, `"shiny"`, `"quarto"`,
#'   `"looker"`, or `"superset"`.
#'
#' @return A named list with `summary`, `compatibility`, and `recommendations`.
#'
#' @examples
#' twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
#' if (nzchar(twb) && file.exists(twb)) {
#'   parser <- TwbParser$new(twb)
#'   twb_migration_assessment(parser, target = "shiny")$summary
#' }
#'
#' @export
twb_migration_assessment <- function(x,
                                     target = c("powerbi", "shiny", "quarto", "looker", "superset")) {
  target <- match.arg(target)
  parser <- .as_parser_or_null(x)
  xml_doc <- .twb_resolve_xml(x)

  ov <- if (!is.null(parser)) parser$get_overview() else .overview_from_xml(xml_doc)
  pages <- safe_call(.ins_pages(xml_doc), tibble::tibble())
  calcs <- safe_call(twb_calc_complexity(xml_doc), .empty_calc_complexity())
  compatibility <- twb_compatibility(x, targets = target)

  issue_points <- sum(compatibility$detected & compatibility$impact != "info", na.rm = TRUE)
  high_points <- sum(compatibility$detected & compatibility$impact == "high", na.rm = TRUE)
  score <- as.integer(issue_points + high_points)

  effort <- dplyr::case_when(
    score >= 8L ~ "high",
    score >= 3L ~ "medium",
    TRUE ~ "low"
  )

  summary <- tibble::tibble(
    target = target,
    workbook_file = if (!is.null(parser)) basename(parser$path %||% "") else "<inline>",
    worksheets = as.integer(sum(pages$page_type == "worksheet", na.rm = TRUE)),
    dashboards = as.integer(ov$dashboards[[1]] %||% 0L),
    datasources = as.integer(ov$datasources[[1]] %||% 0L),
    calculated_fields = as.integer(ov$calculated_fields[[1]] %||% 0L),
    lod_calculations = as.integer(sum(calcs$calc_type == "lod", na.rm = TRUE)),
    table_calculations = as.integer(sum(calcs$calc_type == "table_calc", na.rm = TRUE)),
    migration_score = score,
    migration_effort = effort
  )

  list(
    summary = summary,
    compatibility = compatibility,
    recommendations = .assessment_recommendations(summary, compatibility)
  )
}

#' Report feature compatibility for migration targets
#'
#' Detects Tableau workbook features that commonly affect migrations and maps
#' them to target-tool support levels.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param targets Character vector of targets. Supported values are `"powerbi"`,
#'   `"shiny"`, `"quarto"`, `"looker"`, and `"superset"`.
#'
#' @return A tibble with one row per detected/checked feature and target.
#'
#' @examples
#' twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
#' if (nzchar(twb) && file.exists(twb)) {
#'   parser <- TwbParser$new(twb)
#'   twb_compatibility(parser, targets = c("powerbi", "shiny"))
#' }
#'
#' @export
twb_compatibility <- function(x,
                              targets = c("powerbi", "shiny", "quarto", "looker", "superset")) {
  targets <- match.arg(targets, several.ok = TRUE)
  parser <- .as_parser_or_null(x)
  xml_doc <- .twb_resolve_xml(x)

  pages <- safe_call(.ins_pages(xml_doc), tibble::tibble())
  calcs <- safe_call(twb_calc_complexity(xml_doc), .empty_calc_complexity())
  layout <- safe_call(twb_dashboard_layout(xml_doc), .empty_layout())
  actions <- safe_call(twb_dashboard_actions(xml_doc), .empty_actions())
  custom_sql <- safe_call(twb_custom_sql(xml_doc), tibble::tibble())
  params <- safe_call(extract_parameters(xml_doc), tibble::tibble())
  published <- safe_call(twb_published_refs(xml_doc), tibble::tibble())
  manifest <- if (!is.null(parser)) parser$get_twbx_manifest() else tibble::tibble()

  features <- tibble::tibble(
    feature = c(
      "lod_calculations", "table_calculations", "parameters",
      "dashboard_actions", "custom_sql", "published_datasources",
      "stories", "floating_layout", "packaged_extracts"
    ),
    detected = c(
      any(calcs$calc_type == "lod", na.rm = TRUE),
      any(calcs$calc_type == "table_calc", na.rm = TRUE),
      nrow(params) > 0L,
      nrow(actions) > 0L,
      nrow(custom_sql) > 0L && any(custom_sql$is_custom_sql, na.rm = TRUE),
      nrow(published) > 0L,
      any(pages$page_type == "story", na.rm = TRUE),
      nrow(layout) > 0L && any(layout$layout_type == "floating", na.rm = TRUE),
      nrow(manifest) > 0L && any(manifest$type == "extract", na.rm = TRUE)
    ),
    impact = c("high", "high", "medium", "medium", "medium", "medium", "high", "medium", "medium"),
    note = c(
      "LOD expressions usually need semantic remapping in the target tool.",
      "Table calculations depend on Tableau's visual query context.",
      "Parameters may map to slicers, inputs, or generated code.",
      "Dashboard actions often need manual interaction design.",
      "Custom SQL should be reviewed before moving into a semantic model.",
      "Published datasource references need replacement connection strategy.",
      "Stories rarely have a direct equivalent outside Tableau.",
      "Floating layouts may need responsive layout redesign.",
      "Packaged extracts need a refresh and storage replacement plan."
    )
  )

  support <- .compatibility_support()
  rows <- lapply(targets, function(target) {
    target_support <- support[[target]]
    dplyr::mutate(
      features,
      target = target,
      support = unname(target_support[features$feature]),
      .before = 1L
    )
  })
  dplyr::bind_rows(rows)
}

#' Translate simple Tableau calculated fields
#'
#' Performs deterministic, best-effort formula rewrites for common Tableau
#' functions. Complex Tableau features such as LOD expressions and table
#' calculations are flagged for manual review.
#'
#' @param formula Character vector of Tableau formulas.
#' @param target Target language: `"dax"`, `"sql"`, or `"r"`.
#'
#' @return A tibble with source formula, translated formula, confidence, and
#'   review notes.
#'
#' @examples
#' translate_tableau_calc("IF [Sales] > 0 THEN [Profit] ELSE 0 END", target = "sql")
#'
#' @export
translate_tableau_calc <- function(formula,
                                   target = c("dax", "sql", "r")) {
  target <- match.arg(target)
  formula <- as.character(formula)

  dplyr::bind_rows(lapply(formula, function(f) {
    translated <- .translate_formula_one(f, target)
    issues <- .formula_review_flags(f)
    confidence <- dplyr::case_when(
      length(issues) == 0L ~ "medium",
      any(grepl("LOD|table calculation", issues)) ~ "low",
      TRUE ~ "review"
    )

    tibble::tibble(
      target = target,
      tableau_formula = f,
      translated_formula = translated,
      confidence = confidence,
      notes = if (length(issues)) paste(issues, collapse = "; ") else "Best-effort deterministic rewrite."
    )
  }))
}

#' Export a Tableau migration bundle
#'
#' Writes a folder of migration artifacts such as inventory CSVs, lineage,
#' compatibility results, formula translation candidates, and a Markdown brief.
#'
#' @param x A `TwbParser` object or path to a `.twb` / `.twbx` file.
#' @param target Target tool. One of `"powerbi"`, `"shiny"`, `"quarto"`,
#'   `"looker"`, or `"superset"`.
#' @param path Output directory.
#' @param include_scaffold Logical; include a Shiny or Quarto scaffold when
#'   `target` is `"shiny"` or `"quarto"`. Default `TRUE`.
#'
#' @return Invisibly returns a tibble of written files.
#'
#' @examples
#' twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
#' if (nzchar(twb) && file.exists(twb)) {
#'   parser <- TwbParser$new(twb)
#'   out <- file.path(tempdir(), "twbparser-bundle")
#'   export_migration_bundle(parser, target = "shiny", path = out)
#' }
#'
#' @export
export_migration_bundle <- function(x,
                                    target = c("powerbi", "shiny", "quarto", "looker", "superset"),
                                    path = "twbparser-migration-bundle",
                                    include_scaffold = TRUE) {
  target <- match.arg(target)
  stopifnot(is.character(path), length(path) == 1L)

  parser <- if (inherits(x, "TwbParser")) x else TwbParser$new(x)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  assessment <- twb_migration_assessment(parser, target = target)
  compatibility <- assessment$compatibility
  lineage <- twb_lineage(parser)
  calcs <- parser$get_calc_complexity()
  translated <- if (nrow(calcs)) {
    translated <- translate_tableau_calc(calcs$formula, target = .calc_target_for_tool(target))
    dplyr::bind_cols(calcs[, intersect(c("datasource", "name", "calc_type"), names(calcs)), drop = FALSE], translated)
  } else {
    tibble::tibble()
  }

  files <- c(
    .write_csv_file(parser$get_overview(), path, "workbook_overview.csv"),
    .write_csv_file(parser$get_datasources(), path, "datasources.csv"),
    .write_csv_file(calcs, path, "calculated_fields.csv"),
    .write_csv_file(translated, path, "calculation_translation_candidates.csv"),
    .write_csv_file(parser$get_field_usage(), path, "field_usage.csv"),
    .write_csv_file(lineage$nodes, path, "lineage_nodes.csv"),
    .write_csv_file(lineage$edges, path, "lineage_edges.csv"),
    .write_csv_file(compatibility, path, "compatibility.csv"),
    .write_csv_file(assessment$summary, path, "migration_assessment.csv")
  )

  brief <- render_migration_brief(parser, target = target)
  brief_path <- file.path(path, "migration_brief.md")
  writeLines(brief, brief_path, useBytes = TRUE)
  files <- c(files, brief_path)

  if (isTRUE(include_scaffold) && identical(target, "shiny")) {
    files <- c(files, scaffold_shiny_dashboard(parser, path = file.path(path, "shiny-scaffold")))
  }
  if (isTRUE(include_scaffold) && identical(target, "quarto")) {
    files <- c(files, scaffold_quarto_dashboard(parser, path = file.path(path, "quarto-scaffold")))
  }

  invisible(tibble::tibble(file = normalizePath(files, winslash = "/", mustWork = FALSE)))
}

#' Render a migration brief
#'
#' Creates a Markdown migration brief from parsed workbook metadata and optional
#' target-specific compatibility notes.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param target Target tool.
#' @param output Optional file path. When supplied, the brief is written to disk.
#'
#' @return A character scalar containing Markdown.
#'
#' @export
render_migration_brief <- function(x,
                                   target = c("powerbi", "shiny", "quarto", "looker", "superset"),
                                   output = NULL) {
  target <- match.arg(target)
  assessment <- twb_migration_assessment(x, target = target)
  summary <- assessment$summary
  compatibility <- assessment$compatibility
  detected <- compatibility[compatibility$detected, , drop = FALSE]

  lines <- c(
    paste0("# Tableau Migration Brief: ", summary$workbook_file[[1]]),
    "",
    paste0("Target: `", target, "`"),
    paste0("Estimated effort: **", summary$migration_effort[[1]], "**"),
    paste0("Migration score: **", summary$migration_score[[1]], "**"),
    "",
    "## Inventory",
    "",
    .markdown_table(summary[, c("worksheets", "dashboards", "datasources", "calculated_fields",
                                "lod_calculations", "table_calculations"), drop = FALSE]),
    "",
    "## Compatibility Flags",
    "",
    if (nrow(detected)) .markdown_table(detected[, c("feature", "impact", "support", "note"), drop = FALSE]) else "No major migration flags detected.",
    "",
    "## Recommendations",
    "",
    paste0("- ", assessment$recommendations)
  )

  out <- paste(lines, collapse = "\n")
  if (!is.null(output)) {
    writeLines(out, output, useBytes = TRUE)
  }
  out
}

#' Scaffold a Shiny dashboard rebuild
#'
#' Writes a minimal `app.R` with one tab per Tableau dashboard and placeholders
#' for worksheets detected in the workbook.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param path Output directory.
#'
#' @return Path to the written `app.R`.
#'
#' @export
scaffold_shiny_dashboard <- function(x, path = "shiny-scaffold") {
  xml_doc <- .twb_resolve_xml(x)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dashboards <- safe_call(twb_dashboard_sheets(xml_doc), tibble::tibble())
  pages <- safe_call(.ins_pages(xml_doc), tibble::tibble())
  dash_names <- unique(dashboards$dashboard)
  if (!length(dash_names)) dash_names <- pages$name[pages$page_type == "worksheet"]
  if (!length(dash_names)) dash_names <- "Workbook"

  tab_lines <- vapply(dash_names, function(dash) {
    sheets <- unique(dashboards$sheet[dashboards$dashboard == dash])
    if (!length(sheets)) sheets <- dash
    cards <- paste0("      tags$li(\"", .escape_r_string(sheets), "\")", collapse = ",\n")
    sprintf(
      "    tabPanel(\"%s\",\n      h3(\"%s\"),\n      p(\"Replace these placeholders with rebuilt visuals.\"),\n      tags$ul(\n%s\n      )\n    )",
      .escape_r_string(dash), .escape_r_string(dash), cards
    )
  }, character(1L))

  app <- c(
    "library(shiny)",
    "",
    "ui <- navbarPage(",
    "  title = \"Tableau Migration Scaffold\",",
    paste(tab_lines, collapse = ",\n"),
    ")",
    "",
    "server <- function(input, output, session) {",
    "}",
    "",
    "shinyApp(ui, server)"
  )

  out <- file.path(path, "app.R")
  writeLines(app, out, useBytes = TRUE)
  normalizePath(out, winslash = "/", mustWork = FALSE)
}

#' Scaffold a Quarto dashboard rebuild
#'
#' Writes a minimal Quarto dashboard with one section per Tableau dashboard and
#' worksheet placeholders.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param path Output directory.
#'
#' @return Path to the written `.qmd` file.
#'
#' @export
scaffold_quarto_dashboard <- function(x, path = "quarto-scaffold") {
  xml_doc <- .twb_resolve_xml(x)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dashboards <- safe_call(twb_dashboard_sheets(xml_doc), tibble::tibble())
  pages <- safe_call(.ins_pages(xml_doc), tibble::tibble())
  dash_names <- unique(dashboards$dashboard)
  if (!length(dash_names)) dash_names <- pages$name[pages$page_type == "worksheet"]
  if (!length(dash_names)) dash_names <- "Workbook"

  sections <- unlist(lapply(dash_names, function(dash) {
    sheets <- unique(dashboards$sheet[dashboards$dashboard == dash])
    if (!length(sheets)) sheets <- dash
    c(
      paste0("## ", dash),
      "",
      "::: {.card}",
      paste0("### ", sheets, collapse = "\n\n"),
      "",
      "Replace this placeholder with the rebuilt visual.",
      ":::",
      ""
    )
  }), use.names = FALSE)

  qmd <- c(
    "---",
    "title: \"Tableau Migration Scaffold\"",
    "format: dashboard",
    "---",
    "",
    sections
  )

  out <- file.path(path, "dashboard.qmd")
  writeLines(qmd, out, useBytes = TRUE)
  normalizePath(out, winslash = "/", mustWork = FALSE)
}

.audit_resolve_files <- function(path, recursive = TRUE, pattern = NULL) {
  files <- character()
  for (p in path) {
    if (dir.exists(p)) {
      files <- c(files, list.files(
        p,
        pattern = "\\.(twb|twbx)$",
        recursive = recursive,
        full.names = TRUE,
        ignore.case = TRUE
      ))
    } else if (file.exists(p) && grepl("\\.(twb|twbx)$", p, ignore.case = TRUE)) {
      files <- c(files, p)
    }
  }
  files <- unique(normalizePath(files, winslash = "/", mustWork = FALSE))
  if (!is.null(pattern)) {
    files <- files[grepl(pattern, basename(files), perl = TRUE)]
  }
  sort(files)
}

.audit_one_workbook <- function(parser, file, workbook_id) {
  ov <- parser$get_overview()
  pages <- parser$get_pages()
  layout <- parser$get_dashboard_layout()
  calcs <- parser$get_calc_complexity()
  manifest <- parser$get_twbx_manifest()

  n_of <- function(tbl) if (is.null(tbl)) 0L else as.integer(nrow(tbl))
  has_rows <- function(tbl) n_of(tbl) > 0L
  has_calc_type <- function(type) has_rows(calcs) && any(calcs$calc_type == type, na.rm = TRUE)

  complexity_points <- c(
    ov$dashboards[[1]] >= 5L,
    ov$calculated_fields[[1]] >= 25L,
    has_calc_type("lod"),
    has_calc_type("table_calc"),
    has_rows(parser$get_custom_sql()),
    has_rows(parser$get_dashboard_actions()),
    has_rows(parser$get_published_refs()),
    has_rows(layout) && any(layout$layout_type == "floating", na.rm = TRUE),
    has_rows(manifest) && any(manifest$type == "extract", na.rm = TRUE)
  )
  score <- sum(complexity_points, na.rm = TRUE)

  tibble::tibble(
    workbook = workbook_id,
    file = normalizePath(file, winslash = "/", mustWork = FALSE),
    file_type = tolower(tools::file_ext(file)),
    worksheets = as.integer(sum(pages$page_type == "worksheet", na.rm = TRUE)),
    dashboards = as.integer(ov$dashboards[[1]]),
    stories = as.integer(sum(pages$page_type == "story", na.rm = TRUE)),
    datasources = as.integer(ov$datasources[[1]]),
    parameters = as.integer(ov$parameters[[1]]),
    raw_fields = as.integer(ov$raw_fields[[1]]),
    calculated_fields = as.integer(ov$calculated_fields[[1]]),
    relationships = as.integer(ov$relationships[[1]]),
    inferred_relationships = as.integer(ov$inferred_relationships[[1]]),
    joins = n_of(parser$get_joins()),
    worksheet_filters = n_of(parser$get_sheet_filters()),
    dashboard_actions = n_of(parser$get_dashboard_actions()),
    custom_sql_blocks = n_of(parser$get_custom_sql()),
    initial_sql_blocks = n_of(parser$get_initial_sql()),
    published_refs = n_of(parser$get_published_refs()),
    packaged_assets = n_of(manifest),
    has_lod_calcs = has_calc_type("lod"),
    has_table_calcs = has_calc_type("table_calc"),
    has_custom_sql = has_rows(parser$get_custom_sql()),
    has_dashboard_actions = has_rows(parser$get_dashboard_actions()),
    has_published_refs = has_rows(parser$get_published_refs()),
    has_floating_layout = has_rows(layout) && any(layout$layout_type == "floating", na.rm = TRUE),
    has_packaged_extracts = has_rows(manifest) && any(manifest$type == "extract", na.rm = TRUE),
    migration_complexity_score = as.integer(score),
    migration_complexity = dplyr::case_when(
      score >= 5L ~ "high",
      score >= 2L ~ "medium",
      TRUE ~ "low"
    )
  )
}

.tag_workbook <- function(tbl, workbook_id, file) {
  if (is.null(tbl) || !nrow(tbl)) return(tibble::tibble())
  dplyr::mutate(
    tbl,
    workbook = workbook_id,
    file = normalizePath(file, winslash = "/", mustWork = FALSE),
    .before = 1L
  )
}

.empty_audit_result <- function() {
  structure(
    list(
      workbooks = tibble::tibble(),
      datasources = tibble::tibble(),
      calculated_fields = tibble::tibble(),
      field_usage = tibble::tibble(),
      lineage_edges = tibble::tibble(),
      issues = tibble::tibble(workbook = character(), file = character(), issue = character())
    ),
    class = "twbparser_audit"
  )
}

.write_audit_csv <- function(audit, output_dir = NULL) {
  if (is.null(output_dir)) {
    output_dir <- file.path(getwd(), "twbparser-audit")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  for (nm in names(audit)) {
    if (inherits(audit[[nm]], "data.frame")) {
      utils::write.csv(audit[[nm]], file.path(output_dir, paste0(nm, ".csv")), row.names = FALSE)
    }
  }
  invisible(output_dir)
}

.lineage_id <- function(type, ...) {
  parts <- lapply(list(...), function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x)] <- "<unknown>"
    x
  })
  n <- max(vapply(parts, length, integer(1L)), 1L)
  parts <- lapply(parts, rep_len, length.out = n)
  do.call(paste, c(list(type), parts, sep = "::"))
}

.lineage_field_lookup <- function(fields, calcs) {
  raw <- if (nrow(fields)) {
    fields |>
      dplyr::filter(!is.na(.data$field_clean), nzchar(.data$field_clean)) |>
      dplyr::distinct(.data$datasource, .data$field_clean) |>
      dplyr::transmute(
        datasource = .data$datasource,
        field_clean = .data$field_clean,
        field_id = .lineage_id("field", .data$datasource, .data$field_clean)
      )
  } else {
    tibble::tibble(datasource = character(), field_clean = character(), field_id = character())
  }

  calc <- if (nrow(calcs)) {
    calcs |>
      dplyr::filter(!is.na(.data$name), nzchar(.data$name)) |>
      dplyr::distinct(.data$datasource, .data$name) |>
      dplyr::transmute(
        datasource = .data$datasource,
        field_clean = .data$name,
        field_id = .lineage_id("calc", .data$datasource, .data$name)
      )
  } else {
    tibble::tibble(datasource = character(), field_clean = character(), field_id = character())
  }

  dplyr::bind_rows(calc, raw) |>
    dplyr::distinct(.data$datasource, .data$field_clean, .keep_all = TRUE)
}

.calc_dependency_edges <- function(calcs, field_lookup) {
  if (!nrow(calcs)) {
    return(tibble::tibble(from = character(), to = character(), relationship = character()))
  }

  rows <- lapply(seq_len(nrow(calcs)), function(i) {
    deps <- unique(.extract_tokens(calcs$formula[[i]]))
    deps <- deps[nzchar(deps)]
    if (!length(deps)) return(tibble::tibble())

    ds <- calcs$datasource[[i]]
    to <- .lineage_id("calc", ds, calcs$name[[i]])
    from <- vapply(deps, function(dep) {
      hit <- field_lookup[field_lookup$datasource == ds & field_lookup$field_clean == dep, , drop = FALSE]
      if (nrow(hit)) hit$field_id[[1]] else .lineage_id("field", ds, dep)
    }, character(1L))
    tibble::tibble(from = from, to = to, relationship = "used_by_calculation")
  })

  dplyr::bind_rows(rows) |> dplyr::distinct()
}

.usage_edges <- function(field_usage, field_lookup, calcs) {
  if (!nrow(field_usage)) {
    return(tibble::tibble(from = character(), to = character(), relationship = character()))
  }
  calc_keys <- if (nrow(calcs)) paste(calcs$datasource, calcs$name, sep = "\r") else character()

  rows <- lapply(seq_len(nrow(field_usage)), function(i) {
    ds <- field_usage$datasource[[i]]
    field <- field_usage$field_clean[[i]]
    key <- paste(ds, field, sep = "\r")

    from <- if (key %in% calc_keys) {
      .lineage_id("calc", ds, field)
    } else {
      hit <- field_lookup[field_lookup$datasource == ds & field_lookup$field_clean == field, , drop = FALSE]
      if (nrow(hit)) hit$field_id[[1]] else .lineage_id("field", ds, field)
    }

    tibble::tibble(
      from = from,
      to = .lineage_id("worksheet", field_usage$sheet[[i]]),
      relationship = field_usage$context[[i]] %||% "used_on_sheet"
    )
  })

  dplyr::bind_rows(rows) |> dplyr::distinct()
}

.lineage_to_mermaid <- function(lineage) {
  if (!nrow(lineage$edges)) return("flowchart LR\n")
  ids <- unique(c(lineage$edges$from, lineage$edges$to))
  alias <- stats::setNames(paste0("n", seq_along(ids)), ids)
  labels <- stats::setNames(lineage$nodes$label, lineage$nodes$id)
  label_for <- function(id) {
    label <- unname(labels[id]) %||% id
    gsub('"', "'", label, fixed = TRUE)
  }
  lines <- c(
    "flowchart LR",
    vapply(ids, function(id) sprintf('  %s["%s"]', alias[[id]], label_for(id)), character(1L)),
    vapply(seq_len(nrow(lineage$edges)), function(i) {
      sprintf(
        "  %s -- %s --> %s",
        alias[[lineage$edges$from[[i]]]],
        lineage$edges$relationship[[i]],
        alias[[lineage$edges$to[[i]]]]
      )
    }, character(1L))
  )
  paste(lines, collapse = "\n")
}

.lineage_label_from_id <- function(id) {
  parts <- strsplit(id, "::", fixed = TRUE)[[1]]
  utils::tail(parts, 1L)
}

.as_parser_or_null <- function(x) {
  if (inherits(x, "TwbParser")) x else NULL
}

.overview_from_xml <- function(xml_doc) {
  pages <- safe_call(.ins_pages(xml_doc), tibble::tibble())
  ds <- safe_call(extract_datasource_details(xml_doc), list(data_sources = tibble::tibble(), parameters = tibble::tibble()))
  calcs <- safe_call(extract_calculated_fields(xml_doc), tibble::tibble())
  fields <- safe_call(extract_columns_with_table_source(xml_doc), tibble::tibble())
  rels <- safe_call(extract_relationships(xml_doc), tibble::tibble())
  inferred <- safe_call(infer_implicit_relationships(fields), tibble::tibble())
  filters <- safe_call(.ins_sheet_filters(xml_doc), tibble::tibble())

  tibble::tibble(
    file = "<inline>",
    datasources = nrow(ds$data_sources),
    parameters = nrow(ds$parameters),
    relationships = nrow(rels),
    calculated_fields = nrow(calcs),
    raw_fields = nrow(fields),
    inferred_relationships = nrow(inferred),
    dashboards = as.integer(sum(pages$page_type == "dashboard", na.rm = TRUE)),
    total_filters = nrow(filters)
  )
}

.compatibility_support <- function() {
  list(
    powerbi = c(
      lod_calculations = "manual_rebuild",
      table_calculations = "manual_rebuild",
      parameters = "partial",
      dashboard_actions = "partial",
      custom_sql = "supported_review",
      published_datasources = "manual_reconnect",
      stories = "unsupported",
      floating_layout = "manual_redesign",
      packaged_extracts = "manual_replatform"
    ),
    shiny = c(
      lod_calculations = "code_rebuild",
      table_calculations = "code_rebuild",
      parameters = "supported",
      dashboard_actions = "code_rebuild",
      custom_sql = "supported_review",
      published_datasources = "manual_reconnect",
      stories = "manual_rebuild",
      floating_layout = "manual_redesign",
      packaged_extracts = "manual_replatform"
    ),
    quarto = c(
      lod_calculations = "code_rebuild",
      table_calculations = "code_rebuild",
      parameters = "partial",
      dashboard_actions = "limited",
      custom_sql = "supported_review",
      published_datasources = "manual_reconnect",
      stories = "manual_rebuild",
      floating_layout = "manual_redesign",
      packaged_extracts = "manual_replatform"
    ),
    looker = c(
      lod_calculations = "semantic_model_rebuild",
      table_calculations = "manual_rebuild",
      parameters = "partial",
      dashboard_actions = "limited",
      custom_sql = "model_review",
      published_datasources = "manual_reconnect",
      stories = "unsupported",
      floating_layout = "manual_redesign",
      packaged_extracts = "manual_replatform"
    ),
    superset = c(
      lod_calculations = "sql_or_metric_rebuild",
      table_calculations = "manual_rebuild",
      parameters = "partial",
      dashboard_actions = "limited",
      custom_sql = "supported_review",
      published_datasources = "manual_reconnect",
      stories = "unsupported",
      floating_layout = "manual_redesign",
      packaged_extracts = "manual_replatform"
    )
  )
}

.assessment_recommendations <- function(summary, compatibility) {
  recs <- c(
    "Start with datasource reconnection and field inventory before rebuilding visuals.",
    "Use lineage outputs to identify dashboards affected by each datasource and calculated field."
  )
  detected <- compatibility[compatibility$detected, , drop = FALSE]
  if (any(detected$feature == "lod_calculations")) {
    recs <- c(recs, "Review LOD calculations early; they often define the semantic model needed in the target tool.")
  }
  if (any(detected$feature == "table_calculations")) {
    recs <- c(recs, "Validate table calculations against the final visual grain after rebuilding charts.")
  }
  if (any(detected$feature == "custom_sql")) {
    recs <- c(recs, "Move custom SQL into governed views or documented model queries where possible.")
  }
  if (any(detected$feature == "floating_layout")) {
    recs <- c(recs, "Redesign floating dashboard layouts into responsive target layouts instead of copying pixel positions exactly.")
  }
  if (identical(summary$migration_effort[[1]], "high")) {
    recs <- c(recs, "Pilot one representative dashboard before committing to the full workbook migration.")
  }
  unique(recs)
}

.formula_review_flags <- function(formula) {
  if (is.na(formula) || !nzchar(formula)) return("Empty formula")
  flags <- character()
  if (grepl("\\{\\s*(FIXED|INCLUDE|EXCLUDE)\\b", formula, ignore.case = TRUE, perl = TRUE)) {
    flags <- c(flags, "LOD expression needs semantic review")
  }
  if (grepl("\\b(WINDOW_|LOOKUP\\(|INDEX\\(|RUNNING_|RANK\\(|PREVIOUS_VALUE\\()", formula, ignore.case = TRUE, perl = TRUE)) {
    flags <- c(flags, "Table calculation needs visual-grain review")
  }
  unsupported <- unique(unlist(regmatches(
    formula,
    gregexpr("\\b(RAWSQL|SCRIPT_|MODEL_EXTENSION_)\\w*\\b", formula, ignore.case = TRUE, perl = TRUE)
  )))
  if (length(unsupported)) {
    flags <- c(flags, paste0("Unsupported or external function: ", paste(unsupported, collapse = ", ")))
  }
  flags
}

.translate_formula_one <- function(formula, target) {
  if (is.na(formula)) return(NA_character_)
  out <- formula
  out <- gsub("\\[([^\\]]+)\\]", .target_field_replacement(target), out, perl = TRUE)

  if (identical(target, "sql")) {
    out <- .rewrite_if_then(out, "sql")
    out <- gsub("\\bZN\\s*\\(", "COALESCE(", out, ignore.case = TRUE, perl = TRUE)
    out <- gsub("\\bIFNULL\\s*\\(", "COALESCE(", out, ignore.case = TRUE, perl = TRUE)
    out <- gsub("\\bISNULL\\s*\\(", "IS NULL(", out, ignore.case = TRUE, perl = TRUE)
  } else if (identical(target, "dax")) {
    out <- .rewrite_if_then(out, "dax")
    out <- gsub("\\bZN\\s*\\(", "COALESCE(", out, ignore.case = TRUE, perl = TRUE)
    out <- gsub("\\bIFNULL\\s*\\(", "COALESCE(", out, ignore.case = TRUE, perl = TRUE)
    out <- gsub("\\bDATEPART\\s*\\(", "/* review DATEPART */ DATEPART(", out, ignore.case = TRUE, perl = TRUE)
  } else if (identical(target, "r")) {
    out <- .rewrite_if_then(out, "r")
    out <- gsub("\\bZN\\s*\\(([^\\)]+)\\)", "dplyr::coalesce(\\1, 0)", out, ignore.case = TRUE, perl = TRUE)
    out <- gsub("\\bIFNULL\\s*\\(", "dplyr::coalesce(", out, ignore.case = TRUE, perl = TRUE)
    out <- gsub("\\bISNULL\\s*\\(", "is.na(", out, ignore.case = TRUE, perl = TRUE)
  }

  out
}

.target_field_replacement <- function(target) {
  switch(
    target,
    sql = "\"\\1\"",
    dax = "'Table'[\\1]",
    r = "`\\1`",
    "\\1"
  )
}

.rewrite_if_then <- function(formula, target) {
  pattern <- "^\\s*IF\\s+(.+)\\s+THEN\\s+(.+)\\s+ELSE\\s+(.+)\\s+END\\s*$"
  if (!grepl(pattern, formula, ignore.case = TRUE, perl = TRUE)) return(formula)
  parts <- regmatches(formula, regexec(pattern, formula, ignore.case = TRUE, perl = TRUE))[[1]]
  condition <- parts[[2]]
  yes <- parts[[3]]
  no <- parts[[4]]
  switch(
    target,
    sql = paste0("CASE WHEN ", condition, " THEN ", yes, " ELSE ", no, " END"),
    dax = paste0("IF(", condition, ", ", yes, ", ", no, ")"),
    r = paste0("dplyr::if_else(", condition, ", ", yes, ", ", no, ")"),
    formula
  )
}

.calc_target_for_tool <- function(target) {
  switch(
    target,
    powerbi = "dax",
    shiny = "r",
    quarto = "r",
    looker = "sql",
    superset = "sql",
    "sql"
  )
}

.write_csv_file <- function(tbl, path, filename) {
  out <- file.path(path, filename)
  utils::write.csv(tbl, out, row.names = FALSE)
  out
}

.markdown_table <- function(tbl) {
  if (is.null(tbl) || !nrow(tbl)) return("")
  tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)
  tbl[] <- lapply(tbl, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    gsub("\\|", "\\\\|", x)
  })
  header <- paste0("| ", paste(names(tbl), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", ncol(tbl)), collapse = " | "), " |")
  rows <- apply(tbl, 1L, function(x) paste0("| ", paste(x, collapse = " | "), " |"))
  paste(c(header, sep, rows), collapse = "\n")
}

.escape_r_string <- function(x) {
  gsub('"', '\\"', x, fixed = TRUE)
}
