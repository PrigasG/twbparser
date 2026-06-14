#' Build a workbook report from a TwbParser
#'
#' @param parser A `TwbParser` object.
#' @return A structured list of workbook sections with class
#'   `twbparser_report`.
#' @keywords internal
#' @noRd
twb_workbook_report <- function(parser) {
  stopifnot(inherits(parser, "TwbParser"))

  validation <- parser$last_validation %||%
    safe_call(parser$validate(error = FALSE), list(ok = NA, issues = tibble::tibble()))

  calculated_fields <- safe_call(parser$get_calc_complexity(), .empty_calc_complexity())
  if (NROW(calculated_fields)) {
    calculated_fields <- prettify_calculated_fields(calculated_fields, wrap = 100L)
  }

  structure(
    list(
      overview          = parser$get_overview(),
      pages             = parser$get_pages(),
      pages_summary     = parser$get_pages_summary(),
      sheet_shelves     = parser$get_sheet_shelves(),
      sheet_filters     = parser$get_sheet_filters(),
      sheet_axes        = parser$get_sheet_axes(),
      sheet_sorts       = parser$get_sheet_sorts(),
      dashboards        = parser$get_dashboard_summary(),
      dashboard_sheets  = parser$get_dashboard_sheets(),
      dashboard_filters = parser$get_dashboard_filters(),
      dashboard_layout  = parser$get_dashboard_layout(),
      dashboard_actions = parser$get_dashboard_actions(),
      datasources       = parser$get_datasources(),
      parameters        = parser$get_parameters(),
      calculated_fields = calculated_fields,
      custom_sql        = parser$get_custom_sql(),
      initial_sql       = parser$get_initial_sql(),
      published_refs    = parser$get_published_refs(),
      validation        = validation
    ),
    class = "twbparser_report"
  )
}

#' @export
print.twbparser_report <- function(x, ...) {
  ov <- x$overview

  cat("TWB PARSER SUMMARY\n")
  cat("------------------\n")
  .cat_kv("File", ov$file[1])
  .cat_kv("Datasources", ov$datasources[1])
  .cat_kv("Parameters", ov$parameters[1])
  .cat_kv("Worksheets", .count_page_type(x$pages, "worksheet"))
  .cat_kv("Dashboards", ov$dashboards[1])
  .cat_kv("Stories", .count_page_type(x$pages, "story"))
  .cat_kv("Relationships", ov$relationships[1])
  .cat_kv("Calculated fields", ov$calculated_fields[1])
  .cat_kv("Raw fields", ov$raw_fields[1])
  .cat_kv("Worksheet filters", nrow(x$sheet_filters))
  .cat_kv("Dashboard filters", nrow(x$dashboard_filters))
  .cat_kv("Custom SQL blocks", nrow(x$custom_sql))
  .cat_kv("Initial SQL blocks", nrow(x$initial_sql))

  .print_report_section(
    "Datasources",
    x$datasources,
    c("datasource_name", "connection_type", "field_count", "location"),
    empty = "No datasource details found."
  )

  .print_report_section(
    "Pages",
    x$pages_summary,
    c("page_type", "name", "mark_types", "n_filters", "n_legends", "n_parameter_controls"),
    empty = "No worksheets, dashboards, or stories found."
  )

  .print_report_section(
    "Worksheet Shelves",
    .summarise_shelves(x$sheet_shelves),
    c("sheet", "shelves", "fields"),
    empty = "No worksheet shelf assignments found."
  )

  .print_report_section(
    "Worksheet Filters",
    x$sheet_filters,
    c("sheet", "field_clean", "filter_class", "include_mode", "members", "range_min", "range_max"),
    empty = "No worksheet filters found."
  )

  .print_report_section(
    "Dashboard Filters",
    x$dashboard_filters,
    c("dashboard", "field", "presentation", "x", "y", "w", "h"),
    empty = "No dashboard filters found."
  )

  .print_calc_section(x$calculated_fields)

  .print_report_section(
    "SQL",
    .summarise_sql(x$custom_sql, x$initial_sql),
    c("kind", "name", "preview"),
    empty = "No custom or initial SQL found."
  )

  if (is.list(x$validation) && identical(x$validation$ok, FALSE)) {
    .print_report_section(
      "Validation Issues",
      x$validation$issues,
      names(x$validation$issues),
      empty = "Validation failed, but no issue rows were returned."
    )
  }

  invisible(x)
}

.cat_kv <- function(key, value) {
  value <- value %||% NA
  cat(sprintf("%-20s %s\n", paste0(key, ":"), as.character(value)))
}

.count_page_type <- function(pages, type) {
  if (!nrow(pages) || !"page_type" %in% names(pages)) return(0L)
  sum(pages$page_type == type, na.rm = TRUE)
}

.print_report_section <- function(title, data, columns, empty, n = 8L) {
  cat("\n", title, "\n", sep = "")
  cat(strrep("-", nchar(title)), "\n", sep = "")

  if (is.null(data) || !NROW(data)) {
    cat(empty, "\n", sep = "")
    return(invisible(NULL))
  }

  cols <- intersect(columns, names(data))
  if (!length(cols)) {
    cat(empty, "\n", sep = "")
    return(invisible(NULL))
  }

  out <- data[, cols, drop = FALSE]
  print(utils::head(out, n), n = n, width = Inf)
  if (NROW(out) > n) {
    cat(sprintf("... and %d more rows\n", NROW(out) - n))
  }
  invisible(NULL)
}

.print_calc_section <- function(calcs, n = 8L) {
  cat("\nCalculated Fields\n")
  cat("-----------------\n")

  if (is.null(calcs) || !NROW(calcs)) {
    cat("No calculated fields found.\n")
    return(invisible(NULL))
  }

  rows <- utils::head(calcs, n)
  for (i in seq_len(NROW(rows))) {
    row <- rows[i, , drop = FALSE]
    name <- .first_non_empty(row$name, row$tableau_internal_name, paste0("Calculation ", i))
    formula <- .first_non_empty(row$formula_pretty, row$formula, "")
    formula <- tableau_formula_pretty(formula, wrap = 100L)

    cat(sprintf("%d. %s\n", i, name))
    .cat_kv("Datasource", .first_non_empty(row$datasource, ""))
    .cat_kv("Type", trimws(paste(.first_non_empty(row$datatype, ""),
                                 .first_non_empty(row$role, ""))))
    .cat_kv("Calculation", .first_non_empty(row$calc_type, ""))
    .cat_kv("Table calc", .first_non_empty(row$is_table_calc, ""))
    .cat_kv("Dependencies", .first_non_empty(row$n_deps, ""))
    cat("Formula:\n")
    cat(paste0("  ", strsplit(.brief_formula_text(formula), "\n", fixed = TRUE)[[1L]]), sep = "\n")
    cat("\n")
    if (i < NROW(rows)) cat("\n")
  }

  if (NROW(calcs) > n) {
    cat(sprintf("... and %d more calculated fields\n", NROW(calcs) - n))
  }
  invisible(NULL)
}

.first_non_empty <- function(...) {
  values <- list(...)
  for (value in values) {
    if (is.null(value)) next
    value <- value[[1]]
    if (!is.na(value) && nzchar(as.character(value))) return(as.character(value))
  }
  NA_character_
}

.collapse_unique <- function(x, empty = "") {
  x <- unique(x[!is.na(x) & nzchar(x)])
  if (!length(x)) return(empty)
  paste(x, collapse = ", ")
}

.summarise_shelves <- function(shelves) {
  if (is.null(shelves) || !nrow(shelves)) {
    return(tibble::tibble(sheet = character(), shelves = character(), fields = character()))
  }

  split_shelves <- split(shelves, shelves$sheet)
  purrr::map_dfr(names(split_shelves), function(sheet_name) {
    rows <- split_shelves[[sheet_name]]
    tibble::tibble(
      sheet   = sheet_name,
      shelves = .collapse_unique(rows$shelf),
      fields  = .collapse_unique(rows$field_clean)
    )
  })
}

.summarise_sql <- function(custom_sql, initial_sql) {
  custom <- if (!is.null(custom_sql) && nrow(custom_sql)) {
    tibble::tibble(
      kind    = "custom",
      name    = custom_sql$relation_name %||% NA_character_,
      preview = .preview_text(custom_sql$custom_sql)
    )
  } else {
    tibble::tibble(kind = character(), name = character(), preview = character())
  }

  initial <- if (!is.null(initial_sql) && nrow(initial_sql)) {
    tibble::tibble(
      kind    = "initial",
      name    = initial_sql$connection_id %||% NA_character_,
      preview = .preview_text(initial_sql$initial_sql)
    )
  } else {
    tibble::tibble(kind = character(), name = character(), preview = character())
  }

  dplyr::bind_rows(custom, initial)
}

.preview_text <- function(x, max_chars = 100L) {
  x <- gsub("\\s+", " ", x %||% "")
  too_long <- nchar(x) > max_chars
  x[too_long] <- paste0(substr(x[too_long], 1L, max_chars - 3L), "...")
  x
}
