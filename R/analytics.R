#' @importFrom igraph gorder is_dag topo_sort induced_subgraph neighbors V
#' @importFrom dplyr mutate select arrange distinct filter bind_rows count rename
#' @importFrom tidyr pivot_wider
#' @importFrom tibble tibble
#' @importFrom utils capture.output
NULL

# ---- Internal helpers --------------------------------------------------------

#' Classify a vector of Tableau formulas into computation categories
#' Precedence: lod > table_calc > aggregate > raw
#' @keywords internal
#' @noRd
.classify_calc_type <- function(formula, is_table_calc) {
  vapply(seq_along(formula), function(i) {
    f  <- formula[[i]]
    tc <- isTRUE(is_table_calc[[i]])
    if (is.na(f))
      return("raw")
    if (grepl("\\{\\s*(FIXED|INCLUDE|EXCLUDE)\\b", f,
              ignore.case = TRUE, perl = TRUE))
      return("lod")
    if (tc)
      return("table_calc")
    if (grepl(
      paste0("\\b(SUM|AVG|MIN|MAX|COUNT|COUNTD|MEDIAN|STDEV|STDEVP|",
             "VAR|VARP|ATTR|AGG|RAWSQLAGG|PERCENTILE|CORR|COVAR|COVARP)\\s*\\("),
      f, ignore.case = TRUE, perl = TRUE))
      return("aggregate")
    "raw"
  }, character(1L))
}

#' Extract the LOD sub-type (fixed / include / exclude) from a formula vector
#' @keywords internal
#' @noRd
.extract_lod_type <- function(formula, calc_type) {
  vapply(seq_along(formula), function(i) {
    if (identical(calc_type[[i]], "lod") && !is.na(formula[[i]])) {
      m <- regmatches(
        formula[[i]],
        regexpr("(FIXED|INCLUDE|EXCLUDE)", formula[[i]],
                ignore.case = TRUE, perl = TRUE)
      )
      if (length(m) && nzchar(m[[1L]])) tolower(m[[1L]]) else NA_character_
    } else {
      NA_character_
    }
  }, character(1L))
}

#' Count distinct bracketed field tokens in each formula
#' @keywords internal
#' @noRd
.count_formula_deps <- function(formula) {
  vapply(formula, function(f) {
    if (is.na(f)) 0L else length(unique(.extract_tokens(f)))
  }, integer(1L))
}

#' Longest-path depth through the calc-field subgraph (DP over topo order)
#' @keywords internal
#' @noRd
.dep_depths <- function(g, calc_names) {
  n_calcs <- length(calc_names)
  if (igraph::gorder(g) == 0L || n_calcs == 0L)
    return(rep(0L, n_calcs))

  all_verts  <- igraph::V(g)$name
  calc_verts <- intersect(all_verts, calc_names)
  if (length(calc_verts) == 0L)
    return(rep(0L, n_calcs))

  # Sub-graph of calc fields only so raw-field hops are not counted
  sub_g     <- igraph::induced_subgraph(
    g, vids = igraph::V(g)[all_verts %in% calc_verts]
  )
  sub_names <- igraph::V(sub_g)$name

  if (!igraph::is_dag(sub_g)) {
    warning(
      "Circular dependencies detected in calculated fields. ",
      "dep_depth is set to NA for all fields.",
      call. = FALSE
    )
    return(rep(NA_integer_, n_calcs))
  }

  # DP: dp[i] = longest path from any source vertex to vertex i
  dp    <- rep(0L, igraph::gorder(sub_g))
  topo  <- igraph::topo_sort(sub_g, mode = "out")   # sources first

  for (v in as.integer(topo)) {
    preds <- as.integer(igraph::neighbors(sub_g, v, mode = "in"))
    if (length(preds) > 0L)
      dp[[v]] <- max(dp[preds]) + 1L
  }

  unname(vapply(calc_names, function(nm) {
    idx <- match(nm, sub_names)
    if (!is.na(idx)) dp[[idx]] else 0L
  }, integer(1L)))
}

# ---- twb_calc_complexity -----------------------------------------------------

#' Classify calculated fields by complexity
#'
#' Returns every calculated field in the workbook enriched with a computation
#' category (`calc_type`), LOD sub-type, dependency count, and dependency depth
#' — the maximum number of calc-on-calc hops in the field's dependency chain.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param include_parameters Logical; if `TRUE`, include parameter fields
#'   (they always land in `calc_type = "raw"` and `dep_depth = 0`).
#'   Default `FALSE`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{datasource}{Datasource the field belongs to.}
#'   \item{name}{Human-readable field name.}
#'   \item{tableau_internal_name}{Bracketed internal Tableau name.}
#'   \item{datatype}{Field data type.}
#'   \item{role}{`"measure"` or `"dimension"`.}
#'   \item{calc_type}{One of `"lod"`, `"table_calc"`, `"aggregate"`, `"raw"`.
#'     Tested in that precedence order.}
#'   \item{lod_type}{`"fixed"`, `"include"`, or `"exclude"`; `NA` if not LOD.}
#'   \item{is_table_calc}{Logical; existing heuristic flag preserved for
#'     backward compatibility.}
#'   \item{dep_depth}{Integer; longest chain of calc-on-calc dependencies.
#'     `0` means the field only references raw fields (or has no references).}
#'   \item{n_deps}{Integer; count of distinct bracketed tokens in the formula.}
#'   \item{formula}{Raw formula string.}
#' }
#'
#' @examples
#' twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
#' stopifnot(nzchar(twb), file.exists(twb))
#' xml <- xml2::read_xml(twb)
#' twb_calc_complexity(xml)
#'
#' @export
twb_calc_complexity <- function(x, include_parameters = FALSE) {
  xml_doc <- .twb_resolve_xml(x)
  stopifnot(
    is.logical(include_parameters), length(include_parameters) == 1L,
    !is.na(include_parameters)
  )
  .ins_calc_complexity(xml_doc, include_parameters)
}

#' @keywords internal
#' @noRd
.ins_calc_complexity <- function(xml_doc, include_parameters = FALSE) {
  calcs <- extract_calculated_fields(xml_doc,
                                     include_parameters = include_parameters)
  if (nrow(calcs) == 0L) return(.empty_calc_complexity())

  calcs <- calcs |>
    dplyr::mutate(
      calc_type = .classify_calc_type(formula, is_table_calc),
      lod_type  = .extract_lod_type(formula, calc_type),
      n_deps    = .count_formula_deps(formula)
    )

  dep_d <- .dep_depths(
    build_dependency_graph(calcs),
    calc_names = calcs$name
  )

  calcs |>
    dplyr::mutate(dep_depth = dep_d) |>
    dplyr::select(
      "datasource", "name", "tableau_internal_name", "datatype", "role",
      "calc_type", "lod_type", "is_table_calc", "dep_depth", "n_deps",
      "formula"
    ) |>
    dplyr::arrange(.data$datasource, .data$calc_type, .data$name)
}

.empty_calc_complexity <- function() {
  tibble::tibble(
    datasource            = character(),
    name                  = character(),
    tableau_internal_name = character(),
    datatype              = character(),
    role                  = character(),
    calc_type             = character(),
    lod_type              = character(),
    is_table_calc         = logical(),
    dep_depth             = integer(),
    n_deps                = integer(),
    formula               = character()
  )
}

# ---- twb_field_usage ---------------------------------------------------------

#' Field usage matrix across worksheets
#'
#' Combines shelf placement and filter usage into a tidy long tibble showing
#' where each field appears and in what capacity across all (or selected)
#' worksheets.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param include_filters Logical; include filter appearances. Default `TRUE`.
#' @param include_shelves Logical; include shelf appearances (rows, cols, color,
#'   size, etc.). Default `TRUE`.
#' @param wide Logical; if `TRUE`, pivot to one row per field with one column
#'   per sheet containing a comma-separated list of contexts, or `NA` if the
#'   field does not appear on that sheet. Default `FALSE`.
#'
#' @return
#' **Long form** (`wide = FALSE`): a tibble with columns:
#' \describe{
#'   \item{field_clean}{Human-readable field name.}
#'   \item{datasource}{Datasource the field belongs to.}
#'   \item{sheet}{Worksheet name.}
#'   \item{context}{Usage context, e.g. `"shelf:rows"`, `"shelf:color"`,
#'     `"filter"`.}
#'   \item{n_appearances}{Number of times the field appears in this context on
#'     this sheet (handles multi-pill rows/cols).}
#' }
#'
#' **Wide form** (`wide = TRUE`): one row per `(field_clean, datasource)`,
#' one column per sheet, cell value is a comma-separated context string or
#' `NA`.
#'
#' @examples
#' twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
#' stopifnot(nzchar(twb), file.exists(twb))
#' xml <- xml2::read_xml(twb)
#' twb_field_usage(xml)
#' twb_field_usage(xml, wide = TRUE)
#'
#' @export
twb_field_usage <- function(x,
                             include_filters  = TRUE,
                             include_shelves  = TRUE,
                             wide             = FALSE) {
  xml_doc <- .twb_resolve_xml(x)
  stopifnot(
    is.logical(include_filters),  length(include_filters)  == 1L,
    is.logical(include_shelves),  length(include_shelves)  == 1L,
    is.logical(wide),             length(wide)             == 1L
  )
  if (!include_filters && !include_shelves) {
    message("`include_filters` and `include_shelves` are both FALSE: ",
            "returning empty tibble.")
    return(.empty_field_usage())
  }
  .ins_field_usage(xml_doc, include_filters, include_shelves, wide)
}

#' @keywords internal
#' @noRd
.ins_field_usage <- function(xml_doc,
                              include_filters = TRUE,
                              include_shelves = TRUE,
                              wide            = FALSE) {
  parts <- list()

  if (include_shelves) {
    sh <- .ins_sheet_shelves(xml_doc)
    if (nrow(sh) > 0L) {
      parts[["shelves"]] <- sh |>
        dplyr::filter(!is.na(.data$field_clean), nzchar(.data$field_clean)) |>
        dplyr::mutate(context = paste0("shelf:", .data$shelf)) |>
        dplyr::select("field_clean", "datasource", "sheet", "context")
    }
  }

  if (include_filters) {
    fl <- .ins_sheet_filters(xml_doc)
    if (nrow(fl) > 0L) {
      parts[["filters"]] <- fl |>
        dplyr::filter(!is.na(.data$field_clean), nzchar(.data$field_clean)) |>
        dplyr::mutate(context = "filter") |>
        dplyr::select("field_clean", "datasource", "sheet", "context")
    }
  }

  if (length(parts) == 0L) return(.empty_field_usage())

  combined <- dplyr::bind_rows(parts) |>
    dplyr::count(.data$field_clean, .data$datasource, .data$sheet,
                 .data$context, name = "n_appearances") |>
    dplyr::mutate(n_appearances = as.integer(.data$n_appearances)) |>
    dplyr::arrange(.data$field_clean, .data$sheet, .data$context)

  if (!wide) return(combined)

  # Wide form: one row per (field_clean, datasource), one col per sheet
  tidyr::pivot_wider(
    combined,
    id_cols     = c("field_clean", "datasource"),
    names_from  = "sheet",
    values_from = "context",
    values_fn   = function(ctx) paste(sort(unique(ctx)), collapse = ", ")
  )
}

.empty_field_usage <- function() {
  tibble::tibble(
    field_clean   = character(),
    datasource    = character(),
    sheet         = character(),
    context       = character(),
    n_appearances = integer()
  )
}

# ---- twb_replication_brief ---------------------------------------------------

#' Replication brief for a Tableau workbook or dashboard
#'
#' Assembles all extracted intelligence — datasources, parameters, calculated
#' fields with complexity classifications, field usage, filters, sorts, chart
#' types, dashboard layout, and actions — into a single named list (or
#' formatted text) ready for use when porting to another visualisation tool.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param dashboard Optional character scalar. When supplied, sheet-level
#'   sections (filters, sorts, chart types, field usage, layout) are scoped to
#'   the sheets that belong to this dashboard.
#' @param include_sql Logical; include custom SQL blocks in `$custom_sql`.
#'   Default `TRUE`.
#' @param include_formulas Logical; when `TRUE`, a `formula_pretty` column is
#'   added to `$calculated_fields`. Default `TRUE`.
#' @param format Either `"list"` (default) to return a named R list, or
#'   `"text"` to return a single formatted character string suitable for
#'   printing or writing to a file.
#'
#' @return
#' **`format = "list"`**: a named list with elements:
#' \describe{
#'   \item{meta}{1-row tibble: file name, counts, generation timestamp.}
#'   \item{datasources}{Datasource connection details.}
#'   \item{parameters}{Parameter fields with current values.}
#'   \item{custom_sql}{Custom SQL blocks, or `NULL` if `include_sql = FALSE`.}
#'   \item{calculated_fields}{Tibble from [twb_calc_complexity()], optionally
#'     with a `formula_pretty` column.}
#'   \item{field_usage}{Tibble from [twb_field_usage()].}
#'   \item{filters}{Worksheet filters (scoped to `dashboard` if given).}
#'   \item{sorts}{Worksheet sorts (scoped to `dashboard` if given).}
#'   \item{chart_types}{Mark types per worksheet.}
#'   \item{dashboard_layout}{Zone positions from [twb_dashboard_sheets()].}
#'   \item{actions}{Dashboard actions from [twb_dashboard_actions()].}
#' }
#'
#' **`format = "text"`**: a single `character(1)` with section headers and
#' tabular output.
#'
#' @examples
#' twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
#' stopifnot(nzchar(twb), file.exists(twb))
#' xml <- xml2::read_xml(twb)
#' brief <- twb_replication_brief(xml)
#' names(brief)
#' brief$meta
#'
#' @export
twb_replication_brief <- function(x,
                                   dashboard        = NULL,
                                   include_sql      = TRUE,
                                   include_formulas = TRUE,
                                   format           = c("list", "text")) {
  xml_doc <- .twb_resolve_xml(x)
  format  <- match.arg(format)

  if (!is.null(dashboard)) {
    stopifnot(is.character(dashboard), length(dashboard) == 1L)
    dashboard <- gsub("'", "", dashboard, fixed = TRUE)
  }
  stopifnot(
    is.logical(include_sql),      length(include_sql)      == 1L,
    is.logical(include_formulas), length(include_formulas) == 1L
  )

  brief <- .ins_replication_brief(xml_doc, x, dashboard,
                                   include_sql, include_formulas)

  if (identical(format, "text")) return(.brief_to_text(brief))
  brief
}

#' @keywords internal
#' @noRd
.ins_replication_brief <- function(xml_doc, x_orig, dashboard,
                                    include_sql, include_formulas) {
  # -- meta -------------------------------------------------------------------
  workbook_file <- if (inherits(x_orig, "TwbParser")) {
    basename(x_orig$path %||% "")
  } else {
    "<inline>"
  }

  pages        <- .ins_pages(xml_doc)
  n_worksheets <- sum(pages$page_type == "worksheet", na.rm = TRUE)
  n_dashboards <- sum(pages$page_type == "dashboard", na.rm = TRUE)

  ds_details <- tryCatch(
    extract_datasource_details(xml_doc),
    error = function(e) list(data_sources = tibble::tibble(),
                             parameters   = tibble::tibble())
  )
  calcs  <- .ins_calc_complexity(xml_doc)
  params <- tryCatch(extract_parameters(xml_doc), error = function(e) tibble::tibble())

  meta <- tibble::tibble(
    workbook_file       = workbook_file,
    n_datasources       = nrow(ds_details$data_sources),
    n_worksheets        = as.integer(n_worksheets),
    n_dashboards        = as.integer(n_dashboards),
    n_calculated_fields = nrow(calcs),
    n_parameters        = nrow(params),
    generated_at        = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # -- datasources & parameters -----------------------------------------------
  datasources <- ds_details$data_sources
  parameters  <- params

  # -- custom SQL -------------------------------------------------------------
  custom_sql <- if (include_sql) {
    tryCatch(twb_custom_sql(xml_doc), error = function(e) tibble::tibble())
  } else {
    NULL
  }

  # -- calculated fields ------------------------------------------------------
  calc_fields <- calcs
  if (include_formulas && nrow(calc_fields) > 0L) {
    pretty <- tryCatch(
      prettify_calculated_fields(
        calc_fields |> dplyr::select("name", "formula"),
        wrap = 100L
      ),
      error = function(e) NULL
    )
    if (!is.null(pretty) && "formula_pretty" %in% names(pretty)) {
      calc_fields <- dplyr::bind_rows(
        calc_fields,
        tibble::tibble(formula_pretty = pretty$formula_pretty)
      ) |>
        # safe merge: join on name to avoid row-count mismatch
        (\(cf) {
          cf |> dplyr::select(-"formula_pretty") |>
            dplyr::bind_cols(
              tibble::tibble(
                formula_pretty = pretty$formula_pretty[
                  match(cf$name, pretty$name)
                ]
              )
            )
        })()
    }
  }

  # -- determine sheet scope --------------------------------------------------
  scoped_sheets <- if (!is.null(dashboard)) {
    db_sh <- tryCatch(
      .ins_dashboard_sheets(xml_doc, dashboard),
      error = function(e) tibble::tibble(sheet = character())
    )
    if (nrow(db_sh) > 0L) unique(db_sh$sheet) else character()
  } else {
    NULL   # NULL means all sheets
  }

  .scope <- function(tbl, col = "sheet") {
    if (is.null(scoped_sheets) || !col %in% names(tbl)) return(tbl)
    dplyr::filter(tbl, .data[[col]] %in% scoped_sheets)
  }

  # -- field usage ------------------------------------------------------------
  field_usage <- tryCatch(
    .scope(.ins_field_usage(xml_doc, include_filters = TRUE,
                             include_shelves = TRUE, wide = FALSE)),
    error = function(e) .empty_field_usage()
  )

  # -- filters & sorts --------------------------------------------------------
  filters <- tryCatch(
    .scope(.ins_sheet_filters(xml_doc)),
    error = function(e) .empty_filters()
  )
  sorts <- tryCatch(
    .scope(.ins_sheet_sorts(xml_doc)),
    error = function(e) .empty_sorts()
  )

  # -- chart types ------------------------------------------------------------
  chart_types <- tryCatch({
    ct <- .ins_charts(xml_doc)
    if (!is.null(scoped_sheets) && "worksheet" %in% names(ct))
      ct <- dplyr::filter(ct, .data$worksheet %in% scoped_sheets)
    ct
  }, error = function(e) tibble::tibble())

  # -- dashboard layout & actions ---------------------------------------------
  db_layout <- tryCatch(
    .ins_dashboard_sheets(xml_doc, dashboard),
    error = function(e) tibble::tibble()
  )
  actions <- tryCatch(
    .ins_dashboard_actions(xml_doc, dashboard),
    error = function(e) tibble::tibble()
  )

  list(
    meta               = meta,
    datasources        = datasources,
    parameters         = parameters,
    custom_sql         = custom_sql,
    calculated_fields  = calc_fields,
    field_usage        = field_usage,
    filters            = filters,
    sorts              = sorts,
    chart_types        = chart_types,
    dashboard_layout   = db_layout,
    actions            = actions
  )
}

# ---- text formatter ----------------------------------------------------------

#' @keywords internal
#' @noRd
.brief_to_text <- function(brief) {
  lines <- character()

  .hdr <- function(title) c(paste0("## ", title), "")

  .tbl <- function(tbl) {
    if (is.null(tbl) || (is.data.frame(tbl) && nrow(tbl) == 0L))
      return(c("  (none)", ""))
    c(utils::capture.output(print(as.data.frame(tbl), row.names = FALSE)),
      "")
  }

  .kv <- function(label, value) {
    sprintf("  %-25s %s", paste0(label, ":"), value)
  }

  # Header
  lines <- c(lines, "# TWBPARSER REPLICATION BRIEF", "")

  # Meta
  m <- brief$meta
  lines <- c(
    lines,
    .hdr("WORKBOOK"),
    .kv("File",              m$workbook_file),
    .kv("Generated at",      m$generated_at),
    .kv("Datasources",       m$n_datasources),
    .kv("Worksheets",        m$n_worksheets),
    .kv("Dashboards",        m$n_dashboards),
    .kv("Calculated fields", m$n_calculated_fields),
    .kv("Parameters",        m$n_parameters),
    ""
  )

  .section <- function(title, tbl) c(.hdr(title), .tbl(tbl))

  lines <- c(lines,
    .section("DATASOURCES",       brief$datasources),
    .section("PARAMETERS",        brief$parameters)
  )
  if (!is.null(brief$custom_sql))
    lines <- c(lines, .section("CUSTOM SQL", brief$custom_sql))

  lines <- c(lines,
    .section("CALCULATED FIELDS (complexity)", brief$calculated_fields),
    .section("FIELD USAGE",                    brief$field_usage),
    .section("FILTERS",                        brief$filters),
    .section("SORTS",                          brief$sorts),
    .section("CHART TYPES",                    brief$chart_types),
    .section("DASHBOARD LAYOUT",               brief$dashboard_layout),
    .section("ACTIONS",                        brief$actions)
  )

  paste(lines, collapse = "\n")
}
