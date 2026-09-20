#' Worksheet visualization specs and dashboard chart inventories
#'
#' @description
#' `twb_sheet_spec()` reduces a worksheet to everything needed to
#' understand — and rebuild — its visualization in another tool: the mark type,
#' the fields on the rows and columns shelves (in order), the dimensions and
#' measures in play, every marks-card encoding (color, size, label, detail,
#' shape, tooltip, ...), tooltip configuration, and the worksheet's filters,
#' sorts, and axes.
#'
#' `twb_dashboard_charts()` answers "what graphs are on this dashboard?": one
#' row per worksheet placed on each dashboard, with its mark type, fields,
#' tooltip summary, and layout position.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param sheet Optional character scalar to restrict `twb_sheet_spec()` to one
#'   worksheet.
#' @param dashboard Optional character scalar to restrict
#'   `twb_dashboard_charts()` to one dashboard.
#'
#' @return
#' `twb_sheet_spec()` returns a named list (one element per worksheet) of
#' class `twb_sheet_spec`. Each spec is a list with:
#' \describe{
#'   \item{`sheet`}{Worksheet name.}
#'   \item{`mark_type`}{Detected mark type, e.g. `"bar"`, `"line"`, `"map"`,
#'     `"text"`. `"automatic"` when the workbook leaves it to Tableau.}
#'   \item{`mark_source`}{`"explicit"` when the workbook names the mark type,
#'     `"inferred"` for `"automatic"`.}
#'   \item{`datasources`}{Character vector of datasources referenced.}
#'   \item{`rows`, `cols`}{Character vectors of clean field names on the rows
#'     and columns shelves, in shelf order.}
#'   \item{`dimensions`, `measures`}{Character vectors of the dimension and
#'     measure fields used anywhere in the visualization. A pill counts as a
#'     measure when it is aggregated; otherwise its declared field role wins.}
#'   \item{`encodings`}{Tibble with `channel`, `field`, `aggregation`,
#'     `used_as` (`"dimension"`/`"measure"`) for every marks-card encoding.}
#'   \item{`shelves`}{Tibble with `shelf` (`"rows"`/`"cols"`), `field`,
#'     `aggregation`, `used_as` for the row/column pills, in shelf order.}
#'   \item{`tooltip`}{List with `has_tooltip`, `customized`, `text`, and
#'     `fields`.}
#'   \item{`filters`, `sorts`, `axes`}{Tibbles from [twb_sheet_filters()],
#'     [twb_sheet_sorts()], and [twb_sheet_axes()].}
#' }
#'
#' `twb_dashboard_charts()` returns a tibble with one row per worksheet placed
#' on a dashboard: `dashboard`, `sheet`, `mark_type`, `mark_source`, `rows`,
#' `cols`, `dimensions`, `measures`, `tooltip_fields` (list-columns),
#' `n_tooltip_fields`, `has_tooltip`, `n_filters`, `datasources`, and the
#' layout `zone_id`, `x`, `y`, `w`, `h`.
#'
#' @examples
#' twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
#' if (nzchar(twb) && file.exists(twb)) {
#'   parser <- TwbParser$new(twb)
#'   spec <- twb_sheet_spec(parser)
#'   spec
#'
#'   twb_dashboard_charts(parser)
#' }
#'
#' @export
twb_sheet_spec <- function(x, sheet = NULL) {
  xml_doc <- .twb_resolve_xml(x)
  if (!is.null(sheet)) {
    stopifnot(is.character(sheet), length(sheet) == 1L)
  }

  ws_nodes <- if (is.null(sheet)) {
    xml2::xml_find_all(xml_doc, ".//worksheet")
  } else {
    .twb_find_all_named(xml_doc, "worksheet", sheet)
  }
  if (length(ws_nodes) == 0L) {
    return(structure(list(), class = "twb_sheet_spec"))
  }

  fields <- tryCatch(
    extract_columns_with_table_source(xml_doc),
    error = function(e) tibble::tibble()
  )

  specs <- purrr::map(ws_nodes, function(ws) .viz_sheet_spec_one(xml_doc, ws, fields))
  names(specs) <- purrr::map_chr(specs, "sheet")
  structure(specs, class = "twb_sheet_spec")
}

#' @rdname twb_sheet_spec
#' @export
twb_dashboard_charts <- function(x, dashboard = NULL) {
  xml_doc <- .twb_resolve_xml(x)
  if (!is.null(dashboard)) {
    stopifnot(is.character(dashboard), length(dashboard) == 1L)
  }

  placed <- .ins_dashboard_sheets(xml_doc, dashboard)
  if (nrow(placed) == 0L) {
    return(.empty_dashboard_charts())
  }

  fields <- tryCatch(
    extract_columns_with_table_source(xml_doc),
    error = function(e) tibble::tibble()
  )

  sheet_names <- unique(placed$sheet[!is.na(placed$sheet)])
  summaries <- if (length(sheet_names) == 0L) {
    .empty_sheet_summary()
  } else {
    purrr::map_dfr(sheet_names, function(nm) {
      ws <- .twb_find_named(xml_doc, "worksheet", nm)
      if (inherits(ws, "xml_missing")) {
        return(.empty_sheet_summary())
      }
      .viz_sheet_summary(xml_doc, ws, fields)
    })
  }

  out <- dplyr::left_join(placed, summaries, by = "sheet")
  out <- dplyr::select(
    out, "dashboard", "sheet", "mark_type", "mark_source",
    "rows", "cols", "dimensions", "measures", "tooltip_fields",
    "n_tooltip_fields", "has_tooltip", "n_filters", "datasources",
    "zone_id", "x", "y", "w", "h"
  )
  dplyr::arrange(out, .data$dashboard, .data$y, .data$x)
}

#' @export
print.twb_sheet_spec <- function(x, ...) {
  if (!length(x)) {
    cat("twb_sheet_spec: no worksheets matched.\n")
    return(invisible(x))
  }
  for (nm in names(x)) {
    s <- x[[nm]]
    cat("Sheet:", s$sheet, "\n")
    cat("  Mark type: ", s$mark_type,
        if (s$mark_source == "inferred") " (Tableau automatic)" else "", "\n", sep = "")
    if (length(s$datasources)) {
      cat("  Datasources:", paste(s$datasources, collapse = ", "), "\n")
    }
    .viz_print_fields("  Rows", s$rows)
    .viz_print_fields("  Cols", s$cols)
    .viz_print_fields("  Dimensions", s$dimensions)
    .viz_print_fields("  Measures", s$measures)
    if (nrow(s$encodings)) {
      enc <- paste0(s$encodings$channel, " -> ", s$encodings$field)
      cat("  Encodings:", paste(enc, collapse = "; "), "\n")
    }
    tt <- s$tooltip
    cat("  Tooltips:", if (tt$has_tooltip) "yes" else "none",
        if (isTRUE(tt$customized)) " (customized)" else "",
        if (length(tt$fields)) paste0(" [", paste(tt$fields, collapse = ", "), "]") else "",
        "\n", sep = "")
    cat("  Filters:", nrow(s$filters),
        " Sorts:", nrow(s$sorts),
        " Axes:", nrow(s$axes), "\n\n")
  }
  invisible(x)
}

# ---- internals ---------------------------------------------------------------

#' Mark-type tokens Tableau stores in `<style-rule element="...">`
#' @keywords internal
#' @noRd
.viz_mark_vocabulary <- c(
  "area", "bar", "circle", "crosstab", "density", "gantt", "line",
  "map", "pie", "polygon", "shape", "square", "table", "text"
)

#' Detect a worksheet's mark type from explicit workbook signals
#' @keywords internal
#' @noRd
.viz_mark_type <- function(ws_node) {
  # 1) explicit <mark class="..."> inside pane marks (Tableau's real schema;
  #    e.g. class="Bar", class="Automatic"). Also accept a type attribute
  #    in case a third-party writer emits one.
  mtypes <- unique(tolower(c(
    xml2::xml_attr(xml2::xml_find_all(ws_node, ".//mark[@class]"), "class"),
    xml2::xml_attr(xml2::xml_find_all(ws_node, ".//mark[@type]"), "type")
  )))
  mtypes <- mtypes[!is.na(mtypes) & nzchar(mtypes)]
  # class="Automatic" means "no explicit choice" (Tableau picks the mark), so
  # it carries no signal: a style rule naming a concrete mark still wins.
  mtypes <- mtypes[mtypes != "automatic"]

  # 2) <style-rule element="..."> naming a mark kind (e.g. element="map")
  els <- tolower(xml2::xml_attr(
    xml2::xml_find_all(ws_node, "./table/style/style-rule[@element]"), "element"
  ))
  els <- unique(els[!is.na(els) & els %in% .viz_mark_vocabulary])
  # Tableau synonyms for the text mark
  els[els %in% c("crosstab", "table")] <- "text"

  found <- unique(c(mtypes, els))
  # "automatic" is only ever a lack-of-choice placeholder: if any concrete
  # mark signal was found, it wins and the placeholder is discarded.
  specific <- setdiff(found, "automatic")
  if (length(specific)) found <- specific
  if (!length(found)) {
    return(list(mark_type = "automatic", mark_source = "inferred"))
  }
  list(mark_type = paste(found, collapse = ", "), mark_source = "explicit")
}

#' Rows/cols pills in shelf order (unlike twb_sheet_shelves' alphabetical sort)
#' @keywords internal
#' @noRd
.viz_shelf_pills <- function(ws_node, shelf = c("rows", "cols")) {
  shelf <- match.arg(shelf)
  el <- xml2::xml_find_first(ws_node, paste0("./table/", shelf))
  if (inherits(el, "xml_missing")) {
    return(.empty_shelves())
  }
  nm <- xml2::xml_attr(ws_node, "name") %||% NA_character_
  out <- .parse_shelf_text_to_tibble(nm, shelf, xml2::xml_text(el))
  if (nrow(out) == 0L || !all(c("field_clean", "aggregation", "datasource") %in% names(out))) {
    return(.empty_shelves())
  }
  out
}

#' Classify each pill as dimension or measure
#'
#' An aggregated pill is used as a measure; otherwise the field's declared
#' role wins, defaulting to dimension.
#' @keywords internal
#' @noRd
.viz_add_used_as <- function(pills, fields) {
  if (nrow(pills) == 0L) {
    pills$used_as <- character()
    return(pills)
  }
  role <- rep(NA_character_, nrow(pills))
  if (NROW(fields) > 0L && all(c("field_clean", "role") %in% names(fields))) {
    lu <- fields[!is.na(fields$field_clean) & !is.na(fields$role),
                 c("field_clean", "role"), drop = FALSE]
    lu <- lu[!duplicated(lu$field_clean), , drop = FALSE]
    role <- tolower(lu$role[match(pills$field_clean, lu$field_clean)])
  }
  agg <- !is.na(pills$aggregation) & nzchar(pills$aggregation)
  pills$used_as <- ifelse(agg | (!is.na(role) & role == "measure"),
                          "measure", "dimension")
  pills
}

#' One-row summary tibble shared by twb_sheet_spec() and twb_dashboard_charts()
#' @keywords internal
#' @noRd
.viz_sheet_summary <- function(xml_doc, ws_node, fields) {
  nm <- xml2::xml_attr(ws_node, "name") %||% NA_character_
  mt <- .viz_mark_type(ws_node)

  rows <- .viz_add_used_as(.viz_shelf_pills(ws_node, "rows"), fields)
  cols <- .viz_add_used_as(.viz_shelf_pills(ws_node, "cols"), fields)

  enc <- .ins_sheet_shelves(xml_doc, nm)
  enc <- enc[!enc$shelf %in% c("rows", "cols"), , drop = FALSE]
  enc <- .viz_add_used_as(enc, fields)

  pills <- dplyr::bind_rows(rows, cols, enc)
  dims <- unique(pills$field_clean[pills$used_as == "dimension" &
                                     !is.na(pills$field_clean)])
  meas <- unique(pills$field_clean[pills$used_as == "measure" &
                                     !is.na(pills$field_clean)])
  tt_fields <- unique(enc$field_clean[enc$shelf == "tooltip" &
                                        !is.na(enc$field_clean)])
  tt <- .ins_tooltips(xml_doc, nm)
  has_tt <- length(tt_fields) > 0L || (nrow(tt) > 0L && any(tt$is_customized, na.rm = TRUE))
  n_filt <- nrow(.ins_sheet_filters(xml_doc, nm))

  tibble::tibble(
    sheet            = nm,
    mark_type        = mt$mark_type,
    mark_source      = mt$mark_source,
    rows             = list(rows$field_clean),
    cols             = list(cols$field_clean),
    dimensions       = list(dims),
    measures         = list(meas),
    tooltip_fields   = list(tt_fields),
    n_tooltip_fields = length(tt_fields),
    has_tooltip      = has_tt,
    n_filters        = n_filt,
    datasources      = list(unique(pills$datasource[!is.na(pills$datasource)]))
  )
}

#' Assemble the full rebuild spec for one worksheet node
#' @keywords internal
#' @noRd
.viz_sheet_spec_one <- function(xml_doc, ws_node, fields) {
  nm <- xml2::xml_attr(ws_node, "name") %||% NA_character_
  s <- .viz_sheet_summary(xml_doc, ws_node, fields)

  rows <- .viz_add_used_as(.viz_shelf_pills(ws_node, "rows"), fields)
  cols <- .viz_add_used_as(.viz_shelf_pills(ws_node, "cols"), fields)
  enc <- .ins_sheet_shelves(xml_doc, nm)
  enc <- enc[!enc$shelf %in% c("rows", "cols"), , drop = FALSE]
  enc <- .viz_add_used_as(enc, fields)

  shelves_tbl <- dplyr::bind_rows(rows, cols)
  shelves_tbl <- tibble::tibble(
    shelf       = shelves_tbl$shelf,
    field       = shelves_tbl$field_clean,
    aggregation = shelves_tbl$aggregation,
    used_as     = shelves_tbl$used_as
  )
  enc_tbl <- tibble::tibble(
    channel     = enc$shelf,
    field       = enc$field_clean,
    aggregation = enc$aggregation,
    used_as     = enc$used_as
  )

  tt <- .ins_tooltips(xml_doc, nm)
  tooltip <- list(
    has_tooltip = s$has_tooltip[[1L]],
    customized  = nrow(tt) > 0L && any(tt$is_customized, na.rm = TRUE),
    text        = if (nrow(tt) > 0L) tt$tooltip_text[[1L]] else NA_character_,
    fields      = s$tooltip_fields[[1L]]
  )

  list(
    sheet       = nm,
    mark_type   = s$mark_type[[1L]],
    mark_source = s$mark_source[[1L]],
    datasources = s$datasources[[1L]],
    rows        = s$rows[[1L]],
    cols        = s$cols[[1L]],
    dimensions  = s$dimensions[[1L]],
    measures    = s$measures[[1L]],
    encodings   = enc_tbl,
    shelves     = shelves_tbl,
    tooltip     = tooltip,
    filters     = .ins_sheet_filters(xml_doc, nm),
    sorts       = .ins_sheet_sorts(xml_doc, nm),
    axes        = .ins_sheet_axes(xml_doc, nm)
  )
}

#' @keywords internal
#' @noRd
.empty_sheet_summary <- function() {
  tibble::tibble(
    sheet = character(), mark_type = character(), mark_source = character(),
    rows = list(), cols = list(), dimensions = list(), measures = list(),
    tooltip_fields = list(), n_tooltip_fields = integer(),
    has_tooltip = logical(), n_filters = integer(), datasources = list()
  )
}

#' @keywords internal
#' @noRd
.empty_dashboard_charts <- function() {
  tibble::tibble(
    dashboard = character(), sheet = character(),
    mark_type = character(), mark_source = character(),
    rows = list(), cols = list(), dimensions = list(), measures = list(),
    tooltip_fields = list(), n_tooltip_fields = integer(),
    has_tooltip = logical(), n_filters = integer(), datasources = list(),
    zone_id = character(), x = integer(), y = integer(),
    w = integer(), h = integer()
  )
}

#' @keywords internal
#' @noRd
.viz_print_fields <- function(label, fields) {
  if (!length(fields) || all(is.na(fields))) {
    cat(label, ": (none)\n", sep = "")
  } else {
    cat(label, " (", length(fields), "): ",
        paste(fields, collapse = ", "), "\n", sep = "")
  }
}
