#' @importFrom xml2 xml_find_all xml_find_first xml_attr xml_text xml_name xml_parent
#' @importFrom tibble tibble
#' @importFrom purrr map_dfr
#' @importFrom dplyr arrange bind_rows distinct
#' @importFrom rlang .data
NULL

# ---- twb_dashboard_size ------------------------------------------------------

#' Dashboard page size and sizing mode
#'
#' Reads the `<size>` element of each dashboard, which records how the page is
#' sized: a fixed pixel size, an automatic (fit-to-window) size, or a range.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param dashboard Optional character scalar to restrict output to one
#'   dashboard.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{dashboard}{Dashboard name.}
#'   \item{sizing_mode}{`"fixed"`, `"automatic"`, `"range"`, or another mode
#'     string; `NA` if no `<size>` element is present.}
#'   \item{min_width}{Minimum width in pixels, or `NA`.}
#'   \item{min_height}{Minimum height in pixels, or `NA`.}
#'   \item{max_width}{Maximum (or fixed) width in pixels, or `NA`.}
#'   \item{max_height}{Maximum (or fixed) height in pixels, or `NA`.}
#' }
#'
#' @examples
#' xml <- xml2::read_xml(
#'   '<workbook><dashboards>
#'     <dashboard name="Overview">
#'       <size sizing-mode="fixed" maxwidth="1200" maxheight="800"
#'             minwidth="1200" minheight="800"/>
#'     </dashboard>
#'   </dashboards></workbook>'
#' )
#' twb_dashboard_size(xml)
#'
#' @export
twb_dashboard_size <- function(x, dashboard = NULL) {
  xml_doc <- .twb_resolve_xml(x)
  if (!is.null(dashboard)) {
    stopifnot(is.character(dashboard), length(dashboard) == 1L)
  }
  .ins_dashboard_size(xml_doc, dashboard)
}

#' @keywords internal
#' @noRd
.ins_dashboard_size <- function(xml_doc, dashboard = NULL) {
  d_xpath <- if (is.null(dashboard)) {
    ".//dashboard"
  } else {
    paste0(".//dashboard[@name='", gsub("'", "", dashboard, fixed = TRUE), "']")
  }
  d_nodes <- xml2::xml_find_all(xml_doc, d_xpath)
  if (length(d_nodes) == 0L) return(.empty_dashboard_size())

  int_attr <- function(node, a) {
    v <- suppressWarnings(as.integer(xml2::xml_attr(node, a)))
    if (length(v)) v else NA_integer_
  }

  out <- purrr::map_dfr(d_nodes, function(d) {
    d_name <- xml2::xml_attr(d, "name") %||% NA_character_
    sz <- xml2::xml_find_first(d, "./size")
    if (inherits(sz, "xml_missing")) sz <- xml2::xml_find_first(d, ".//size")
    if (inherits(sz, "xml_missing")) {
      return(tibble::tibble(
        dashboard = d_name, sizing_mode = NA_character_,
        min_width = NA_integer_, min_height = NA_integer_,
        max_width = NA_integer_, max_height = NA_integer_
      ))
    }
    tibble::tibble(
      dashboard   = d_name,
      sizing_mode = xml2::xml_attr(sz, "sizing-mode") %||% NA_character_,
      min_width   = int_attr(sz, "minwidth"),
      min_height  = int_attr(sz, "minheight"),
      max_width   = int_attr(sz, "maxwidth"),
      max_height  = int_attr(sz, "maxheight")
    )
  })
  dplyr::arrange(out, .data$dashboard)
}

.empty_dashboard_size <- function() {
  tibble::tibble(
    dashboard = character(), sizing_mode = character(),
    min_width = integer(), min_height = integer(),
    max_width = integer(), max_height = integer()
  )
}

# ---- twb_formatting ----------------------------------------------------------

#' Formatting rules (fonts, colours, number formats, …)
#'
#' Walks every `<format>` entry inside a `<style-rule>` and returns a tidy
#' table of formatting attributes. This is where fonts, colours, number/date
#' formats, alignment, and shading live in a Tableau workbook. Filter the
#' `attr` column for what you need (e.g. `attr == "number-format"`, or
#' `startsWith(attr, "font")`).
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param scope Optional character scalar (`"worksheet"`, `"dashboard"`, or
#'   `"workbook"`) to restrict output.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{scope}{`"worksheet"`, `"dashboard"`, or `"workbook"`.}
#'   \item{container}{Worksheet or dashboard name (`NA` for workbook scope).}
#'   \item{element}{The styled element, e.g. `"mark"`, `"axis"`, `"pane"`,
#'     `"cell"`, `"title"`.}
#'   \item{field}{Field the rule targets, if any; `NA` otherwise.}
#'   \item{target}{Palette bucket or other target value, if any; `NA`
#'     otherwise.}
#'   \item{attr}{Formatting attribute, e.g. `"font-family"`, `"font-size"`,
#'     `"color"`, `"number-format"`, `"encoding-color"`.}
#'   \item{value}{Attribute value.}
#' }
#'
#' @examples
#' xml <- xml2::read_xml(
#'   '<workbook><worksheets><worksheet name="Sales"><table><style>
#'     <style-rule element="mark">
#'       <format attr="mark-labels-cull" value="true"/>
#'       <format attr="number-format" value="$#,##0"/>
#'     </style-rule>
#'   </style></table></worksheet></worksheets></workbook>'
#' )
#' twb_formatting(xml)
#'
#' @export
twb_formatting <- function(x, scope = NULL) {
  xml_doc <- .twb_resolve_xml(x)
  if (!is.null(scope)) {
    stopifnot(is.character(scope), length(scope) == 1L)
  }
  .ins_formatting(xml_doc, scope)
}

#' @keywords internal
#' @noRd
.ins_formatting <- function(xml_doc, scope = NULL) {
  fmt_nodes <- xml2::xml_find_all(xml_doc, ".//style-rule/format")
  enc_nodes <- xml2::xml_find_all(xml_doc, ".//style-rule/encoding")
  if (length(fmt_nodes) == 0L && length(enc_nodes) == 0L) {
    return(.empty_formatting())
  }

  first_attr <- function(..., default = NA_character_) {
    vals <- vapply(list(...), function(x) {
      if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(x)) {
        NA_character_
      } else {
        as.character(x)
      }
    }, character(1))
    vals <- vals[!is.na(vals)]
    if (length(vals)) vals[[1L]] else default
  }

  context_for <- function(node) {
    rule <- xml2::xml_parent(node)
    element <- xml2::xml_attr(rule, "element") %||% NA_character_

    anc_ws <- xml2::xml_find_first(node, "ancestor::worksheet[1]")
    anc_db <- xml2::xml_find_first(node, "ancestor::dashboard[1]")
    if (!inherits(anc_ws, "xml_missing")) {
      scp  <- "worksheet"
      cont <- xml2::xml_attr(anc_ws, "name") %||% NA_character_
    } else if (!inherits(anc_db, "xml_missing")) {
      scp  <- "dashboard"
      cont <- xml2::xml_attr(anc_db, "name") %||% NA_character_
    } else {
      scp  <- "workbook"
      cont <- NA_character_
    }
    list(rule = rule, scope = scp, container = cont, element = element)
  }

  formats <- purrr::map_dfr(fmt_nodes, function(fn) {
    ctx <- context_for(fn)
    tibble::tibble(
      scope     = ctx$scope,
      container = ctx$container,
      element   = ctx$element,
      field     = first_attr(xml2::xml_attr(fn, "field"), xml2::xml_attr(ctx$rule, "field")),
      target    = NA_character_,
      attr      = first_attr(xml2::xml_attr(fn, "attr")),
      value     = first_attr(xml2::xml_attr(fn, "value"))
    )
  })

  encodings <- purrr::map_dfr(enc_nodes, function(en) {
    ctx <- context_for(en)
    enc_attr <- first_attr(xml2::xml_attr(en, "attr"), xml2::xml_name(en), default = "encoding")
    field <- first_attr(xml2::xml_attr(en, "field"))
    maps <- xml2::xml_find_all(en, "./map")

    if (length(maps)) {
      return(purrr::map_dfr(maps, function(mp) {
        bucket <- xml2::xml_find_first(mp, "./bucket")
        target <- if (inherits(bucket, "xml_missing")) {
          first_attr(xml2::xml_attr(mp, "bucket"))
        } else {
          trimws(xml2::xml_text(bucket))
        }
        tibble::tibble(
          scope     = ctx$scope,
          container = ctx$container,
          element   = ctx$element,
          field     = field,
          target    = target,
          attr      = paste0("encoding-", enc_attr),
          value     = first_attr(xml2::xml_attr(mp, "to"), xml2::xml_attr(mp, "color"))
        )
      }))
    }

    tibble::tibble(
      scope     = ctx$scope,
      container = ctx$container,
      element   = ctx$element,
      field     = field,
      target    = NA_character_,
      attr      = paste0("encoding-", enc_attr),
      value     = first_attr(xml2::xml_attr(en, "type"), xml2::xml_attr(en, "palette"))
    )
  })

  out <- dplyr::bind_rows(formats, encodings)
  out <- dplyr::distinct(out)
  if (!is.null(scope)) out <- out[out$scope == scope, , drop = FALSE]
  dplyr::arrange(out, .data$scope, .data$container, .data$element, .data$attr, .data$target)
}

.empty_formatting <- function() {
  tibble::tibble(
    scope = character(), container = character(), element = character(),
    field = character(), target = character(), attr = character(), value = character()
  )
}

# ---- twb_tooltips ------------------------------------------------------------

#' Worksheet tooltips
#'
#' Extracts the (plain-text) tooltip configured for each worksheet. Tableau
#' stores customised tooltips as formatted-text runs; this returns the
#' concatenated text with markup stripped.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param sheet Optional character scalar to restrict output to one worksheet.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{sheet}{Worksheet name.}
#'   \item{is_customized}{`TRUE` if a non-empty customised tooltip is present.}
#'   \item{tooltip_text}{Plain-text tooltip content, or `NA`.}
#' }
#'
#' @examples
#' xml <- xml2::read_xml(
#'   '<workbook><worksheets><worksheet name="Sales">
#'     <customized-tooltip><formatted-text>
#'       <run>Sales: </run><run>&lt;SUM(Sales)&gt;</run>
#'     </formatted-text></customized-tooltip>
#'   </worksheet></worksheets></workbook>'
#' )
#' twb_tooltips(xml)
#'
#' @export
twb_tooltips <- function(x, sheet = NULL) {
  xml_doc <- .twb_resolve_xml(x)
  if (!is.null(sheet)) {
    stopifnot(is.character(sheet), length(sheet) == 1L)
  }
  .ins_tooltips(xml_doc, sheet)
}

#' @keywords internal
#' @noRd
.ins_tooltips <- function(xml_doc, sheet = NULL) {
  ws_xpath <- if (is.null(sheet)) {
    ".//worksheet"
  } else {
    paste0(".//worksheet[@name='", gsub("'", "", sheet, fixed = TRUE), "']")
  }
  ws_nodes <- xml2::xml_find_all(xml_doc, ws_xpath)
  if (length(ws_nodes) == 0L) return(.empty_tooltips())

  out <- purrr::map_dfr(ws_nodes, function(w) {
    nm <- xml2::xml_attr(w, "name") %||% NA_character_
    tt <- xml2::xml_find_first(w, ".//customized-tooltip | .//tooltip")
    if (inherits(tt, "xml_missing")) {
      return(tibble::tibble(sheet = nm, is_customized = FALSE, tooltip_text = NA_character_))
    }
    txt <- gsub("\\s+", " ", trimws(xml2::xml_text(tt) %||% ""))
    tibble::tibble(
      sheet         = nm,
      is_customized = nzchar(txt),
      tooltip_text  = if (nzchar(txt)) txt else NA_character_
    )
  })
  dplyr::arrange(out, .data$sheet)
}

.empty_tooltips <- function() {
  tibble::tibble(
    sheet = character(), is_customized = logical(), tooltip_text = character()
  )
}
