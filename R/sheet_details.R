#' @importFrom xml2 xml_find_all xml_find_first xml_attr xml_text xml_parent xml_name xml_children
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows filter arrange mutate select distinct
#' @importFrom purrr map_dfr
NULL

# ---- Internal helpers -------------------------------------------------------

# Parse a Tableau column-reference attribute, e.g.
#   "[federated.abc].[none:Category:nk]"
#   "SUM([federated.abc].[Sales])"
# Returns a named list: datasource, field_instance, field_clean, aggregation
.parse_col_ref <- function(ref) {
  empty <- list(
    datasource     = NA_character_,
    field_instance = NA_character_,
    field_clean    = NA_character_,
    aggregation    = NA_character_
  )
  if (is.null(ref) || is.na(ref) || !nzchar(ref)) return(empty)

  # 1) strip outer aggregation wrapper: SUM(...) ATTR(...) etc.
  agg <- NA_character_
  m <- regexpr("^([A-Z_]+)\\((.+)\\)$", ref, perl = TRUE)
  if (m > 0L) {
    starts <- attr(m, "capture.start")
    lens   <- attr(m, "capture.length")
    agg <- substr(ref, starts[1], starts[1] + lens[1] - 1L)
    ref <- substr(ref, starts[2], starts[2] + lens[2] - 1L)
  }

  # 2) extract bracketed tokens
  toks <- regmatches(ref, gregexpr("\\[[^\\]]+\\]", ref, perl = TRUE))[[1L]]
  toks <- gsub("^\\[|\\]$", "", toks)

  ds <- if (length(toks) >= 1L) toks[[1L]] else NA_character_
  fi <- if (length(toks) >= 2L) toks[[2L]] else
        if (length(toks) == 1L) toks[[1L]] else NA_character_

  # 3) clean field instance: "none:Category:nk" -> "Category"
  fc <- fi
  if (!is.na(fc)) {
    fc <- sub("^[a-z]+:", "", fc)   # strip derivation prefix "none:", "clct:"
    fc <- sub(":[a-z]+$", "", fc)   # strip pivot suffix ":nk", ":qk"
    fc <- .twb_clean_table(fc)      # strip remaining brackets / hex suffix
  }

  list(datasource = ds, field_instance = fi, field_clean = fc, aggregation = agg)
}

# ---- twb_sheet_shelves -------------------------------------------------------

#' Extract field-to-shelf assignments for worksheets
#'
#' Returns a tidy tibble describing which fields are placed on each visual
#' shelf (rows, cols, color, size, label, detail, tooltip, etc.) for every
#' worksheet in the workbook (or a single named sheet).
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param sheet Optional character scalar. When supplied only that worksheet
#'   is returned; otherwise all worksheets are returned.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{sheet}{Worksheet name.}
#'   \item{shelf}{Shelf name: `"rows"`, `"cols"`, or an encoding type such as
#'     `"color"`, `"size"`, `"label"`, `"detail"`, `"tooltip"`, `"shape"`,
#'     `"text"`, `"path"`, `"angle"`, `"lod"`, `"geometry"`, etc.}
#'   \item{field_ref}{Raw column-reference attribute value.}
#'   \item{field_instance}{Field instance name (after stripping datasource prefix).}
#'   \item{field_clean}{Human-readable field name.}
#'   \item{datasource}{Datasource name referenced.}
#'   \item{aggregation}{Aggregation function (`"SUM"`, `"AVG"`, …) or `NA`.}
#' }
#'
#' @examples
#' xml <- xml2::read_xml(
#'   '<workbook>
#'     <worksheets>
#'       <worksheet name="Sales">
#'         <table>
#'           <rows>[ds].[Category]</rows>
#'           <cols>[ds].[Sales]</cols>
#'           <panes>
#'             <pane>
#'               <mark class="Bar"/>
#'               <encodings>
#'                 <color column="[ds].[Category]"/>
#'               </encodings>
#'             </pane>
#'           </panes>
#'         </table>
#'       </worksheet>
#'     </worksheets>
#'   </workbook>'
#' )
#' twb_sheet_shelves(xml)
#'
#' @export
twb_sheet_shelves <- function(x, sheet = NULL) {
  xml_doc <- .twb_resolve_xml(x)
  if (!is.null(sheet)) {
    stopifnot(is.character(sheet), length(sheet) == 1L)
  }
  .ins_sheet_shelves(xml_doc, sheet)
}

#' @keywords internal
#' @noRd
.ins_sheet_shelves <- function(xml_doc, sheet = NULL) {
  ws_xpath <- if (is.null(sheet)) {
    ".//worksheet"
  } else {
    paste0(".//worksheet[@name='", gsub("'", "", sheet, fixed = TRUE), "']")
  }
  ws_nodes <- xml2::xml_find_all(xml_doc, ws_xpath)

  if (length(ws_nodes) == 0L) {
    return(.empty_shelves())
  }

  out <- purrr::map_dfr(ws_nodes, function(ws) {
    ws_name <- xml2::xml_attr(ws, "name") %||% NA_character_

    # rows shelf
    rows_el   <- xml2::xml_find_first(ws, "./table/rows")
    rows_text <- if (!inherits(rows_el, "xml_missing")) xml2::xml_text(rows_el) else NA_character_

    # cols shelf
    cols_el   <- xml2::xml_find_first(ws, "./table/cols")
    cols_text <- if (!inherits(cols_el, "xml_missing")) xml2::xml_text(cols_el) else NA_character_

    shelf_rows <- .parse_shelf_text_to_tibble(ws_name, "rows", rows_text)
    shelf_cols <- .parse_shelf_text_to_tibble(ws_name, "cols", cols_text)

    # encoding shelves (color, size, label, detail, tooltip, ...)
    enc_nodes <- xml2::xml_find_all(ws, "./table/panes/pane/encodings/*")
    shelf_enc <- if (length(enc_nodes)) {
      purrr::map_dfr(enc_nodes, function(en) {
        shelf_nm <- xml2::xml_name(en)
        col_ref  <- xml2::xml_attr(en, "column") %||% NA_character_
        if (is.na(col_ref)) return(tibble::tibble())
        parsed <- .parse_col_ref(col_ref)
        tibble::tibble(
          sheet          = ws_name,
          shelf          = shelf_nm,
          field_ref      = col_ref,
          field_instance = parsed$field_instance,
          field_clean    = parsed$field_clean,
          datasource     = parsed$datasource,
          aggregation    = parsed$aggregation
        )
      })
    } else {
      tibble::tibble()
    }

    dplyr::bind_rows(shelf_rows, shelf_cols, shelf_enc)
  })
  if (nrow(out) == 0L) return(.empty_shelves())
  dplyr::arrange(dplyr::distinct(out), .data$sheet, .data$shelf, .data$field_clean)
}

# Parse "[ds].[f1], [ds].[f2]" shelf text into a shelf tibble
.parse_shelf_text_to_tibble <- function(ws_name, shelf_name, text) {
  if (is.null(text) || is.na(text) || !nzchar(trimws(text))) {
    return(tibble::tibble())
  }
  # Extract every [something].[something] pair (handles AGG wrappers)
  refs <- regmatches(
    text,
    gregexpr("[A-Z_]*\\[[^\\]]+\\]\\.\\[[^\\]]+\\]", text, perl = TRUE)
  )[[1L]]
  if (!length(refs)) return(tibble::tibble())

  purrr::map_dfr(refs, function(ref) {
    parsed <- .parse_col_ref(ref)
    tibble::tibble(
      sheet          = ws_name,
      shelf          = shelf_name,
      field_ref      = ref,
      field_instance = parsed$field_instance,
      field_clean    = parsed$field_clean,
      datasource     = parsed$datasource,
      aggregation    = parsed$aggregation
    )
  })
}

.empty_shelves <- function() {
  tibble::tibble(
    sheet          = character(),
    shelf          = character(),
    field_ref      = character(),
    field_instance = character(),
    field_clean    = character(),
    datasource     = character(),
    aggregation    = character()
  )
}

# ---- twb_sheet_filters -------------------------------------------------------

#' Extract detailed filter configuration for worksheets
#'
#' Returns one row per filter per worksheet, with full details on filter class,
#' inclusion mode, categorical members, and numeric or date range bounds.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param sheet Optional character scalar to restrict output to one worksheet.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{sheet}{Worksheet name.}
#'   \item{field_ref}{Raw column-reference attribute value.}
#'   \item{field_clean}{Human-readable field name.}
#'   \item{datasource}{Datasource name.}
#'   \item{filter_class}{Tableau filter class: `"categorical"`, `"range"`,
#'     `"relative-date"`, `"date"`, `"set"`, `"top"`, etc.}
#'   \item{include_mode}{`"include"` or `"exclude"`.}
#'   \item{members}{Comma-separated member values for categorical filters; `NA` otherwise.}
#'   \item{range_min}{Lower bound for range/quantitative filters; `NA` otherwise.}
#'   \item{range_max}{Upper bound; `NA` otherwise.}
#' }
#'
#' @examples
#' xml <- xml2::read_xml(
#'   '<workbook>
#'     <worksheets>
#'       <worksheet name="Sheet1">
#'         <table>
#'           <view>
#'             <filter class="categorical" column="[ds].[Category]">
#'               <groupfilter function="union">
#'                 <groupfilter function="member" member="[Furniture]"/>
#'                 <groupfilter function="member" member="[Technology]"/>
#'               </groupfilter>
#'             </filter>
#'           </view>
#'         </table>
#'       </worksheet>
#'     </worksheets>
#'   </workbook>'
#' )
#' twb_sheet_filters(xml)
#'
#' @export
twb_sheet_filters <- function(x, sheet = NULL) {
  xml_doc <- .twb_resolve_xml(x)
  if (!is.null(sheet)) {
    stopifnot(is.character(sheet), length(sheet) == 1L)
  }
  .ins_sheet_filters(xml_doc, sheet)
}

#' @keywords internal
#' @noRd
.ins_sheet_filters <- function(xml_doc, sheet = NULL) {
  ws_xpath <- if (is.null(sheet)) {
    ".//worksheet"
  } else {
    paste0(".//worksheet[@name='", gsub("'", "", sheet, fixed = TRUE), "']")
  }
  ws_nodes <- xml2::xml_find_all(xml_doc, ws_xpath)

  if (length(ws_nodes) == 0L) {
    return(.empty_filters())
  }

  out <- purrr::map_dfr(ws_nodes, function(ws) {
    ws_name  <- xml2::xml_attr(ws, "name") %||% NA_character_
    flt_nodes <- xml2::xml_find_all(ws, ".//filter[@column]")
    if (length(flt_nodes) == 0L) return(tibble::tibble())

    purrr::map_dfr(flt_nodes, function(fn) {
      col_ref <- xml2::xml_attr(fn, "column") %||% NA_character_
      parsed  <- .parse_col_ref(col_ref)
      cls     <- xml2::xml_attr(fn, "class") %||% NA_character_

      # include / exclude
      excl <- xml2::xml_attr(fn, "exclude") %||% "false"
      include_mode <- if (identical(excl, "true")) "exclude" else "include"

      # categorical members
      members <- NA_character_
      if (!is.na(cls) && cls %in% c("categorical", "set")) {
        mem_nodes <- xml2::xml_find_all(fn, ".//groupfilter[@function='member']")
        if (length(mem_nodes)) {
          raw_vals <- xml2::xml_attr(mem_nodes, "member") %||% character()
          raw_vals <- gsub("^\\[|\\]$", "", raw_vals[!is.na(raw_vals)])
          if (length(raw_vals)) members <- paste(raw_vals, collapse = ", ")
        }
      }

      # range min / max
      range_min <- NA_character_
      range_max <- NA_character_
      min_nd <- xml2::xml_find_first(fn, "./min")
      max_nd <- xml2::xml_find_first(fn, "./max")
      if (!inherits(min_nd, "xml_missing")) range_min <- xml2::xml_text(min_nd)
      if (!inherits(max_nd, "xml_missing")) range_max <- xml2::xml_text(max_nd)

      tibble::tibble(
        sheet        = ws_name,
        field_ref    = col_ref,
        field_clean  = parsed$field_clean,
        datasource   = parsed$datasource,
        filter_class = cls,
        include_mode = include_mode,
        members      = members,
        range_min    = range_min,
        range_max    = range_max
      )
    })
  })
  if (nrow(out) == 0L) return(.empty_filters())
  dplyr::arrange(dplyr::distinct(out), .data$sheet, .data$field_clean)
}

.empty_filters <- function() {
  tibble::tibble(
    sheet        = character(),
    field_ref    = character(),
    field_clean  = character(),
    datasource   = character(),
    filter_class = character(),
    include_mode = character(),
    members      = character(),
    range_min    = character(),
    range_max    = character()
  )
}

# ---- twb_sheet_axes ----------------------------------------------------------

#' Extract axis configuration for worksheets
#'
#' Reads per-axis style rules embedded in worksheet XML and returns one row per
#' axis per worksheet.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param sheet Optional character scalar to restrict output to one worksheet.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{sheet}{Worksheet name.}
#'   \item{axis}{Axis identifier (e.g., `"rows"`, `"cols"`, or `"automatic"`).}
#'   \item{field_ref}{Column reference if axis is field-specific; `NA` otherwise.}
#'   \item{field_clean}{Human-readable field name; `NA` if not field-specific.}
#'   \item{scale_type}{Scale type (`"linear"`, `"log"`, …) if present; `NA` otherwise.}
#'   \item{reversed}{Logical: `TRUE` if axis is reversed.}
#'   \item{include_zero}{Logical: `TRUE` if zero is pinned on axis.}
#' }
#'
#' @examples
#' xml <- xml2::read_xml(
#'   '<workbook>
#'     <worksheets>
#'       <worksheet name="Sheet1">
#'         <table>
#'           <style>
#'             <style-rule element="axis">
#'               <format attr="reverse" value="false"/>
#'               <format attr="scale-include-zero" value="true"/>
#'             </style-rule>
#'           </style>
#'         </table>
#'       </worksheet>
#'     </worksheets>
#'   </workbook>'
#' )
#' twb_sheet_axes(xml)
#'
#' @export
twb_sheet_axes <- function(x, sheet = NULL) {
  xml_doc <- .twb_resolve_xml(x)
  if (!is.null(sheet)) {
    stopifnot(is.character(sheet), length(sheet) == 1L)
  }
  .ins_sheet_axes(xml_doc, sheet)
}

#' @keywords internal
#' @noRd
.ins_sheet_axes <- function(xml_doc, sheet = NULL) {
  ws_xpath <- if (is.null(sheet)) {
    ".//worksheet"
  } else {
    paste0(".//worksheet[@name='", gsub("'", "", sheet, fixed = TRUE), "']")
  }
  ws_nodes <- xml2::xml_find_all(xml_doc, ws_xpath)
  if (length(ws_nodes) == 0L) return(.empty_axes())

  out <- purrr::map_dfr(ws_nodes, function(ws) {
    ws_name <- xml2::xml_attr(ws, "name") %||% NA_character_

    # Workbook-style-rule approach: <style-rule element="axis">
    axis_rules <- xml2::xml_find_all(ws, ".//style-rule[@element='axis']")
    rule_rows <- if (length(axis_rules)) {
      purrr::map_dfr(axis_rules, function(rule) {
        fmt_nodes  <- xml2::xml_find_all(rule, "./format")
        fmt_attrs  <- xml2::xml_attr(fmt_nodes, "attr")
        fmt_vals   <- xml2::xml_attr(fmt_nodes, "value")
        get_fmt <- function(a) {
          i <- match(a, fmt_attrs)
          if (!is.na(i)) fmt_vals[[i]] else NA_character_
        }
        reversed     <- isTRUE(get_fmt("reverse")           %in% c("true", "1"))
        include_zero <- isTRUE(get_fmt("scale-include-zero") %in% c("true", "1"))
        scale_type   <- get_fmt("scale-type")

        tibble::tibble(
          sheet        = ws_name,
          axis         = "automatic",
          field_ref    = NA_character_,
          field_clean  = NA_character_,
          scale_type   = scale_type,
          reversed     = reversed,
          include_zero = include_zero
        )
      })
    } else {
      tibble::tibble()
    }

    # Per-column axis styles: <axis-styles><axis-style column="...">
    col_axes <- xml2::xml_find_all(ws, ".//axis-styles/axis-style[@column]")
    col_rows <- if (length(col_axes)) {
      purrr::map_dfr(col_axes, function(ax) {
        col_ref <- xml2::xml_attr(ax, "column") %||% NA_character_
        parsed  <- .parse_col_ref(col_ref)
        scale_nd <- xml2::xml_find_first(ax, "./scale")
        reversed <- FALSE
        include_zero <- FALSE
        scale_type <- NA_character_
        if (!inherits(scale_nd, "xml_missing")) {
          reversed     <- isTRUE(xml2::xml_attr(scale_nd, "reversed")       %in% c("true", "1"))
          include_zero <- isTRUE(xml2::xml_attr(scale_nd, "include-origin") %in% c("true", "1"))
          scale_type   <- xml2::xml_attr(scale_nd, "type") %||% NA_character_
        }
        tibble::tibble(
          sheet        = ws_name,
          axis         = xml2::xml_attr(ax, "axis") %||% NA_character_,
          field_ref    = col_ref,
          field_clean  = parsed$field_clean,
          scale_type   = scale_type,
          reversed     = reversed,
          include_zero = include_zero
        )
      })
    } else {
      tibble::tibble()
    }

    dplyr::bind_rows(rule_rows, col_rows)
  })
  if (nrow(out) == 0L) return(.empty_axes())
  dplyr::arrange(dplyr::distinct(out), .data$sheet, .data$axis)
}

.empty_axes <- function() {
  tibble::tibble(
    sheet        = character(),
    axis         = character(),
    field_ref    = character(),
    field_clean  = character(),
    scale_type   = character(),
    reversed     = logical(),
    include_zero = logical()
  )
}

# ---- twb_sheet_sorts ---------------------------------------------------------

#' Extract sort configuration for worksheets
#'
#' Returns one row per sort directive per worksheet.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param sheet Optional character scalar to restrict output to one worksheet.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{sheet}{Worksheet name.}
#'   \item{field_ref}{Raw column-reference attribute.}
#'   \item{field_clean}{Human-readable field name.}
#'   \item{datasource}{Datasource name.}
#'   \item{sort_order}{`"ascending"` or `"descending"`.}
#'   \item{sort_by}{Sort method: `"field"`, `"alphabetic"`, `"manual"`, `"data-source-order"`, etc.}
#' }
#'
#' @examples
#' xml <- xml2::read_xml(
#'   '<workbook>
#'     <worksheets>
#'       <worksheet name="Sheet1">
#'         <table>
#'           <view>
#'             <sort class="sum" column="[ds].[Sales]" direction="descending"/>
#'           </view>
#'         </table>
#'       </worksheet>
#'     </worksheets>
#'   </workbook>'
#' )
#' twb_sheet_sorts(xml)
#'
#' @export
twb_sheet_sorts <- function(x, sheet = NULL) {
  xml_doc <- .twb_resolve_xml(x)
  if (!is.null(sheet)) {
    stopifnot(is.character(sheet), length(sheet) == 1L)
  }
  .ins_sheet_sorts(xml_doc, sheet)
}

#' @keywords internal
#' @noRd
.ins_sheet_sorts <- function(xml_doc, sheet = NULL) {
  ws_xpath <- if (is.null(sheet)) {
    ".//worksheet"
  } else {
    paste0(".//worksheet[@name='", gsub("'", "", sheet, fixed = TRUE), "']")
  }
  ws_nodes <- xml2::xml_find_all(xml_doc, ws_xpath)
  if (length(ws_nodes) == 0L) return(.empty_sorts())

  out <- purrr::map_dfr(ws_nodes, function(ws) {
    ws_name <- xml2::xml_attr(ws, "name") %||% NA_character_
    sort_nodes <- xml2::xml_find_all(ws, ".//sort[@column]")
    if (length(sort_nodes) == 0L) return(tibble::tibble())

    purrr::map_dfr(sort_nodes, function(sn) {
      col_ref <- xml2::xml_attr(sn, "column") %||% NA_character_
      parsed  <- .parse_col_ref(col_ref)
      dir     <- xml2::xml_attr(sn, "direction") %||% NA_character_
      cls     <- xml2::xml_attr(sn, "class")     %||% NA_character_

      sort_by <- dplyr::case_when(
        !is.na(cls) & cls %in% c("sum", "avg", "min", "max", "count") ~ "field",
        !is.na(cls) & cls == "alphabetic"                              ~ "alphabetic",
        !is.na(cls) & cls == "explicit"                                ~ "manual",
        !is.na(cls) & cls == "data-source-order"                       ~ "data-source-order",
        !is.na(cls)                                                     ~ cls,
        TRUE                                                            ~ NA_character_
      )

      tibble::tibble(
        sheet       = ws_name,
        field_ref   = col_ref,
        field_clean = parsed$field_clean,
        datasource  = parsed$datasource,
        sort_order  = dir,
        sort_by     = sort_by
      )
    })
  })
  if (nrow(out) == 0L) return(.empty_sorts())
  dplyr::arrange(dplyr::distinct(out), .data$sheet, .data$field_clean)
}

.empty_sorts <- function() {
  tibble::tibble(
    sheet       = character(),
    field_ref   = character(),
    field_clean = character(),
    datasource  = character(),
    sort_order  = character(),
    sort_by     = character()
  )
}
