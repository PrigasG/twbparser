#' @importFrom xml2 xml_find_all xml_find_first xml_attr xml_name xml_parent xml_children
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows filter arrange mutate select distinct coalesce
#' @importFrom purrr map_dfr map_chr
NULL

# ---- twb_dashboard_sheets ----------------------------------------------------

#' List worksheets embedded in each dashboard
#'
#' Returns one row per worksheet per dashboard, with the zone's position on the
#' canvas.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param dashboard Optional character scalar to restrict output to one
#'   dashboard.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{dashboard}{Dashboard name.}
#'   \item{sheet}{Referenced worksheet name.}
#'   \item{zone_id}{Zone identifier.}
#'   \item{x}{Horizontal offset (pixels), or `NA`.}
#'   \item{y}{Vertical offset (pixels), or `NA`.}
#'   \item{w}{Width (pixels), or `NA`.}
#'   \item{h}{Height (pixels), or `NA`.}
#' }
#'
#' @examples
#' xml <- xml2::read_xml(
#'   '<workbook>
#'     <dashboards>
#'       <dashboard name="Overview">
#'         <zones>
#'           <zone id="1" worksheet="Sheet1" x="0" y="0" w="600" h="400"/>
#'         </zones>
#'       </dashboard>
#'     </dashboards>
#'   </workbook>'
#' )
#' twb_dashboard_sheets(xml)
#'
#' @export
twb_dashboard_sheets <- function(x, dashboard = NULL) {
  xml_doc <- .twb_resolve_xml(x)
  if (!is.null(dashboard)) {
    stopifnot(is.character(dashboard), length(dashboard) == 1L)
  }
  .ins_dashboard_sheets(xml_doc, dashboard)
}

#' @keywords internal
#' @noRd
.ins_dashboard_sheets <- function(xml_doc, dashboard = NULL) {
  d_xpath <- if (is.null(dashboard)) {
    ".//dashboard"
  } else {
    paste0(".//dashboard[@name='", gsub("'", "", dashboard, fixed = TRUE), "']")
  }
  d_nodes <- xml2::xml_find_all(xml_doc, d_xpath)
  if (length(d_nodes) == 0L) {
    return(tibble::tibble(
      dashboard = character(), sheet   = character(), zone_id = character(),
      x = integer(),           y = integer(), w = integer(), h = integer()
    ))
  }

  purrr::map_dfr(d_nodes, function(d) {
    d_name  <- xml2::xml_attr(d, "name") %||% NA_character_
    ws_zones <- xml2::xml_find_all(d, ".//zone[@worksheet]")
    if (length(ws_zones) == 0L) return(tibble::tibble())

    purrr::map_dfr(ws_zones, function(z) {
      tibble::tibble(
        dashboard = d_name,
        sheet     = xml2::xml_attr(z, "worksheet") %||% NA_character_,
        zone_id   = xml2::xml_attr(z, "id")        %||% NA_character_,
        x = .int_attr(z, "x"),
        y = .int_attr(z, "y"),
        w = .int_attr(z, "w"),
        h = .int_attr(z, "h")
      )
    })
  }) |>
    dplyr::arrange(.data$dashboard, .data$sheet)
}

# ---- twb_dashboard_layout ----------------------------------------------------

#' Full layout of dashboard zones with container hierarchy
#'
#' Returns one row per zone per dashboard, including the parent-zone relationship
#' and a tiled/floating classification.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param dashboard Optional character scalar to restrict output to one
#'   dashboard.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{dashboard}{Dashboard name.}
#'   \item{zone_id}{Zone identifier.}
#'   \item{parent_zone_id}{Parent zone identifier (`NA` for root zones).}
#'   \item{component_type}{`"worksheet"`, `"filter"`, `"legend"`,
#'     `"parameter_control"`, `"text"`, `"image"`, `"container"`, or `"blank"`.}
#'   \item{target}{Referenced worksheet name or object, if applicable.}
#'   \item{layout_type}{`"floating"` or `"tiled"`.}
#'   \item{x}{Horizontal offset, or `NA`.}
#'   \item{y}{Vertical offset, or `NA`.}
#'   \item{w}{Width, or `NA`.}
#'   \item{h}{Height, or `NA`.}
#' }
#'
#' @examples
#' xml <- xml2::read_xml(
#'   '<workbook>
#'     <dashboards>
#'       <dashboard name="Overview">
#'         <zones>
#'           <zone id="1" type="layoutV">
#'             <zone id="2" worksheet="Sheet1" x="0" y="0" w="600" h="400"/>
#'             <zone id="3" type="filter" x="0" y="400" w="600" h="50"/>
#'           </zone>
#'         </zones>
#'       </dashboard>
#'     </dashboards>
#'   </workbook>'
#' )
#' twb_dashboard_layout(xml)
#'
#' @export
twb_dashboard_layout <- function(x, dashboard = NULL) {
  xml_doc <- .twb_resolve_xml(x)
  if (!is.null(dashboard)) {
    stopifnot(is.character(dashboard), length(dashboard) == 1L)
  }
  .ins_dashboard_layout(xml_doc, dashboard)
}

#' @keywords internal
#' @noRd
.ins_dashboard_layout <- function(xml_doc, dashboard = NULL) {
  d_xpath <- if (is.null(dashboard)) {
    ".//dashboard"
  } else {
    paste0(".//dashboard[@name='", gsub("'", "", dashboard, fixed = TRUE), "']")
  }
  d_nodes <- xml2::xml_find_all(xml_doc, d_xpath)
  if (length(d_nodes) == 0L) {
    return(.empty_layout())
  }

  purrr::map_dfr(d_nodes, function(d) {
    d_name <- xml2::xml_attr(d, "name") %||% NA_character_
    zones  <- xml2::xml_find_all(d, ".//zone")
    if (length(zones) == 0L) return(tibble::tibble())

    purrr::map_dfr(zones, function(z) {
      z_id   <- xml2::xml_attr(z, "id")   %||% NA_character_
      z_type <- xml2::xml_attr(z, "type") %||% NA_character_

      # parent zone id
      parent_node <- xml2::xml_parent(z)
      parent_id <- if (!inherits(parent_node, "xml_missing") &&
                       identical(xml2::xml_name(parent_node), "zone")) {
        xml2::xml_attr(parent_node, "id") %||% NA_character_
      } else {
        NA_character_
      }

      # component type classification
      has_ws    <- !is.na(xml2::xml_attr(z, "worksheet"))
      has_filt  <- length(xml2::xml_find_all(z, "./filter | ./quick-filter")) > 0L
      has_leg   <- length(xml2::xml_find_all(z, "./legend")) > 0L
      has_param <- length(xml2::xml_find_all(z, "./parameter-control")) > 0L
      has_text  <- length(xml2::xml_find_all(z, "./text")) > 0L
      has_img   <- length(xml2::xml_find_all(z, "./image")) > 0L
      is_container <- !is.na(z_type) &&
        grepl("^layout", z_type, ignore.case = TRUE)

      comp_type <- dplyr::case_when(
        has_ws                                                              ~ "worksheet",
        has_filt | (!is.na(z_type) & grepl("filter",    z_type, ignore.case = TRUE)) ~ "filter",
        has_leg  | (!is.na(z_type) & grepl("legend",    z_type, ignore.case = TRUE)) ~ "legend",
        has_param                                                           ~ "parameter_control",
        has_text | (!is.na(z_type) & grepl("^text",     z_type, ignore.case = TRUE)) ~ "text",
        has_img  | (!is.na(z_type) & grepl("image",     z_type, ignore.case = TRUE)) ~ "image",
        is_container                                                        ~ "container",
        !is.na(z_type) & nzchar(z_type)                                    ~ z_type,
        TRUE                                                                ~ "blank"
      )

      layout_type <- if (!is.na(z_type) && grepl("floating", z_type, ignore.case = TRUE)) {
        "floating"
      } else {
        "tiled"
      }

      target <- xml2::xml_attr(z, "worksheet") %||% NA_character_

      tibble::tibble(
        dashboard      = d_name,
        zone_id        = z_id,
        parent_zone_id = parent_id,
        component_type = comp_type,
        target         = target,
        layout_type    = layout_type,
        x = .int_attr(z, "x"),
        y = .int_attr(z, "y"),
        w = .int_attr(z, "w"),
        h = .int_attr(z, "h")
      )
    })
  }) |>
    dplyr::arrange(.data$dashboard, .data$zone_id)
}

.empty_layout <- function() {
  tibble::tibble(
    dashboard      = character(),
    zone_id        = character(),
    parent_zone_id = character(),
    component_type = character(),
    target         = character(),
    layout_type    = character(),
    x = integer(), y = integer(), w = integer(), h = integer()
  )
}

# ---- twb_dashboard_actions ---------------------------------------------------

#' Extract dashboard and workbook actions
#'
#' Parses `<action>` nodes from dashboard `<actions>` sections and the
#' top-level workbook `<actions>` section. Returns one row per action.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#' @param dashboard Optional character scalar. When supplied, only actions
#'   whose `<source-sheets>` include a sheet from that dashboard are returned.
#'   Pass `NULL` (default) to return all actions.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{action_name}{Caption / display name of the action.}
#'   \item{action_type}{`"filter"`, `"highlight"`, `"url"`, `"set"`, or
#'     another type string from the XML.}
#'   \item{source_sheets}{Comma-separated list of source worksheet names.}
#'   \item{target_sheet}{Target worksheet name, or `NA` for URL actions.}
#'   \item{run_on}{Trigger: `"select"`, `"menu"`, or `"hover"`.}
#'   \item{url}{URL value for URL-type actions; `NA` otherwise.}
#' }
#'
#' @examples
#' xml <- xml2::read_xml(
#'   '<workbook>
#'     <actions>
#'       <action caption="Filter 1" name="act1" type="filter">
#'         <source-sheets>
#'           <source-sheet name="Sheet1"/>
#'         </source-sheets>
#'         <target-sheets>
#'           <target-sheet name="Sheet2"/>
#'         </target-sheets>
#'         <run-on type="select"/>
#'       </action>
#'     </actions>
#'   </workbook>'
#' )
#' twb_dashboard_actions(xml)
#'
#' @export
twb_dashboard_actions <- function(x, dashboard = NULL) {
  xml_doc <- .twb_resolve_xml(x)
  if (!is.null(dashboard)) {
    stopifnot(is.character(dashboard), length(dashboard) == 1L)
  }
  .ins_dashboard_actions(xml_doc, dashboard)
}

#' @keywords internal
#' @noRd
.ins_dashboard_actions <- function(xml_doc, dashboard = NULL) {
  act_nodes <- xml2::xml_find_all(xml_doc, ".//actions/action")
  if (length(act_nodes) == 0L) {
    return(.empty_actions())
  }

  out <- purrr::map_dfr(act_nodes, function(act) {
    caption <- xml2::xml_attr(act, "caption") %||%
               xml2::xml_attr(act, "name")    %||% NA_character_
    atype   <- xml2::xml_attr(act, "type")    %||% NA_character_

    src_sheets <- xml2::xml_find_all(act, ".//source-sheet")
    src_names  <- xml2::xml_attr(src_sheets, "name")
    src_names  <- src_names[!is.na(src_names)]
    src_str    <- if (length(src_names)) paste(src_names, collapse = ", ") else NA_character_

    tgt_sheets <- xml2::xml_find_all(act, ".//target-sheet")
    tgt_names  <- xml2::xml_attr(tgt_sheets, "name")
    tgt_names  <- tgt_names[!is.na(tgt_names)]
    tgt_str    <- if (length(tgt_names)) paste(tgt_names, collapse = ", ") else NA_character_

    run_on_nd  <- xml2::xml_find_first(act, ".//run-on")
    run_on     <- if (!inherits(run_on_nd, "xml_missing")) {
      xml2::xml_attr(run_on_nd, "type") %||% NA_character_
    } else {
      NA_character_
    }

    url_nd <- xml2::xml_find_first(act, ".//url")
    url    <- if (!inherits(url_nd, "xml_missing")) {
      xml2::xml_attr(url_nd, "value") %||% NA_character_
    } else {
      NA_character_
    }

    tibble::tibble(
      action_name   = caption,
      action_type   = atype,
      source_sheets = src_str,
      target_sheet  = tgt_str,
      run_on        = run_on,
      url           = url
    )
  })

  # Optional: filter to actions relevant to a named dashboard
  if (!is.null(dashboard)) {
    # Get sheets in that dashboard
    dash_sheets <- .ins_dashboard_sheets(xml_doc, dashboard)$sheet
    if (length(dash_sheets)) {
      out <- dplyr::filter(
        out,
        vapply(
          source_sheets,
          function(s) {
            if (is.na(s)) return(FALSE)
            any(dash_sheets %in% trimws(strsplit(s, ",", fixed = TRUE)[[1L]]))
          },
          logical(1L)
        )
      )
    }
  }

  out |>
    dplyr::distinct() |>
    dplyr::arrange(.data$action_type, .data$action_name)
}

.empty_actions <- function() {
  tibble::tibble(
    action_name   = character(),
    action_type   = character(),
    source_sheets = character(),
    target_sheet  = character(),
    run_on        = character(),
    url           = character()
  )
}

# ---- Shared helpers ----------------------------------------------------------

#' @keywords internal
.int_attr <- function(node, attr_name) {
  suppressWarnings(as.integer(xml2::xml_attr(node, attr_name)))
}
