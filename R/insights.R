#' @importFrom xml2 xml_find_all xml_find_first xml_attr xml_name xml_parent
#' @importFrom tibble tibble
#' @importFrom purrr map_dfr
#' @importFrom dplyr arrange mutate select filter count left_join group_by summarise bind_rows coalesce everything
NULL

#' @keywords internal
#' @noRd
.twb_resolve_xml <- function(x) {
  if (inherits(x, "TwbParser")) return(x$xml_doc)
  if (inherits(x, "xml_document")) return(x)
  stop("Expected a TwbParser or an xml2 document.")
}

# ---- Internal workers -------------------------------------------------------

# List all pages (dashboards, worksheets, stories)
#' @keywords internal
#' @noRd
.ins_pages <- function(xml_doc) {
  dashboards <- xml2::xml_find_all(xml_doc, ".//dashboard")
  worksheets <- xml2::xml_find_all(xml_doc, ".//worksheet")
  stories    <- xml2::xml_find_all(xml_doc, ".//story")

  nm_dash <- xml2::xml_attr(dashboards, "name")
  nm_work <- xml2::xml_attr(worksheets, "name")
  nm_story<- xml2::xml_attr(stories, "name")

  tibble::tibble(
    page_type = c(
      rep("dashboard", length(nm_dash)),
      rep("worksheet", length(nm_work)),
      rep("story",     length(nm_story))
    ),
    name = c(nm_dash, nm_work, nm_story)
  ) |>
    dplyr::filter(!is.na(.data$name) & nzchar(.data$name)) |>
    dplyr::arrange(.data$page_type, .data$name)
}

# Composition of a named page (what it contains and where)
# For dashboards: returns one row per zone, classifies component_type,
# includes x/y/w/h when present.
# For worksheets: returns mark types and any inline filter/legend/parameter controls.
# For stories: returns one row per story point with referenced target.
#' @keywords internal
#' @noRd
.ins_page_composition <- function(xml_doc, name) {
  safe_name <- gsub("([\"'])", "", as.character(name))
  if (!nzchar(safe_name)) return(tibble::tibble())

  d <- xml2::xml_find_first(xml_doc, paste0(".//dashboard[@name='", safe_name, "']"))
  if (!inherits(d, "xml_missing")) {
    zones <- xml2::xml_find_all(d, ".//zone")
    if (length(zones) == 0) return(
      tibble::tibble(
        page_type = "dashboard", page_name = xml2::xml_attr(d, "name"),
        component_type = character(), zone_id = character(),
        target = character(), field = character(), presentation = character(),
        x = integer(), y = integer(), w = integer(), h = integer()
      )
    )
    out <- purrr::map_dfr(zones, function(z) {
      z_id   <- xml2::xml_attr(z, "id")
      z_type <- xml2::xml_attr(z, "type")
      x <- suppressWarnings(as.integer(xml2::xml_attr(z, "x"))); if (is.na(x)) x <- NA_integer_
      y <- suppressWarnings(as.integer(xml2::xml_attr(z, "y"))); if (is.na(y)) y <- NA_integer_
      w <- suppressWarnings(as.integer(xml2::xml_attr(z, "w"))); if (is.na(w)) w <- NA_integer_
      h <- suppressWarnings(as.integer(xml2::xml_attr(z, "h"))); if (is.na(h)) h <- NA_integer_

      has_ws    <- !is.na(xml2::xml_attr(z, "worksheet"))
      has_filt  <- length(xml2::xml_find_all(z, ".//filter | .//quick-filter")) > 0
      has_leg   <- length(xml2::xml_find_all(z, ".//legend")) > 0
      has_param <- length(xml2::xml_find_all(z, ".//parameter-control")) > 0
      has_text  <- length(xml2::xml_find_all(z, ".//text")) > 0
      has_img   <- length(xml2::xml_find_all(z, ".//image")) > 0

      comp_type <- if (has_ws) {
        "worksheet"
      } else if (has_filt || grepl("filter", tolower(z_type %||% ""), fixed = TRUE)) {
        "filter"
      } else if (has_leg || grepl("legend", tolower(z_type %||% ""), fixed = TRUE)) {
        "legend"
      } else if (has_param) {
        "parameter_control"
      } else if (has_text) {
        "text"
      } else if (has_img || grepl("image", tolower(z_type %||% ""), fixed = TRUE)) {
        "image"
      } else if (!is.null(z_type) && nzchar(z_type)) {
        paste0("zone:", z_type)
      } else {
        "zone"
      }

      target_ws <- xml2::xml_attr(z, "worksheet")
      field_node <- xml2::xml_find_first(z, ".//filter | .//quick-filter")
      field <- if (!inherits(field_node, "xml_missing")) {
        xml2::xml_attr(field_node, "field") %||% xml2::xml_attr(field_node, "name")
      } else NA_character_

      style_node <- xml2::xml_find_first(z, ".//style | .//format")
      presentation <- if (!inherits(style_node, "xml_missing")) {
        xml2::xml_attr(style_node, "type") %||% NA_character_
      } else NA_character_

      tibble::tibble(
        page_type = "dashboard",
        page_name = xml2::xml_attr(d, "name"),
        component_type = comp_type,
        zone_id = z_id %||% NA_character_,
        target = (target_ws %||% field) %||% NA_character_,
        field = if (identical(comp_type, "filter")) field else NA_character_,
        presentation = if (identical(comp_type, "filter")) presentation else NA_character_,
        x = x, y = y, w = w, h = h
      )
    })
    return(out |>
             dplyr::arrange(.data$component_type, .data$zone_id))
  }

  w <- xml2::xml_find_first(xml_doc, paste0(".//worksheet[@name='", safe_name, "']"))
  if (!inherits(w, "xml_missing")) {
    marks <- xml2::xml_find_all(w, ".//mark")
    mtypes <- unique(tolower(xml2::xml_attr(marks, "type")))
    filt_nodes  <- xml2::xml_find_all(w, ".//filter | .//quick-filter")
    param_nodes <- xml2::xml_find_all(w, ".//parameter-control")
    legend_nodes<- xml2::xml_find_all(w, ".//legend")

    # rows for mark types
    mt <- if (length(mtypes)) {
      tibble::tibble(
        page_type = "worksheet",
        page_name = xml2::xml_attr(w, "name"),
        component_type = "mark_type",
        zone_id = NA_character_,
        target = mtypes,
        field = NA_character_,
        presentation = NA_character_,
        x = NA_integer_, y = NA_integer_, w = NA_integer_, h = NA_integer_
      )
    } else tibble::tibble()

    # rows for filters
    fl <- if (length(filt_nodes)) {
      tibble::tibble(
        page_type = "worksheet",
        page_name = xml2::xml_attr(w, "name"),
        component_type = "filter",
        zone_id = NA_character_,
        target = NA_character_,
        field = vapply(
          filt_nodes,
          function(n) xml2::xml_attr(n, "field") %||% xml2::xml_attr(n, "name") %||% NA_character_,
          character(1)
        ),
        presentation = vapply(
          filt_nodes,
          function(n) {
            sn <- xml2::xml_find_first(n, ".//style | .//format")
            if (inherits(sn, "xml_missing")) NA_character_ else xml2::xml_attr(sn, "type") %||% NA_character_
          },
          character(1)
        ),
        x = NA_integer_, y = NA_integer_, w = NA_integer_, h = NA_integer_
      )
    } else tibble::tibble()

    # rows for parameter controls
    pc <- if (length(param_nodes)) {
      tibble::tibble(
        page_type = "worksheet",
        page_name = xml2::xml_attr(w, "name"),
        component_type = "parameter_control",
        zone_id = NA_character_,
        target = NA_character_,
        field = NA_character_,
        presentation = NA_character_,
        x = NA_integer_, y = NA_integer_, w = NA_integer_, h = NA_integer_
      )
    } else tibble::tibble()

    # rows for legends
    lg <- if (length(legend_nodes)) {
      tibble::tibble(
        page_type = "worksheet",
        page_name = xml2::xml_attr(w, "name"),
        component_type = "legend",
        zone_id = NA_character_,
        target = NA_character_,
        field = NA_character_,
        presentation = NA_character_,
        x = NA_integer_, y = NA_integer_, w = NA_integer_, h = NA_integer_
      )
    } else tibble::tibble()

    return(dplyr::bind_rows(mt, fl, pc, lg) |>
             dplyr::arrange(.data$component_type, .data$field))
  }

  s <- xml2::xml_find_first(xml_doc, paste0(".//story[@name='", safe_name, "']"))
  if (!inherits(s, "xml_missing")) {
    pts <- xml2::xml_find_all(s, ".//story-point")
    if (length(pts) == 0) return(
      tibble::tibble(
        page_type = "story", page_name = xml2::xml_attr(s, "name"),
        component_type = character(), zone_id = character(), target = character(),
        field = character(), presentation = character(),
        x = integer(), y = integer(), w = integer(), h = integer_
      )
    )
    out <- purrr::map_dfr(seq_along(pts), function(i) {
      p <- pts[[i]]
      tgt <- xml2::xml_attr(p, "story-point-content") %||%
        xml2::xml_attr(p, "sheet") %||%
        xml2::xml_attr(p, "dashboard") %||%
        NA_character_
      tibble::tibble(
        page_type = "story",
        page_name = xml2::xml_attr(s, "name"),
        component_type = "story_point",
        zone_id = as.character(i),
        target = tgt,
        field = NA_character_,
        presentation = NA_character_,
        x = NA_integer_, y = NA_integer_, w = NA_integer_, h = NA_integer_
      )
    })
    return(out)
  }

  tibble::tibble()
}

# Summary of all pages (counts and quick descriptors)
#' @keywords internal
#' @noRd
.ins_pages_summary <- function(xml_doc) {
  pages <- .ins_pages(xml_doc)

  # dashboards
  dsum <- if (any(pages$page_type == "dashboard")) {
    d_nodes <- xml2::xml_find_all(xml_doc, ".//dashboard")
    purrr::map_dfr(d_nodes, function(d) {
      nm <- xml2::xml_attr(d, "name") %||% NA_character_
      zones <- xml2::xml_find_all(d, ".//zone")
      ws    <- xml2::xml_attr(xml2::xml_find_all(d, ".//zone[@worksheet]"), "worksheet")
      filt  <- sum(vapply(zones, function(z) {
        zt <- xml2::xml_attr(z, "type") %||% ""
        as.integer(grepl("filter", tolower(zt), fixed = TRUE) ||
                     length(xml2::xml_find_all(z, ".//filter | .//quick-filter")) > 0)
      }, integer(1)))
      leg   <- sum(vapply(zones, function(z) {
        as.integer(length(xml2::xml_find_all(z, ".//legend")) > 0)
      }, integer(1)))
      param <- sum(vapply(zones, function(z) {
        as.integer(length(xml2::xml_find_all(z, ".//parameter-control")) > 0)
      }, integer(1)))
      tibble::tibble(
        name = nm,
        n_zones = length(zones),
        n_worksheets = length(unique(ws)),
        n_filters = filt,
        n_legends = leg,
        n_parameter_controls = param
      )
    })
  } else tibble::tibble()

  # worksheets
  wsum <- if (any(pages$page_type == "worksheet")) {
    w_nodes <- xml2::xml_find_all(xml_doc, ".//worksheet")
    purrr::map_dfr(w_nodes, function(w) {
      nm <- xml2::xml_attr(w, "name") %||% NA_character_
      marks <- xml2::xml_find_all(w, ".//mark")
      types <- unique(tolower(xml2::xml_attr(marks, "type")))
      filt  <- length(xml2::xml_find_all(w, ".//filter | .//quick-filter"))
      leg   <- length(xml2::xml_find_all(w, ".//legend"))
      param <- length(xml2::xml_find_all(w, ".//parameter-control"))
      tibble::tibble(
        name = nm,
        mark_types = paste(types[!is.na(types) & nzchar(types)], collapse = ", "),
        n_filters = filt,
        n_legends = leg,
        n_parameter_controls = param
      )
    })
  } else tibble::tibble()

  # stories
  ssum <- if (any(pages$page_type == "story")) {
    s_nodes <- xml2::xml_find_all(xml_doc, ".//story")
    purrr::map_dfr(s_nodes, function(s) {
      nm <- xml2::xml_attr(s, "name") %||% NA_character_
      pts <- xml2::xml_find_all(s, ".//story-point")
      tibble::tibble(
        name = nm,
        n_story_points = length(pts)
      )
    })
  } else tibble::tibble()

  dplyr::bind_rows(
    dplyr::mutate(dsum, page_type = "dashboard"),
    dplyr::mutate(wsum, page_type = "worksheet"),
    dplyr::mutate(ssum, page_type = "story")
  ) |>
    dplyr::select(page_type, dplyr::everything()) |>
    dplyr::arrange(page_type, name)
}

# Dashboards overview (compat layer, used by twb_dashboard_summary)
#' @keywords internal
#' @noRd
.ins_dashboards <- function(xml_doc) {
  d_nodes <- xml2::xml_find_all(xml_doc, ".//dashboard")
  if (length(d_nodes) == 0) {
    return(tibble::tibble(
      dashboard       = character(),
      worksheet_count = integer(),
      zone_count      = integer()
    ))
  }
  purrr::map_dfr(d_nodes, function(d) {
    name  <- xml2::xml_attr(d, "name") %||% NA_character_
    w_refs <- xml2::xml_find_all(d, ".//zone[@worksheet]")
    zones  <- xml2::xml_find_all(d, ".//zone")
    tibble::tibble(
      dashboard       = name,
      worksheet_count = length(unique(xml2::xml_attr(w_refs, "worksheet"))),
      zone_count      = length(zones)
    )
  }) |>
    dplyr::arrange(dashboard)
}


# Filters per dashboard (compat layer)
#' @keywords internal
#' @noRd
.ins_dashboard_filters <- function(xml_doc, dashboard = NULL) {
  d_xpath <- if (is.null(dashboard)) ".//dashboard" else {
    paste0(".//dashboard[@name='", gsub("([\"'])", "", dashboard), "']")
  }
  d_nodes <- xml2::xml_find_all(xml_doc, d_xpath)
  if (length(d_nodes) == 0) return(tibble::tibble())

  purrr::map_dfr(d_nodes, function(d) {
    d_name <- xml2::xml_attr(d, "name") %||% NA_character_
    zones <- xml2::xml_find_all(d, ".//zone")
    purrr::map_dfr(zones, function(z) {
      z_id   <- xml2::xml_attr(z, "id")   %||% NA_character_
      z_type <- xml2::xml_attr(z, "type") %||% NA_character_
      x <- suppressWarnings(as.integer(xml2::xml_attr(z, "x"))); if (is.na(x)) x <- NA_integer_
      y <- suppressWarnings(as.integer(xml2::xml_attr(z, "y"))); if (is.na(y)) y <- NA_integer_
      w <- suppressWarnings(as.integer(xml2::xml_attr(z, "w"))); if (is.na(w)) w <- NA_integer_
      h <- suppressWarnings(as.integer(xml2::xml_attr(z, "h"))); if (is.na(h)) h <- NA_integer_

      is_filter <- grepl("filter", tolower(z_type %||% ""), fixed = TRUE) ||
        length(xml2::xml_find_all(z, ".//filter | .//quick-filter")) > 0
      if (!is_filter) return(tibble::tibble())

      field_node <- xml2::xml_find_first(z, ".//filter | .//quick-filter")
      field_name <- if (!inherits(field_node, "xml_missing")) {
        xml2::xml_attr(field_node, "field") %||% xml2::xml_attr(field_node, "name")
      } else NA_character_

      style_node <- xml2::xml_find_first(z, ".//style | .//format")
      style <- if (!inherits(style_node, "xml_missing")) {
        xml2::xml_attr(style_node, "type") %||% NA_character_
      } else NA_character_

      tibble::tibble(
        dashboard = d_name,
        zone_id = z_id,
        zone_type = z_type,
        field = field_name,
        presentation = style,
        x = x, y = y, w = w, h = h
      )
    })
  }) |>
    dplyr::arrange(.data$dashboard, .data$zone_id)
}

# Chart types per worksheet (compat layer)
#' @keywords internal
#' @noRd
.ins_charts <- function(xml_doc) {
  w_nodes <- xml2::xml_find_all(xml_doc, ".//worksheet")
  if (length(w_nodes) == 0) return(tibble::tibble())
  purrr::map_dfr(w_nodes, function(w) {
    w_name <- xml2::xml_attr(w, "name") %||% NA_character_
    mark_nodes <- xml2::xml_find_all(w, ".//mark")
    types <- unique(tolower(xml2::xml_attr(mark_nodes, "type")))
    tibble::tibble(
      worksheet = w_name,
      mark_types = paste(types[!is.na(types) & nzchar(types)], collapse = ", ")
    )
  }) |>
    dplyr::arrange(.data$worksheet)
}

# Colors and palettes referenced in the workbook
#' @keywords internal
#' @noRd
.ins_colors <- function(xml_doc) {
  # palettes
  pal_nodes <- xml2::xml_find_all(xml_doc, ".//color-palette | .//palette")
  pal_tbl <- if (length(pal_nodes)) {
    purrr::map_dfr(pal_nodes, function(p) {
      parent <- xml2::xml_parent(p)
      scope  <- if (inherits(parent, "xml_missing")) NA_character_ else xml2::xml_name(parent)
      tibble::tibble(
        scope        = scope %||% NA_character_,
        palette_name = xml2::xml_attr(p, "name") %||% NA_character_
      )
    })
  } else {
    tibble::tibble(scope = character(), palette_name = character())
  }

  # concrete color entries (various encodings)
  color_nodes <- xml2::xml_find_all(xml_doc, ".//color | .//palette/color | .//map/entry[@color]")
  col_tbl <- if (length(color_nodes)) {
    purrr::map_dfr(color_nodes, function(cn) {
      tibble::tibble(
        role  = xml2::xml_name(cn) %||% "entry",
        value = xml2::xml_attr(cn, "color") %||%
          xml2::xml_attr(cn, "rgb")   %||%
          xml2::xml_attr(cn, "hex"),
        label = xml2::xml_attr(cn, "label") %||% NA_character_
      )
    }) |>
      dplyr::filter(!is.na(value) & nzchar(value))
  } else {
    tibble::tibble(role = character(), value = character(), label = character())
  }

  # normalize rgb "r,g,b" -> #RRGGBB (leave hex alone)
  to_hex <- function(v) {
    v <- trimws(v %||% "")
    if (grepl("^[0-9]+,[0-9]+,[0-9]+$", v)) {
      nums <- as.integer(strsplit(v, ",", fixed = TRUE)[[1]])
      nums <- pmax(0L, pmin(255L, nums))
      sprintf("#%02X%02X%02X", nums[1], nums[2], nums[3])
    } else v
  }
  if (nrow(col_tbl)) {
    col_tbl$value <- vapply(col_tbl$value, to_hex, character(1))
  }

  # bind rows safely even if one side is empty
  palettes_out <- if (nrow(pal_tbl)) {
    dplyr::mutate(pal_tbl, kind = "palette", detail = palette_name) |>
      dplyr::select(kind, detail, scope)
  } else {
    tibble::tibble(kind = character(), detail = character(), scope = character())
  }

  colors_out <- if (nrow(col_tbl)) {
    dplyr::mutate(col_tbl, kind = "color", detail = value) |>
      dplyr::select(kind, detail, label)
  } else {
    tibble::tibble(kind = character(), detail = character(), label = character())
  }

  dplyr::bind_rows(palettes_out, colors_out)
}


# Per-dashboard summary (compat with previous helper)
#' @keywords internal
#' @noRd
.ins_dashboard_summary <- function(xml_doc) {
  d <- .ins_dashboards(xml_doc)
  f <- .ins_dashboard_filters(xml_doc)
  c <- .ins_charts(xml_doc)

  f_sum <- if (nrow(f)) dplyr::count(f, dashboard, name = "filters")
  else tibble::tibble(dashboard = character(), filters = integer())

  dash_nodes <- xml2::xml_find_all(xml_doc, ".//dashboard")
  dash_refs <- if (length(dash_nodes)) {
    purrr::map_dfr(dash_nodes, function(dnode) {
      dnm <- xml2::xml_attr(dnode, "name") %||% NA_character_
      w   <- xml2::xml_attr(xml2::xml_find_all(dnode, ".//zone[@worksheet]"), "worksheet")
      if (length(w)) tibble::tibble(dashboard = rep_len(dnm, length(w)), worksheet = w)
      else           tibble::tibble(dashboard = character(), worksheet = character())
    })
  } else tibble::tibble(dashboard = character(), worksheet = character())

  chart_by_dash <- if (nrow(dash_refs) && nrow(c)) {
    dplyr::left_join(dash_refs, c, by = "worksheet") |>
      dplyr::group_by(dashboard) |>
      dplyr::summarise(
        chart_types = paste(
          sort(unique(unlist(strsplit(paste(mark_types, collapse = ", "), ",\\s*")))),
          collapse = ", "
        ),
        .groups = "drop"
      )
  } else tibble::tibble(dashboard = character(), chart_types = character())

  out <- d |>
    dplyr::left_join(f_sum,       by = "dashboard") |>
    dplyr::left_join(chart_by_dash, by = "dashboard") |>
    dplyr::mutate(
      filters     = dplyr::coalesce(filters, 0L),
      chart_types = dplyr::coalesce(chart_types, "")
    ) |>
    dplyr::arrange(.data$dashboard)

  # Guarantee schema even when empty
  if (!nrow(out)) {
    out <- tibble::tibble(
      dashboard       = character(),
      worksheet_count = integer(),
      zone_count      = integer(),
      filters         = integer(),
      chart_types     = character()
    )
  }
  out
}


# ---- User-callable wrappers -------------------------------------------------

#' List all pages (dashboards, worksheets, stories).
#'
#' @param x TwbParser or xml2 document.
#' @return Tibble with columns: page_type, name.
#' @export
twb_pages <- function(x) {
  .ins_pages(.twb_resolve_xml(x))
}

#' Show what a specific page is composed of.
#'
#' For a dashboard: one row per zone with component type, target (worksheet or field),
#' filter presentation (if applicable), and x/y/w/h when present.
#' For a worksheet: mark types, filters, legends, parameter controls.
#' For a story: one row per story point with its referenced target.
#'
#' @param x TwbParser or xml2 document.
#' @param name Page name (character scalar).
#' @return Tibble with columns: page_type, page_name, component_type, zone_id,
#'   target, field, presentation, x, y, w, h.
#' @export
twb_page_composition <- function(x, name) {
  stopifnot(is.character(name), length(name) == 1L)
  .ins_page_composition(.twb_resolve_xml(x), name)
}

#' Summary of all pages (counts and quick descriptors).
#'
#' @param x TwbParser or xml2 document.
#' @return Tibble with columns including page_type, name, and count columns such as
#'   n_zones, n_worksheets, n_filters, n_legends, n_parameter_controls, n_story_points,
#'   and mark_types for worksheets.
#' @export
twb_pages_summary <- function(x) {
  .ins_pages_summary(.twb_resolve_xml(x))
}

#' Dashboards overview (count of zones and referenced worksheets).
#'
#' @param x TwbParser or xml2 document.
#' @return Tibble with columns: dashboard, worksheet_count, zone_count.
#' @export
twb_dashboards <- function(x) {
  .ins_dashboards(.twb_resolve_xml(x))
}

#' Filters found on dashboards and their positions.
#'
#' @param x TwbParser or xml2 document.
#' @param dashboard Optional dashboard name to filter to.
#' @return Tibble with columns: dashboard, zone_id, zone_type, field, presentation, x, y, w, h.
#' @export
twb_dashboard_filters <- function(x, dashboard = NULL) {
  .ins_dashboard_filters(.twb_resolve_xml(x), dashboard = dashboard)
}

#' Chart (mark) types per worksheet.
#'
#' @param x TwbParser or xml2 document.
#' @return Tibble with columns: worksheet, mark_types (comma-separated).
#' @export
twb_charts <- function(x) {
  .ins_charts(.twb_resolve_xml(x))
}

#' Colors and palettes referenced in the workbook.
#'
#' @param x TwbParser or xml2 document.
#' @return Tibble with columns describing palette names and explicit colors.
#' @export
twb_colors <- function(x) {
  .ins_colors(.twb_resolve_xml(x))
}

#' Per-dashboard summary (filters count and chart types).
#'
#' @param x TwbParser or xml2 document.
#' @return Tibble with columns: dashboard, worksheet_count, zone_count, filters, chart_types.
#' @export
twb_dashboard_summary <- function(x) {
  .ins_dashboard_summary(.twb_resolve_xml(x))
}
