# ---- rebuild kit -------------------------------------------------------------
# "What do I need to recreate?" helpers for rebuilding a workbook in another
# tool: which fields are actually used, the order in which calculated fields
# must be rebuilt, and where each parameter is consumed.

#' Canonical name key used to match fields across XML locations
#'
#' Lower-cased, bracket-free, derivation-free form of a field reference, so
#' `[sales-data].[none:Sales:qk]`, `[Sales]`, and the display name `"Sales"`
#' all reduce to the same key.
#'
#' @param x Character vector of field references or names.
#' @return Character vector of canonical keys (`NA` where unparseable).
#' @keywords internal
#' @noRd
.rebuild_key <- function(x) {
  x <- as.character(x)
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x) & nzchar(trimws(x))
  v <- trimws(x[ok])
  v <- gsub("\\[|\\]", "", v)        # strip [ ]
  v <- sub("^([a-z]+:)+", "", v)     # strip derivation prefixes (none:, clct:)
  v <- sub("(:[a-z0-9]+)+$", "", v)  # strip pivot suffixes (:nk, :qk)
  v <- sub("^.*\\.", "", v)          # table-qualified tail: T.F -> F
  v <- trimws(v)
  v[!nzchar(v)] <- NA_character_
  out[ok] <- v
  tolower(out)
}

#' Every match key for one field (internal name + display name)
#'
#' @param internal Tableau internal name, e.g. `"[Calculation_0001]"`.
#' @param name Display name / caption, e.g. `"Profit Ratio"`.
#' @return Character vector of canonical keys.
#' @keywords internal
#' @noRd
.rebuild_field_keys <- function(internal, name) {
  keys <- c(.rebuild_key(internal), .rebuild_key(name))
  unique(keys[!is.na(keys) & nzchar(keys)])
}

#' Collect every "used" field key in the workbook
#'
#' A field counts as used when it appears on any worksheet shelf, filter, or
#' sort, inside any customized tooltip text, inside any calculated-field
#' formula, or in any dashboard filter zone.
#'
#' @param xml_doc An `xml2` document for a Tableau `.twb`.
#' @return Character vector of canonical keys.
#' @keywords internal
#' @noRd
.rebuild_used_keys <- function(xml_doc) {
  used <- character()

  # 1. worksheet shelves (rows, cols, encodings)
  sh <- tryCatch(.ins_sheet_shelves(xml_doc), error = function(e) .empty_shelves())
  if (nrow(sh) > 0L) used <- c(used, .rebuild_key(sh$field_clean))

  # 2. worksheet filters
  fl <- tryCatch(.ins_sheet_filters(xml_doc), error = function(e) .empty_filters())
  if (nrow(fl) > 0L) used <- c(used, .rebuild_key(fl$field_clean))

  # 3. worksheet sorts
  so <- tryCatch(.ins_sheet_sorts(xml_doc), error = function(e) .empty_sorts())
  if (nrow(so) > 0L) used <- c(used, .rebuild_key(so$field_clean))

  # 4. customized tooltip text, e.g. "Sales: <[Sales]>"
  tt <- tryCatch(.ins_tooltips(xml_doc), error = function(e) .empty_tooltips())
  if (nrow(tt) > 0L) {
    txt <- tt$tooltip_text[!is.na(tt$tooltip_text) & nzchar(tt$tooltip_text)]
    for (t in txt) {
      m <- regmatches(t, gregexpr("\\[[^\\]]+\\]", t, perl = TRUE))[[1L]]
      used <- c(used, .rebuild_key(m))
    }
  }

  # 5. calculated-field formulas
  calcs <- tryCatch(
    extract_calculated_fields(xml_doc),
    error = function(e) tibble::tibble()
  )
  if (nrow(calcs) > 0L && "formula" %in% names(calcs)) {
    for (f in calcs$formula) used <- c(used, .rebuild_key(.extract_tokens(f)))
  }

  # 6. dashboard filter zones
  df <- tryCatch(
    .ins_dashboard_filters(xml_doc),
    error = function(e) .empty_dashboard_filters()
  )
  if (nrow(df) > 0L && "field" %in% names(df)) {
    used <- c(used, .rebuild_key(df$field))
  }

  used <- used[!is.na(used) & nzchar(used)]
  unique(used)
}

#' Every field in the workbook in one standardized table
#'
#' Combines raw fields, calculated fields, and parameters with a
#' `field_type` discriminator.
#'
#' @param xml_doc An `xml2` document for a Tableau `.twb`.
#' @return A tibble with columns `datasource`, `field_type` (`"raw"`,
#'   `"calculated"`, or `"parameter"`), `name`, `tableau_internal_name`,
#'   `datatype`, `role`, and `is_hidden` (`NA` for calculated fields and
#'   parameters).
#' @keywords internal
#' @noRd
.rebuild_all_fields <- function(xml_doc) {
  raw <- tryCatch(extract_raw_fields(xml_doc), error = function(e) tibble::tibble())
  calcs <- tryCatch(
    extract_calculated_fields(xml_doc),
    error = function(e) tibble::tibble()
  )
  params <- tryCatch(
    extract_parameters(xml_doc),
    error = function(e) tibble::tibble()
  )

  std <- function(df, type) {
    if (is.null(df) || nrow(df) == 0L) {
      return(tibble::tibble(
        datasource            = character(),
        field_type            = character(),
        name                  = character(),
        tableau_internal_name = character(),
        datatype              = character(),
        role                  = character(),
        is_hidden             = logical()
      ))
    }
    col <- function(nm) {
      if (nm %in% names(df)) as.character(df[[nm]]) else rep(NA_character_, nrow(df))
    }
    tibble::tibble(
      datasource            = col("datasource"),
      field_type            = type,
      name                  = col("name"),
      tableau_internal_name = col("tableau_internal_name"),
      datatype              = col("datatype"),
      role                  = col("role"),
      is_hidden             = if ("is_hidden" %in% names(df)) {
        as.logical(df[["is_hidden"]])
      } else {
        rep(NA, nrow(df))
      }
    )
  }

  dplyr::bind_rows(
    std(raw, "raw"),
    std(calcs, "calculated"),
    std(params, "parameter")
  )
}

.empty_unused_fields <- function() {
  tibble::tibble(
    datasource            = character(),
    field_type            = character(),
    name                  = character(),
    tableau_internal_name = character(),
    datatype              = character(),
    role                  = character(),
    is_hidden             = logical()
  )
}

# ---- twb_unused_fields --------------------------------------------------------

#' Fields defined in the workbook but never used
#'
#' Lists every raw field, calculated field, and parameter that is defined in a
#' datasource but referenced nowhere: not on any worksheet shelf, filter, or
#' sort, not in any tooltip, not inside any other calculated-field formula,
#' and not in any dashboard filter zone. When rebuilding a workbook in another
#' tool, these are the fields that can safely be left behind.
#'
#' @details
#' Matching is deliberately conservative: a field is reported as unused only
#' when none of its name forms (internal name or display caption, in any
#' bracket/derivation wrapping) appears anywhere. A field whose name merely
#' collides with a used field in another datasource is treated as used.
#'
#' This complements [twb_field_usage()], which shows where each *used* field
#' appears across worksheets.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{datasource}{Datasource the field belongs to.}
#'   \item{field_type}{`"raw"`, `"calculated"`, or `"parameter"`.}
#'   \item{name}{Human-readable field name / caption.}
#'   \item{tableau_internal_name}{Bracketed internal Tableau name.}
#'   \item{datatype}{Field data type.}
#'   \item{role}{`"measure"` or `"dimension"`.}
#'   \item{is_hidden}{Whether a raw field is hidden (`NA` for calculated
#'     fields and parameters).}
#' }
#'
#' @examples
#' twb <- system.file("extdata", "rebuild_kit.twb", package = "twbparser")
#' stopifnot(nzchar(twb), file.exists(twb))
#' xml <- xml2::read_xml(twb)
#'
#' # the fixture leaves exactly one raw field, one calc, and one parameter unused
#' twb_unused_fields(xml)
#'
#' # same result through the parser object
#' parser <- TwbParser$new(twb)
#' parser$get_unused_fields()
#' parser$unused_fields
#'
#' @seealso [twb_field_usage()] for where used fields appear,
#'   [twb_calc_build_order()] for rebuilding calculations in dependency order.
#'
#' @export
twb_unused_fields <- function(x) {
  xml_doc <- .twb_resolve_xml(x)
  fields <- .rebuild_all_fields(xml_doc)
  if (nrow(fields) == 0L) return(.empty_unused_fields())

  used <- .rebuild_used_keys(xml_doc)
  is_used <- vapply(seq_len(nrow(fields)), function(i) {
    keys <- .rebuild_field_keys(
      fields$tableau_internal_name[i],
      fields$name[i]
    )
    any(keys %in% used)
  }, logical(1L))

  fields[!is_used, , drop = FALSE] |>
    dplyr::arrange(.data$field_type, .data$datasource, .data$name) |>
    tibble::as_tibble()
}

# ---- twb_calc_build_order -----------------------------------------------------

.empty_build_order <- function() {
  tibble::tibble(
    build_order           = integer(),
    datasource            = character(),
    name                  = character(),
    tableau_internal_name = character(),
    formula               = character(),
    depends_on            = character(),
    n_calc_deps           = integer(),
    is_cyclic             = logical()
  )
}

#' Calculated fields in rebuild dependency order
#'
#' Returns every calculated field topologically sorted so that each field
#' appears *after* the calculated fields its formula depends on. Recreate the
#' fields in `build_order` sequence and every reference will already exist.
#' Fields caught in a dependency cycle cannot be ordered: they get
#' `build_order = NA` and `is_cyclic = TRUE` (plus a warning naming them).
#'
#' @details
#' A formula token is treated as a calc-on-calc dependency only when it
#' matches a calculated field *and not* a raw field or parameter, so
#' `SUM([Sales])` inside a calc captioned `"Sales"` does not create a false
#' self-loop. This is a rebuild-ordering aid, not a duplicate of
#' [twb_calc_complexity()]: that function classifies complexity and counts
#' dependencies, while this one gives the concrete creation sequence plus the
#' human-readable dependency list.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{build_order}{Integer creation sequence (`NA` for cyclic fields).}
#'   \item{datasource}{Datasource the field belongs to.}
#'   \item{name}{Human-readable field name / caption.}
#'   \item{tableau_internal_name}{Bracketed internal Tableau name.}
#'   \item{formula}{Raw formula string.}
#'   \item{depends_on}{Comma-separated names of the calculated fields this
#'     formula directly depends on (`NA` when none).}
#'   \item{n_calc_deps}{Integer count of direct calc-on-calc dependencies.}
#'   \item{is_cyclic}{Logical; `TRUE` when the field is part of a dependency
#'     cycle and could not be ordered.}
#' }
#'
#' @examples
#' twb <- system.file("extdata", "rebuild_kit.twb", package = "twbparser")
#' stopifnot(nzchar(twb), file.exists(twb))
#' xml <- xml2::read_xml(twb)
#'
#' # "Adjusted Ratio" is built after "Profit Ratio"; the Cycle A/B pair is flagged
#' twb_calc_build_order(xml)
#'
#' parser <- TwbParser$new(twb)
#' parser$get_calc_build_order()
#' parser$calc_build_order
#'
#' @seealso [twb_calc_complexity()] for complexity classification,
#'   [twb_unused_fields()] for calculations nothing references.
#'
#' @export
twb_calc_build_order <- function(x) {
  xml_doc <- .twb_resolve_xml(x)
  calcs <- extract_calculated_fields(xml_doc)
  if (nrow(calcs) == 0L) return(.empty_build_order())
  stopifnot(all(c("name", "tableau_internal_name", "formula") %in% names(calcs)))

  n <- nrow(calcs)

  # match keys for every calculated field
  calc_keys <- lapply(seq_len(n), function(i) {
    .rebuild_field_keys(calcs$tableau_internal_name[i], calcs$name[i])
  })

  # keys that belong to raw fields or parameters: tokens matching these are
  # never calc-on-calc edges (avoids false self-loops like SUM([Sales])
  # inside a calc captioned "Sales")
  raw <- tryCatch(extract_raw_fields(xml_doc), error = function(e) tibble::tibble())
  params <- tryCatch(extract_parameters(xml_doc), error = function(e) tibble::tibble())
  other_keys <- unique(unlist(lapply(
    list(raw, params),
    function(df) {
      if (is.null(df) || nrow(df) == 0L) return(character())
      unlist(lapply(seq_len(nrow(df)), function(i) {
        .rebuild_field_keys(df$tableau_internal_name[i], df$name[i])
      }))
    }
  )))
  if (is.null(other_keys)) other_keys <- character(0)

  # token -> calc index lookup
  key_to_idx <- list()
  for (i in seq_len(n)) {
    for (k in calc_keys[[i]]) key_to_idx[[k]] <- c(key_to_idx[[k]], i)
  }

  # deps[[i]]: indices of calculated fields that calc i directly depends on.
  # NOTE: assign integer(0) (never NULL) for dependency-free calcs: in R,
  # `deps[[i]] <- NULL` *deletes* the element and shortens the list.
  deps <- vector("list", n)
  for (i in seq_len(n)) {
    toks <- .rebuild_key(.extract_tokens(calcs$formula[i]))
    toks <- toks[!is.na(toks) & nzchar(toks) & !(toks %in% other_keys)]
    hit <- unique(unlist(key_to_idx[toks], use.names = FALSE))
    if (is.null(hit)) hit <- integer(0)
    deps[[i]] <- sort(hit[!is.na(hit)])
  }

  # Kahn's algorithm over the calc subgraph
  dependents <- vector("list", n)
  for (i in seq_len(n)) {
    for (j in deps[[i]]) dependents[[j]] <- c(dependents[[j]], i)
  }
  remaining <- lengths(deps)
  built <- rep(FALSE, n)
  build_order <- rep(NA_integer_, n)
  cur <- 1L
  repeat {
    ready <- which(!built & remaining == 0L)
    if (length(ready) == 0L) break
    for (i in sort(ready)) {
      build_order[i] <- cur
      cur <- cur + 1L
      built[i] <- TRUE
      for (d in dependents[[i]]) remaining[d] <- remaining[d] - 1L
    }
  }

  is_cyclic <- !built
  if (any(is_cyclic)) {
    warning(
      "Circular dependencies detected among calculated fields: ",
      paste(calcs$name[is_cyclic], collapse = ", "),
      ". `build_order` is NA for these fields; break the cycle before rebuilding.",
      call. = FALSE
    )
  }

  depends_on <- vapply(deps, function(j) {
    if (length(j) == 0L) return(NA_character_)
    paste(sort(unique(calcs$name[j])), collapse = ", ")
  }, character(1L))

  tibble::tibble(
    build_order           = build_order,
    datasource            = as.character(calcs$datasource),
    name                  = as.character(calcs$name),
    tableau_internal_name = as.character(calcs$tableau_internal_name),
    formula               = as.character(calcs$formula),
    depends_on            = depends_on,
    n_calc_deps           = as.integer(lengths(deps)),
    is_cyclic             = is_cyclic
  ) |> dplyr::arrange(.data$build_order)
}

# ---- twb_parameter_usage ------------------------------------------------------

.empty_parameter_usage <- function() {
  tibble::tibble(
    parameter     = character(),
    datasource    = character(),
    datatype      = character(),
    current_value = character(),
    context       = character(),
    location      = character()
  )
}

#' Where each parameter is consumed
#'
#' One row per parameter usage: calculated-field formulas that reference the
#' parameter, worksheet shelves and filters it appears on, and dashboard
#' filter zones bound to it. Parameters become variables when rebuilding in
#' another tool, so this is the map of everywhere each variable's value
#' flows.
#'
#' @details
#' Parameters with no usages do not appear here; find them with
#' [twb_unused_fields()]. There is no overlap with [twb_field_usage()]:
#' that function maps *fields* to worksheets, while this one maps
#' *parameters* to every consumption point including formulas.
#'
#' @param x A `TwbParser` object or an `xml2` document.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{parameter}{Human-readable parameter name / caption.}
#'   \item{datasource}{Datasource the parameter belongs to.}
#'   \item{datatype}{Parameter data type.}
#'   \item{current_value}{Current value if specified in the workbook.}
#'   \item{context}{Where it is used: `"formula"`, `"shelf:<shelf>"`
#'     (e.g. `"shelf:rows"`), `"filter"`, or `"dashboard_filter"`.}
#'   \item{location}{Calculated field name, worksheet name, or dashboard
#'     name, depending on context.}
#' }
#'
#' @examples
#' twb <- system.file("extdata", "rebuild_kit.twb", package = "twbparser")
#' stopifnot(nzchar(twb), file.exists(twb))
#' xml <- xml2::read_xml(twb)
#'
#' # "Top N" is used in the "Adjusted Ratio" formula and on a worksheet filter
#' twb_parameter_usage(xml)
#'
#' parser <- TwbParser$new(twb)
#' parser$get_parameter_usage()
#' parser$parameter_usage
#'
#' @seealso [twb_unused_fields()] for parameters nothing references,
#'   [extract_parameters()] for parameter definitions.
#'
#' @export
twb_parameter_usage <- function(x) {
  xml_doc <- .twb_resolve_xml(x)
  params <- extract_parameters(xml_doc)
  if (nrow(params) == 0L) return(.empty_parameter_usage())

  np <- nrow(params)
  pkeys <- lapply(seq_len(np), function(i) {
    .rebuild_field_keys(params$tableau_internal_name[i], params$name[i])
  })
  hits <- function(key) {
    if (is.na(key) || !nzchar(key)) return(integer())
    which(vapply(pkeys, function(k) key %in% k, logical(1L)))
  }

  rows <- list()
  add <- function(p, context, location) {
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      parameter  = as.character(params$name[p]),
      datasource = as.character(params$datasource[p]),
      context    = context,
      location   = as.character(location)
    )
  }

  # 1. calculated-field formulas
  calcs <- tryCatch(
    extract_calculated_fields(xml_doc),
    error = function(e) tibble::tibble()
  )
  if (nrow(calcs) > 0L && "formula" %in% names(calcs)) {
    for (i in seq_len(nrow(calcs))) {
      toks <- .rebuild_key(.extract_tokens(calcs$formula[i]))
      toks <- toks[!is.na(toks) & nzchar(toks)]
      for (p in unique(unlist(lapply(toks, hits)))) add(p, "formula", calcs$name[i])
    }
  }

  # 2. worksheet shelves
  sh <- tryCatch(.ins_sheet_shelves(xml_doc), error = function(e) .empty_shelves())
  if (nrow(sh) > 0L) {
    skeys <- .rebuild_key(sh$field_clean)
    for (r in seq_len(nrow(sh))) {
      for (p in hits(skeys[r])) add(p, paste0("shelf:", sh$shelf[r]), sh$sheet[r])
    }
  }

  # 3. worksheet filters
  fl <- tryCatch(.ins_sheet_filters(xml_doc), error = function(e) .empty_filters())
  if (nrow(fl) > 0L) {
    fkeys <- .rebuild_key(fl$field_clean)
    for (r in seq_len(nrow(fl))) {
      for (p in hits(fkeys[r])) add(p, "filter", fl$sheet[r])
    }
  }

  # 4. dashboard filter zones
  df <- tryCatch(
    .ins_dashboard_filters(xml_doc),
    error = function(e) .empty_dashboard_filters()
  )
  if (nrow(df) > 0L && "field" %in% names(df)) {
    dkeys <- .rebuild_key(df$field)
    for (r in seq_len(nrow(df))) {
      for (p in hits(dkeys[r])) add(p, "dashboard_filter", df$dashboard[r])
    }
  }

  if (length(rows) == 0L) return(.empty_parameter_usage())

  usage <- dplyr::bind_rows(rows) |> dplyr::distinct()
  meta <- tibble::tibble(
    parameter     = as.character(params$name),
    datasource    = as.character(params$datasource),
    datatype      = as.character(params$datatype),
    current_value = as.character(params$current_value)
  ) |>
    dplyr::distinct(.data$parameter, .data$datasource, .keep_all = TRUE)

  usage |>
    dplyr::left_join(meta, by = c("parameter", "datasource")) |>
    dplyr::select("parameter", "datasource", "datatype", "current_value",
                  "context", "location") |>
    dplyr::arrange(.data$parameter, .data$context, .data$location) |>
    tibble::as_tibble()
}
