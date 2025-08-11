#' @keywords internal
.j_clean_table <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(NA_character_)
  }
  x <- gsub("^\\[.*?\\]\\.", "", x) # remove [Extract]. / [Connection].
  x <- gsub("\\[|\\]", "", x) # remove brackets
  x <- sub("_[0-9A-Fa-f]{32}$", "", x) # drop 32-hex suffix
  x <- trimws(x)
  if (!nzchar(x)) NA_character_ else x
}

#' @keywords internal
.j_clean_field <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(NA_character_)
  }
  x <- gsub("\\[|\\]", "", x) # strip brackets
  x <- sub("^([^:]+:)+", "", x) # drop column-instance prefixes (e.g., none:, clct:)
  parts <- strsplit(x, "\\.", fixed = FALSE)[[1]]
  parts <- parts[nzchar(parts)]
  if (!length(parts)) {
    return(NA_character_)
  }
  tail(parts, 1)
}

#' @keywords internal
.j_field_from_expr <- function(node) {
  if (inherits(node, "xml_missing")) {
    return(NA_character_)
  }
  cand <- c(
    xml2::xml_attr(node, "op"),
    xml2::xml_attr(node, "field"),
    xml2::xml_attr(node, "value")
  )
  cand <- cand[!is.na(cand) & nzchar(cand)]
  if (!length(cand)) {
    return(NA_character_)
  }
  raw <- cand[which.max(nchar(cand))]
  m <- stringr::str_extract_all(raw, "\\[[^\\]]+\\]")[[1]]
  token <- if (length(m)) m[length(m)] else raw
  .j_clean_field(token)
}

#' Extract Tableau join clauses from `<relation type="join">` nodes
#'
#' Handles both column-based clauses (`<clause><column/></clause>`) and
#' expression-based predicates (`<expression op=...>`) found in TWB XML.
#'
#' @param xml_doc An `xml2` document for a Tableau `.twb`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{join_type}{Join kind (e.g., `inner`, `left`), if available.}
#'   \item{left_table}{Left table name (cleaned).}
#'   \item{left_field}{Left field name.}
#'   \item{operator}{Predicate operator (defaults to `"="` when missing).}
#'   \item{right_table}{Right table name (cleaned).}
#'   \item{right_field}{Right field name.}
#' }
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("inst/extdata/sample.twb")
#' extract_joins(xml)
#' }
#'
#' @export
#' @importFrom xml2 xml_find_all xml_find_first xml_attrs xml_attr
#' @importFrom dplyr filter mutate distinct bind_rows transmute
#' @importFrom tibble tibble
#' @importFrom purrr map_dfr
#' @importFrom stringr str_extract_all
extract_joins <- function(xml_doc) {
  join_nodes <- xml2::xml_find_all(xml_doc, ".//relation[@type='join']")
  if (length(join_nodes) == 0) {
    return(tibble::tibble(
      join_type   = character(),
      left_table  = character(),
      left_field  = character(),
      operator    = character(),
      right_table = character(),
      right_field = character()
    ))
  }

  purrr::map_dfr(join_nodes, function(join_node) {
    join_attrs <- xml2::xml_attrs(join_node)
    join_type <- attr_safe_get(join_attrs, "join", NA_character_)

    # 1) Preferred: clause/column style
    clauses <- xml2::xml_find_all(join_node, ".//clause")
    rows1 <- purrr::map_dfr(clauses, function(cl) {
      op <- attr_safe_get(xml2::xml_attrs(cl), "op", "=")
      cols <- xml2::xml_find_all(cl, ".//column")
      if (length(cols) == 2) {
        left_tbl <- .j_clean_table(attr_safe_get(xml2::xml_attrs(cols[[1]]), "table"))
        left_fld <- .j_clean_field(attr_safe_get(xml2::xml_attrs(cols[[1]]), "name"))
        right_tbl <- .j_clean_table(attr_safe_get(xml2::xml_attrs(cols[[2]]), "table"))
        right_fld <- .j_clean_field(attr_safe_get(xml2::xml_attrs(cols[[2]]), "name"))

        tibble::tibble(
          join_type   = join_type,
          left_table  = left_tbl,
          left_field  = left_fld,
          operator    = op,
          right_table = right_tbl,
          right_field = right_fld
        )
      } else {
        tibble::tibble()
      }
    })

    # 2) Fallback: expression-based join conditions (binary expressions)
    exprs <- xml2::xml_find_all(join_node, ".//expression[@op]")
    rows2 <- purrr::map_dfr(exprs, function(en) {
      kids <- xml2::xml_find_all(en, "./expression")
      if (length(kids) != 2) {
        return(tibble::tibble())
      }
      op <- xml2::xml_attr(en, "op") %||% "="
      lf <- .j_field_from_expr(kids[[1]])
      rf <- .j_field_from_expr(kids[[2]])
      if (is.na(lf) || is.na(rf)) {
        return(tibble::tibble())
      }

      # Optional table hints on child expressions (rare)
      lt <- .j_clean_table(xml2::xml_attr(kids[[1]], "table"))
      rt <- .j_clean_table(xml2::xml_attr(kids[[2]], "table"))

      tibble::tibble(
        join_type   = join_type,
        left_table  = lt,
        left_field  = lf,
        operator    = op,
        right_table = rt,
        right_field = rf
      )
    })

    dplyr::bind_rows(rows1, rows2)
  }) |>
    dplyr::filter(
      !is.na(left_field),
      !is.na(right_field),
      left_field != "",
      right_field != ""
    ) |>
    dplyr::mutate(operator = dplyr::coalesce(operator, "=")) |>
    dplyr::distinct()
}
