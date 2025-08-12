#' @keywords internal
clean_name <- function(name) {
  name |>
    stringr::str_remove("^\\[.*?\\]\\.") |> # remove leading [Extract]. or [Connection].
    stringr::str_remove("_[:xdigit:]{32}$") |> # drop trailing _32hex suffix
    stringr::str_remove_all("\\[|\\]") |> # strip all brackets
    stringr::str_trim()
}

#' @keywords internal
.extract_field_from_expr <- function(x) {
  if (inherits(x, "xml_missing")) {
    return(NA_character_)
  }
  cand <- c(xml2::xml_attr(x, "op"), xml2::xml_attr(x, "field"), xml2::xml_attr(x, "value"))
  cand <- cand[!is.na(cand) & nzchar(cand)]
  if (!length(cand)) {
    return(NA_character_)
  }
  raw <- cand[which.max(nchar(cand))]
  m <- stringr::str_extract_all(raw, "\\[[^\\]]+\\]")[[1]]
  token <- if (length(m)) m[length(m)] else raw
  token <- gsub("\\[|\\]", "", token)
  token <- sub("^([^:]+:)+", "", token)
  parts <- unlist(strsplit(token, "\\."), use.names = FALSE)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) {
    return(NA_character_)
  }
  tail(parts, 1)
}

#' Extract all \verb{<relation>} tags from a TWB
#'
#' Returns a tibble of \verb{<relation>} elements found in a Tableau TWB XML,
#' with key attributes and any custom SQL text.
#'
#' @param xml_doc An `xml2` document for a Tableau `.twb`.
#'
#' @return A tibble with columns:
#'   \item{name}{Relation name}
#'   \item{table}{Table reference}
#'   \item{connection}{Connection ID}
#'   \item{type}{Relation type (table, join, etc.)}
#'   \item{join}{Join type if applicable}
#'   \item{custom_sql}{Inline SQL text if present}
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("inst/extdata/sample.twb")
#' extract_relations(xml)
#' }
#'
#' @export
extract_relations <- function(xml_doc) {
  nodes <- xml2::xml_find_all(xml_doc, ".//relation")
  if (length(nodes) == 0) {
    return(tibble::tibble())
  }
  purrr::map_dfr(nodes, function(node) {
    attrs <- xml2::xml_attrs(node)
    tibble::tibble(
      name       = attr_safe_get(attrs, "name"),
      table      = attr_safe_get(attrs, "table"),
      connection = attr_safe_get(attrs, "connection"),
      type       = attr_safe_get(attrs, "type"),
      join       = attr_safe_get(attrs, "join"),
      custom_sql = xml2::xml_text(node) %||% NA_character_
    )
  }) |>
    dplyr::distinct()
}

#' @keywords internal
.rel_clean_table <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(NA_character_)
  }
  x <- gsub("^\\[.*?\\]\\.", "", x)
  x <- gsub("\\[|\\]", "", x)
  x <- sub("_[0-9A-Fa-f]{32}$", "", x)
  trimws(x)
}

#' @keywords internal
.rel_field_expr <- function(node) {
  if (inherits(node, "xml_missing")) {
    return(NA_character_)
  }
  vals <- c(
    unname(xml2::xml_attrs(node)),
    xml2::xml_attr(node, "op"),
    xml2::xml_attr(node, "value"),
    xml2::xml_attr(xml2::xml_find_all(node, ".//expression"), "op"),
    xml2::xml_attr(xml2::xml_find_all(node, ".//expression"), "value"),
    xml2::xml_attr(xml2::xml_find_all(node, ".//calculation"), "formula")
  )
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (!length(vals)) {
    return(NA_character_)
  }
  br <- unlist(stringr::str_extract_all(vals, "\\[[^\\]]+\\]"))
  if (length(br)) {
    clean <- gsub("^\\[|\\]$", "", br)
    idx_fun <- grep("^[A-Za-z_][A-Za-z0-9_]*\\s*\\(", clean, perl = TRUE)
    if (length(idx_fun)) {
      return(clean[tail(idx_fun, 1)])
    }
    return(tail(clean, 1))
  }
  fn <- unlist(stringr::str_extract_all(vals, "[A-Za-z_][A-Za-z0-9_]*\\([^)]*\\)"))
  if (length(fn)) {
    return(tail(fn, 1))
  }
  NA_character_
}

#' @keywords internal
build_object_table_mapping <- function(xml_doc) {
  mapping <- list()
  objs <- xml2::xml_find_all(xml_doc, "//*[contains(local-name(), 'object-graph')]//object[@id]")
  if (length(objs)) {
    ids <- xml2::xml_attr(objs, "id")
    caps <- xml2::xml_attr(objs, "caption")
    for (i in seq_along(ids)) {
      if (!is.na(ids[i]) && !is.na(caps[i])) {
        mapping[[ids[i]]] <- .rel_clean_table(caps[i])
      }
    }
  }
  for (lt in xml2::xml_find_all(xml_doc, "//logical-table[@id]")) {
    id <- xml2::xml_attr(lt, "id")
    nm <- xml2::xml_attr(lt, "name")
    if (!is.na(id) && !is.na(nm)) mapping[[id]] <- .rel_clean_table(nm)
  }
  for (rel in xml2::xml_find_all(xml_doc, "//relation[@name]")) {
    nm <- xml2::xml_attr(rel, "name")
    tb <- xml2::xml_attr(rel, "table")
    if (!is.na(nm)) mapping[[nm]] <- .rel_clean_table(tb %||% nm)
  }
  mapping
}

#' Extract modern relationships from a Tableau TWB
#'
#' Parses Tableau "relationships" (introduced in 2020.2) between logical tables,
#' including the join predicate fields and operator.
#'
#' @param xml_doc An `xml2` document for a Tableau `.twb`.
#'
#' @return A tibble with columns:
#'   \item{relationship_type}{Always "Relationship"}
#'   \item{left_table}{Left table name}
#'   \item{right_table}{Right table name}
#'   \item{left_field}{Field name on left side}
#'   \item{operator}{Join operator (e.g., "=")}
#'   \item{right_field}{Field name on right side}
#'   \item{left_is_calc}{Logical, whether left field is a calculation}
#'   \item{right_is_calc}{Logical, whether right field is a calculation}
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("inst/extdata/sample.twb")
#' extract_relationships(xml)
#' }
#'
#' @export
extract_relationships <- function(xml_doc) {
  rel_nodes <- xml2::xml_find_all(xml_doc, "//relationships/relationship")
  if (length(rel_nodes) == 0) {
    return(tibble::tibble())
  }
  id_map <- build_object_table_mapping(xml_doc)
  purrr::map_dfr(rel_nodes, function(rel_node) {
    e1 <- xml2::xml_attr(xml2::xml_find_first(rel_node, ".//first-end-point"), "object-id")
    e2 <- xml2::xml_attr(xml2::xml_find_first(rel_node, ".//second-end-point"), "object-id")
    left_table <- .rel_clean_table(id_map[[e1]] %||% e1)
    right_table <- .rel_clean_table(id_map[[e2]] %||% e2)
    ex <- xml2::xml_find_first(rel_node, ".//expression[@op][count(./expression) >= 2]")
    if (inherits(ex, "xml_missing")) {
      return(tibble::tibble())
    }
    op <- xml2::xml_attr(ex, "op") %||% "="
    lhs <- xml2::xml_find_first(ex, "./expression[1]")
    rhs <- xml2::xml_find_first(ex, "./expression[2]")
    left_field <- .rel_field_expr(lhs)
    right_field <- .rel_field_expr(rhs)
    tibble::tibble(
      relationship_type = "Relationship",
      left_table = left_table,
      right_table = right_table,
      left_field = left_field,
      operator = op,
      right_field = right_field,
      left_is_calc = grepl("^[A-Za-z_][A-Za-z0-9_]*\\s*\\(", left_field %||% "", perl = TRUE),
      right_is_calc = grepl("^[A-Za-z_][A-Za-z0-9_]*\\s*\\(", right_field %||% "", perl = TRUE)
    )
  }) |>
    dplyr::filter(
      !is.na(left_field), !is.na(right_field),
      left_field != "", right_field != ""
    ) |>
    dplyr::distinct()
}
