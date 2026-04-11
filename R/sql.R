#' Extract Custom SQL relations from a Tableau workbook
#'
#' Finds every `<relation formula="...">` node that looks like a SQL statement
#' and returns its name, type, raw SQL text, and a flag for whether it starts
#' with `SELECT` or `WITH`.
#'
#' @param x A `TwbParser` object **or** an `xml2` document.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{relation_name}{Name attribute of the relation node.}
#'   \item{relation_type}{Type attribute (e.g. `"text"`, `"table"`).}
#'   \item{custom_sql}{Full SQL text.}
#'   \item{is_custom_sql}{`TRUE` when the text begins with `SELECT` or `WITH`.}
#' }
#'
#' @examples
#' twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
#' stopifnot(nzchar(twb), file.exists(twb))
#' xml <- xml2::read_xml(twb)
#' twb_custom_sql(xml)
#'
#' @importFrom rlang .data
#' @export
twb_custom_sql <- function(x) {
  xml_doc <- .twb_resolve_xml(x)
  rels <- xml2::xml_find_all(xml_doc, "//relation[@formula]")
  tibble::tibble(
    relation_name = xml2::xml_attr(rels, "name"),
    relation_type = xml2::xml_attr(rels, "type"),
    custom_sql    = xml2::xml_attr(rels, "formula")
  ) |>
    dplyr::filter(!is.na(.data$custom_sql)) |>
    dplyr::mutate(
      is_custom_sql = dplyr::coalesce(
        stringr::str_detect(
          .data$custom_sql,
          stringr::regex("^\\s*(select|with)\\b", ignore_case = TRUE)
        ),
        FALSE
      )
    )
}

#' Extract Initial SQL statements from Tableau connections
#'
#' Returns any `<initial-sql>` nodes found inside connection or
#' named-connection elements.
#'
#' @param x A `TwbParser` object **or** an `xml2` document.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{connection_id}{Name or caption of the parent connection element.}
#'   \item{initial_sql}{SQL text of the initial statement.}
#' }
#'
#' @examples
#' twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
#' stopifnot(nzchar(twb), file.exists(twb))
#' xml <- xml2::read_xml(twb)
#' twb_initial_sql(xml)
#'
#' @export
twb_initial_sql <- function(x) {
  xml_doc <- .twb_resolve_xml(x)
  nodes <- xml2::xml_find_all(
    xml_doc,
    "//connection/initial-sql | //named-connection/initial-sql"
  )
  if (length(nodes) == 0L) {
    return(tibble::tibble(connection_id = character(), initial_sql = character()))
  }
  tibble::tibble(
    connection_id = xml2::xml_attr(xml2::xml_parent(nodes), "name") %||%
      xml2::xml_attr(xml2::xml_parent(nodes), "caption"),
    initial_sql   = xml2::xml_text(nodes)
  )
}
