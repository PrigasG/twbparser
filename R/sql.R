#' Extract Custom SQL relations from a TWB XML
#' @importFrom rlang .data
#' @param xml_doc An xml2 document for a .twb
#' @return tibble with relation_name, relation_type, custom_sql
#' @export
twb_custom_sql <- function(xml_doc) {
  stopifnot(inherits(xml_doc, "xml_document"))
  rels <- xml2::xml_find_all(xml_doc, "//relation[@formula]")
  tibble::tibble(
    relation_name = xml2::xml_attr(rels, "name"),
    relation_type = xml2::xml_attr(rels, "type"),
    custom_sql    = xml2::xml_attr(rels, "formula")
  ) |>
    dplyr::filter(!is.na(.data$custom_sql)) |>
    dplyr::mutate(
      is_custom_sql = dplyr::coalesce(
        stringr::str_detect(.data$custom_sql,
                            stringr::regex("^\\s*(select|with)\\b", ignore_case = TRUE)
        ),
        FALSE
      )
    )
}

#' Extract Initial SQL statements from connections (if present)
#' @param xml_doc An xml2 document for a .twb
#' @return tibble with connection_id, initial_sql
#' @export
twb_initial_sql <- function(xml_doc) {
  stopifnot(inherits(xml_doc, "xml_document"))
  nodes <- xml2::xml_find_all(xml_doc, "//connection/initial-sql | //named-connection/initial-sql")
  if (length(nodes) == 0) {
    return(tibble::tibble(connection_id = character(), initial_sql = character()))
  }
  tibble::tibble(
    connection_id = xml2::xml_attr(xml2::xml_parent(nodes), "name") %||%
      xml2::xml_attr(xml2::xml_parent(nodes), "caption"),
    initial_sql   = xml2::xml_text(nodes)
  )
}
