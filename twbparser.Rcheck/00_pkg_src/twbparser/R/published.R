#' Detect likely references to published data sources (vs embedded)
#' @importFrom rlang .data
#' @param xml_doc An xml2 document for a .twb
#' @return tibble with datasource name, caption, likely_published, hints
#' @export
twb_published_refs <- function(xml_doc) {
  stopifnot(inherits(xml_doc, "xml_document"))
  dsn <- xml2::xml_find_all(xml_doc, "//datasource")
  if (length(dsn) == 0) {
    return(tibble::tibble(
      name = character(), caption = character(),
      likely_published = logical(), hints = character()
    ))
  }
  tibble::tibble(
    name    = xml2::xml_attr(dsn, "name"),
    caption = xml2::xml_attr(dsn, "caption"),
    hasconn = xml2::xml_attr(dsn, "hasconnection"),
    raw     = vapply(dsn, xml2::xml_text, character(1))
  ) |>
    dplyr::mutate(
      likely_published =
        .data$hasconn %in% c("false", "0") |
        stringr::str_detect(.data$raw,
                            "(?i)published|tableau server|tableau cloud|catalog-id|content-url"
        ),
      hints = dplyr::if_else(
        .data$likely_published,
        "hasconnection=false or published markers present",
        "embedded or no published markers"
      )
    ) |>
    dplyr::select(-.data$raw)
}
