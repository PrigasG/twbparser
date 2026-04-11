#' Detect references to published data sources
#'
#' Inspects datasource nodes and heuristically flags those that reference a
#' published (server-side) source rather than an embedded one.
#'
#' @param x A `TwbParser` object **or** an `xml2` document.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{name}{Internal datasource name.}
#'   \item{caption}{User-visible caption.}
#'   \item{hasconn}{Value of the `hasconnection` attribute.}
#'   \item{likely_published}{`TRUE` when `hasconnection = false` or when the
#'     node text contains published-source markers.}
#'   \item{hints}{Short explanation of the classification.}
#' }
#'
#' @examples
#' twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
#' stopifnot(nzchar(twb), file.exists(twb))
#' xml <- xml2::read_xml(twb)
#' twb_published_refs(xml)
#'
#' @importFrom rlang .data
#' @export
twb_published_refs <- function(x) {
  xml_doc <- .twb_resolve_xml(x)
  dsn <- xml2::xml_find_all(xml_doc, "//datasource")
  if (length(dsn) == 0L) {
    return(tibble::tibble(
      name             = character(),
      caption          = character(),
      likely_published = logical(),
      hints            = character()
    ))
  }
  tibble::tibble(
    name    = xml2::xml_attr(dsn, "name"),
    caption = xml2::xml_attr(dsn, "caption"),
    hasconn = xml2::xml_attr(dsn, "hasconnection"),
    raw     = vapply(dsn, xml2::xml_text, character(1L))
  ) |>
    dplyr::mutate(
      likely_published =
        .data$hasconn %in% c("false", "0") |
        stringr::str_detect(
          .data$raw,
          "(?i)published|tableau server|tableau cloud|catalog-id|content-url"
        ),
      hints = dplyr::if_else(
        .data$likely_published,
        "hasconnection=false or published markers present",
        "embedded or no published markers"
      )
    ) |>
    dplyr::select(-"raw")
}
