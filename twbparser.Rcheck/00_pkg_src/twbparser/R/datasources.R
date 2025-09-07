#' Extract datasource details from a Tableau TWB
#'
#' Gathers runtime tables (from the object graph), merges in named connection
#' metadata (class, caption, targets), and augments with top-level datasource
#' definitions (field counts, connection type, location). Also returns a
#' filtered table of parameter datasources.
#'
#' @param xml_doc An `xml2` document for a Tableau `.twb`.
#'
#' @return A named list with:
#' \describe{
#'   \item{data_sources}{Tibble of datasources joined with connection metadata.}
#'   \item{parameters}{Tibble of parameter datasources (if present).}
#'   \item{all_sources}{Same as `data_sources` (placeholder for future variants).}
#' }
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("inst/extdata/sample.twb")
#' out <- extract_datasource_details(xml)
#' out$data_sources
#' }
#'
#' @export
#' @importFrom xml2 xml_find_all xml_find_first xml_attr xml_attrs
#' @importFrom dplyr left_join mutate coalesce distinct arrange desc select filter first
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr replace_na
#' @importFrom stringr str_detect str_to_title
extract_datasource_details <- function(xml_doc) {
  # 1) Runtime tables from object-graph (context = "")
  rels <- xml2::xml_find_all(
    xml_doc,
    "//*[contains(local-name(), 'object-graph')]//object//properties[@context='']/relation[@type='table']"
  )

  runtime_ds <- if (length(rels)) {
    tibble::tibble(
      datasource    = xml2::xml_attr(rels, "name"),
      primary_table = xml2::xml_attr(rels, "table"),
      connection_id = xml2::xml_attr(rels, "connection")
    ) |>
      dplyr::distinct()
  } else {
    tibble::tibble(
      datasource    = character(),
      primary_table = character(),
      connection_id = character()
    )
  }

  # 2) Named connections (Athena, OGRDirect, Excel, etc.)
  #    Provided by utils.R::extract_named_connections()
  conn_meta <- extract_named_connections(xml_doc)

  # 3) Top-level datasource definitions (optional)
  defs <- xml2::xml_find_all(xml_doc, "/workbook/datasources/datasource[@name and not(ancestor::view)]")

  meta <- if (length(defs)) {
    purrr::map_dfr(defs, function(ds) {
      nm <- xml2::xml_attr(ds, "name")
      ncol <- length(xml2::xml_find_all(ds, ".//column"))

      tbl <- xml2::xml_find_first(ds, ".//relation[@type='table']")
      pt <- xml2::xml_attr(tbl, "table") %||% NA_character_

      conn <- xml2::xml_find_first(ds, ".//connection")
      a <- if (!inherits(conn, "xml_missing") && length(conn)) xml2::xml_attrs(conn) else list()

      cls <- attr_safe_get(a, "class", "inline")
      server <- attr_safe_get(a, "server", NA_character_)
      filename <- attr_safe_get(a, "filename", NA_character_)

      location <- dplyr::case_when(
        cls == "excel" ~ paste0("Excel: ", base::basename(filename %||% "")),
        cls == "textscan" ~ paste0("CSV: ", base::basename(filename %||% "")),
        cls == "federated" ~ paste0("Federated: ", server %||% "<unknown>"),
        TRUE ~ "Unknown"
      )

      tibble::tibble(
        datasource_name = nm,
        primary_table   = pt,
        field_count     = ncol,
        connection_type = cls,
        location        = location
      )
    })
  } else {
    tibble::tibble(
      datasource_name = character(),
      primary_table   = character(),
      field_count     = integer(),
      connection_type = character(),
      location        = character()
    )
  }

  # 4) Assemble final table
  final <- runtime_ds |>
    dplyr::left_join(conn_meta, by = "connection_id") |> # + connection_class, location_named, etc.
    dplyr::left_join(meta, by = "primary_table") |>
    dplyr::mutate(
      # prefer named-connection location; fall back to top-level meta
      location = dplyr::coalesce(location, location_named),
      # prefer named-connection class if top-level is empty
      connection_type = dplyr::coalesce(connection_type, connection_class),
      field_count = tidyr::replace_na(field_count, 0L),
      # fall back to connection caption if datasource_name missing
      datasource_name = dplyr::coalesce(datasource_name, connection_caption)
    ) |>
    dplyr::select(
      datasource, primary_table, connection_id, connection_caption,
      connection_class, connection_target, datasource_name,
      field_count, connection_type, location
    )

  # 5) Parameters table (from meta)
  params <- meta |> dplyr::filter(stringr::str_detect(datasource_name, "^Parameters?$"))

  # 6) Return structure expected by TwbParser getters
  list(
    data_sources = final,
    parameters   = params,
    all_sources  = final
  )
}
