#' Extract datasource details from a Tableau TWB
#'
#' Gathers runtime tables (from the object graph), merges in named-connection
#' metadata (class, caption, targets), and augments with top-level datasource
#' definitions (field counts, connection type, location). Also returns the
#' workbook's parameter fields via [extract_parameters()].
#'
#' @param xml_doc An `xml2` document for a Tableau `.twb`.
#'
#' @return A named list with:
#' \describe{
#'   \item{data_sources}{Tibble of datasources joined with connection metadata.}
#'   \item{parameters}{Tibble of parameter fields from [extract_parameters()].}
#'   \item{all_sources}{Same as `data_sources` (placeholder for future variants).}
#' }
#'
#' @examples
#' # Preferred: from a tiny .twb
#' twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
#' if (nzchar(twb) && file.exists(twb)) {
#'   xml <- xml2::read_xml(twb)
#'   res <- extract_datasource_details(xml)
#'   head(res$data_sources)
#' }
#'
#' @examplesIf nzchar(system.file("extdata","test_for_zip.twbx", package = "twbparser"))
#' # Alternative: from a tiny .twbx (guarded)
#' twbx <- system.file("extdata", "test_for_zip.twbx", package = "twbparser")
#' if (nzchar(twbx) && file.exists(twbx)) {
#'  members  <- twbx_list(twbx)
#'  twb_rows <- members$name[grepl("\\.twb$", members$name)]
#'  if (length(twb_rows) > 0L && !is.na(twb_rows[1])) {
#'    twb_member <- twb_rows[1]
#'    xml <- xml2::read_xml(utils::unzip(twbx, twb_member, exdir = tempdir()))
#'    res <- extract_datasource_details(xml)
#'    head(res$data_sources)
#'  }
#'}
#'
#' @export
#' @importFrom xml2 xml_find_all xml_find_first xml_attr xml_attrs
#' @importFrom dplyr left_join mutate coalesce distinct arrange desc select filter first
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr replace_na
#' @importFrom stringr str_detect str_to_title
extract_datasource_details <- function(xml_doc) {
  stopifnot(inherits(xml_doc, "xml_document"))

  # Runtime tables from object-graph (context = "")
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

  # Named connections (Athena, OGRDirect, Excel, etc.)
  conn_meta <- extract_named_connections(xml_doc)
  # Ensure expected columns exist so joins never error
  need_conn_cols <- c("connection_id", "connection_class", "connection_caption",
                      "connection_target", "location_named")
  for (nm in setdiff(need_conn_cols, names(conn_meta))) {
    conn_meta[[nm]] <- if (nm %in% c("field_count")) integer() else character()
  }
  # Keep only expected columns (prevents accidental conflicting names)
  conn_meta <- conn_meta[, intersect(names(conn_meta), need_conn_cols), drop = FALSE]

  # Top-level datasource definitions (optional)
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

      server_label <- if (!is.na(server) && nzchar(server)) server else "<unknown>"
      file_label <- function(x) {
        if (!is.na(x) && nzchar(x)) base::basename(x) else "<unknown>"
      }

      location <- dplyr::case_when(
        cls == "excel"     ~ paste0("Excel: ", file_label(filename)),
        cls == "textscan"  ~ paste0("CSV: ", file_label(filename)),
        cls == "federated" ~ paste0("Federated: ", server_label),
        TRUE               ~ "Unknown"
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

  # Ensure expected columns exist in meta
  need_meta_cols <- c("primary_table", "datasource_name", "field_count", "connection_type", "location")
  for (nm in setdiff(need_meta_cols, names(meta))) {
    meta[[nm]] <- if (nm == "field_count") integer() else character()
  }

  # Assemble final table (joins are now safe)
  final <- runtime_ds |>
    dplyr::left_join(conn_meta, by = "connection_id") |>
    dplyr::left_join(meta,      by = "primary_table") |>
    dplyr::mutate(
      # prefer named-connection location; fall back to top-level meta
      location        = dplyr::coalesce(location, location_named),
      # prefer named-connection class if top-level is empty
      connection_type = dplyr::coalesce(connection_type, connection_class),
      field_count     = tidyr::replace_na(field_count, 0L),
      # fall back to connection caption if datasource_name missing
      datasource_name = dplyr::coalesce(datasource_name, connection_caption)
    ) |>
    dplyr::select(
      datasource, primary_table, connection_id, connection_caption,
      connection_class, connection_target, datasource_name,
      field_count, connection_type, location
    )

  # Parameters: the actual parameter columns (those with @param-domain-type),
  # not datasource-level metadata. Sourcing from meta mislabelled the Parameters
  # *datasource* as a single "parameter" and dropped the real ones.
  params <- tryCatch(
    extract_parameters(xml_doc),
    error = function(e) tibble::tibble()
  )

  # Return structure expected by TwbParser getters
  list(
    data_sources = final,
    parameters   = params,
    all_sources  = final
  )
}
