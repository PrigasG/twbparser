#' @keywords internal
.strip_brackets <- function(x) {
  ifelse(is.na(x), NA_character_, gsub("\\[|\\]", "", x))
}

#' @keywords internal
.clean_table <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(NA_character_)
  }
  x <- gsub("^\\[.*?\\]\\.", "", x) # drop [Extract]. / [Connection].
  x <- gsub("\\[|\\]", "", x) # strip [ ]
  x <- sub("_[0-9A-Fa-f]{32}$", "", x) # drop 32-hex suffix
  x <- trimws(x)
  if (!nzchar(x)) NA_character_ else x
}

#' Extract calculated fields from a TWB
#'
#' Finds columns that contain \verb{<calculation>} nodes and returns metadata and
#' formulas, with a heuristic flag for table calculations.
#'
#' @param xml_doc An `xml2` document for a Tableau `.twb`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{datasource}{Datasource name.}
#'   \item{name}{User-visible caption or cleaned internal name.}
#'   \item{tableau_internal_name}{Internal Tableau name (often bracketed).}
#'   \item{datatype}{Tableau datatype.}
#'   \item{role}{Tableau role.}
#'   \item{formula}{Calculation formula string.}
#'   \item{class}{Tableau calc class (often `"tableau"`).}
#'   \item{is_table_calc}{Heuristic flag for table calcs (e.g., `WINDOW_`, `LOOKUP`).}
#'   \item{table}{Raw table reference.}
#'   \item{table_clean}{Cleaned table name.}
#' }
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("inst/extdata/sample.twb")
#' calc <- extract_calculated_fields(xml)
#' }
#'
#' @export
#' @importFrom xml2 xml_find_all xml_find_first xml_attrs xml_attr
#' @importFrom tibble tibble
#' @importFrom dplyr distinct
extract_calculated_fields <- function(xml_doc) {
  ds_nodes <- xml2::xml_find_all(
    xml_doc,
    "/workbook/datasources/datasource[@name and not(ancestor::view)]"
  )
  if (length(ds_nodes) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(ds_nodes, function(ds) {
    ds_name <- xml2::xml_attr(ds, "name")
    cols <- xml2::xml_find_all(ds, ".//column[@name][.//calculation]")
    if (length(cols) == 0) {
      return(tibble::tibble())
    }

    purrr::map_dfr(cols, function(col) {
      ca <- xml2::xml_attrs(col) # column attrs
      calc <- xml2::xml_find_first(col, ".//calculation")
      fa <- if (!inherits(calc, "xml_missing")) xml2::xml_attrs(calc) else list()

      internal <- attr_safe_get(ca, "name", NA_character_)
      caption <- attr_safe_get(ca, "caption", NA_character_)
      raw_tbl <- attr_safe_get(ca, "table", NA_character_)

      formula <- attr_safe_get(fa, "formula", NA_character_)
      class <- attr_safe_get(fa, "class", NA_character_)

      # Heuristic for table calcs
      is_tbl <- if (!is.na(formula)) {
        grepl("\\b(WINDOW_|LOOKUP\\(|INDEX\\(|RUNNING_|RANK\\(|PREVIOUS_VALUE\\()", formula)
      } else {
        FALSE
      }

      tibble::tibble(
        datasource            = ds_name,
        name                  = if (!is.na(caption) && nzchar(caption)) caption else .strip_brackets(internal),
        tableau_internal_name = internal,
        datatype              = attr_safe_get(ca, "datatype", NA_character_),
        role                  = attr_safe_get(ca, "role", NA_character_),
        formula               = formula,
        class                 = class,
        is_table_calc         = is_tbl,
        table                 = raw_tbl,
        table_clean           = .clean_table(raw_tbl)
      )
    })
  }) |>
    dplyr::distinct()
}

#' Extract parameter fields from a TWB
#'
#' Returns parameter columns (those with `param-domain-type`) and basic metadata,
#' including a best-effort current value if present.
#'
#' @param xml_doc An `xml2` document for a Tableau `.twb`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{datasource}{Datasource name.}
#'   \item{name}{User-visible caption or cleaned internal name.}
#'   \item{tableau_internal_name}{Internal Tableau name.}
#'   \item{datatype}{Tableau datatype.}
#'   \item{role}{Tableau role.}
#'   \item{parameter_type}{Tableau parameter domain type.}
#'   \item{allowable_type}{Underlying data-type (if present).}
#'   \item{current_value}{Current value if specified.}
#'   \item{is_parameter}{Always `TRUE`.}
#'   \item{table}{Raw table reference.}
#'   \item{table_clean}{Cleaned table name.}
#' }
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("inst/extdata/sample.twb")
#' params <- extract_parameters(xml)
#' }
#'
#' @export
#' @importFrom xml2 xml_find_all xml_find_first xml_attrs xml_attr
#' @importFrom tibble tibble
#' @importFrom dplyr distinct
extract_parameters <- function(xml_doc) {
  ds_nodes <- xml2::xml_find_all(
    xml_doc,
    "/workbook/datasources/datasource[@name and not(ancestor::view)]"
  )
  if (length(ds_nodes) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(ds_nodes, function(ds) {
    ds_name <- xml2::xml_attr(ds, "name")
    nodes <- xml2::xml_find_all(ds, ".//column[@param-domain-type]")
    if (length(nodes) == 0) {
      return(tibble::tibble())
    }

    purrr::map_dfr(nodes, function(node) {
      a <- xml2::xml_attrs(node)

      internal <- attr_safe_get(a, "name", NA_character_)
      caption <- attr_safe_get(a, "caption", NA_character_)
      raw_tbl <- attr_safe_get(a, "table", NA_character_)

      cur <- xml2::xml_find_first(node, ".//current-value")
      cur_val <- if (!inherits(cur, "xml_missing")) xml2::xml_attr(cur, "value") else NA_character_

      tibble::tibble(
        datasource            = ds_name,
        name                  = if (!is.na(caption) && nzchar(caption)) caption else .strip_brackets(internal),
        tableau_internal_name = internal,
        datatype              = attr_safe_get(a, "datatype", NA_character_),
        role                  = attr_safe_get(a, "role", NA_character_),
        parameter_type        = attr_safe_get(a, "param-domain-type", NA_character_),
        allowable_type        = attr_safe_get(a, "data-type", NA_character_),
        current_value         = cur_val,
        is_parameter          = TRUE,
        table                 = raw_tbl,
        table_clean           = .clean_table(raw_tbl)
      )
    })
  }) |>
    dplyr::distinct()
}

#' Extract non-calculated, non-parameter fields from a TWB
#'
#' Returns raw columns excluding calculated fields and parameters.
#'
#' @param xml_doc An `xml2` document for a Tableau `.twb`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{datasource}{Datasource name.}
#'   \item{name}{User-visible caption or cleaned internal name.}
#'   \item{tableau_internal_name}{Internal Tableau name.}
#'   \item{datatype}{Tableau datatype.}
#'   \item{role}{Tableau role.}
#'   \item{is_hidden}{Whether the field is hidden.}
#'   \item{is_parameter}{Always `FALSE`.}
#'   \item{table}{Raw table reference.}
#'   \item{table_clean}{Cleaned table name.}
#' }
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("inst/extdata/sample.twb")
#' raw_fields <- extract_raw_fields(xml)
#' }
#'
#' @export
#' @importFrom xml2 xml_find_all xml_attrs xml_attr
#' @importFrom tibble tibble
#' @importFrom dplyr distinct
extract_raw_fields <- function(xml_doc) {
  ds_nodes <- xml2::xml_find_all(
    xml_doc,
    "/workbook/datasources/datasource[@name and not(ancestor::view)]"
  )
  if (length(ds_nodes) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(ds_nodes, function(ds) {
    ds_name <- xml2::xml_attr(ds, "name")
    nodes <- xml2::xml_find_all(ds, ".//column[@name and not(.//calculation) and not(@param-domain-type)]")
    if (length(nodes) == 0) {
      return(tibble::tibble())
    }

    purrr::map_dfr(nodes, function(node) {
      a <- xml2::xml_attrs(node)

      internal <- attr_safe_get(a, "name", NA_character_)
      caption <- attr_safe_get(a, "caption", NA_character_)
      raw_tbl <- attr_safe_get(a, "table", NA_character_)

      tibble::tibble(
        datasource            = ds_name,
        name                  = if (!is.na(caption) && nzchar(caption)) caption else .strip_brackets(internal),
        tableau_internal_name = internal,
        datatype              = attr_safe_get(a, "datatype", NA_character_),
        role                  = attr_safe_get(a, "role", NA_character_),
        is_hidden             = attr_safe_get(a, "hidden", "false") %in% c("true", "1"),
        is_parameter          = FALSE,
        table                 = raw_tbl,
        table_clean           = .clean_table(raw_tbl)
      )
    })
  }) |>
    dplyr::distinct()
}
