#' @keywords internal
.f_clean_table <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(NA_character_)
  }
  x <- gsub("^\\[.*?\\]\\.", "", x) # remove [Extract]. or [Connection].
  x <- gsub("\\[|\\]", "", x) # strip brackets
  x <- sub("_[0-9A-Fa-f]{32}$", "", x) # drop trailing _32hex
  x <- trimws(x)
  if (!nzchar(x)) NA_character_ else x
}

#' @keywords internal
.f_clean_field <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(NA_character_)
  }
  x <- gsub("\\[|\\]", "", x) # strip brackets
  x <- sub("^([^:]+:)+", "", x) # drop "none:" / "clct:" prefixes
  parts <- unlist(strsplit(x, "\\.", fixed = FALSE), use.names = FALSE)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) {
    return(NA_character_)
  }
  tail(parts, 1)
}

#' Extract columns with their source tables from a TWB
#'
#' Scans top-level \verb{<datasource>} nodes (excluding view-specific references) and
#' returns fields with raw names/captions, cleaned table/field names, and basic
#' metadata.
#'
#' @param xml_doc An `xml2` document for a Tableau `.twb`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{datasource}{Datasource name.}
#'   \item{name}{Raw column name (may include brackets/qualifiers).}
#'   \item{caption}{Column caption if present.}
#'   \item{datatype}{Tableau datatype.}
#'   \item{role}{Tableau role.}
#'   \item{semantic_role}{Semantic role if present.}
#'   \item{table}{Raw table reference.}
#'   \item{table_clean}{Cleaned table name (no brackets/suffix).}
#'   \item{field_clean}{Cleaned field name.}
# %   \item{is_parameter}{Logical, whether column belongs to a parameter set.}
#' }
#'
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("inst/extdata/sample.twb")
#' fields <- extract_columns_with_table_source(xml)
#' }
#'
#' @export
#' @importFrom xml2 xml_find_all xml_attr xml_attrs
#' @importFrom tibble tibble
#' @importFrom dplyr distinct
extract_columns_with_table_source <- function(xml_doc) {
  ds_nodes <- xml2::xml_find_all(
    xml_doc,
    "/workbook/datasources/datasource[@name and not(ancestor::view)]"
  )
  if (length(ds_nodes) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(ds_nodes, function(ds_node) {
    ds_name <- xml2::xml_attr(ds_node, "name")
    cols <- xml2::xml_find_all(ds_node, ".//column[@name]")
    if (length(cols) == 0) {
      return(tibble::tibble())
    }

    purrr::map_dfr(cols, function(col) {
      a <- xml2::xml_attrs(col)

      raw_table <- attr_safe_get(a, "table", NA_character_)
      raw_name <- attr_safe_get(a, "name", NA_character_)
      cap <- attr_safe_get(a, "caption", NA_character_)

      # compute field_clean
      nn <- gsub("\\[|\\]", "", raw_name)
      nn <- sub("^([^:]+:)+", "", nn)
      parts <- strsplit(nn, "\\.", perl = TRUE)[[1]]
      field_clean <- if (length(parts)) tail(parts, 1) else nn

      # compute table_clean
      table_clean <- raw_table |>
        gsub("^\\[.*?\\]\\.", "", x = _) |>
        gsub("\\[|\\]", "", x = _) |>
        gsub("_[0-9A-Fa-f]{32}$", "", x = _) |>
        trimws()

      tibble::tibble(
        datasource     = ds_name,
        name           = raw_name,
        caption        = cap,
        datatype       = attr_safe_get(a, "datatype", NA_character_),
        role           = attr_safe_get(a, "role", NA_character_),
        semantic_role  = attr_safe_get(a, "semantic-role", NA_character_),
        table          = gsub("_[0-9A-Fa-f]{32}$", "", raw_table),
        table_clean    = ifelse(table_clean == "", NA_character_, table_clean),
        field_clean    = ifelse(field_clean == "", NA_character_, field_clean),
        is_parameter   = !is.na(attr_safe_get(a, "param-domain-type", NA_character_))
      )
    })
  }) |>
    dplyr::distinct()
}

#' Infer implicit relationships between tables from field metadata
#'
#' Generates candidate join pairs by:
#' * Matching `semantic_role` across different tables.
#' * Matching field names (case-insensitive) across different tables.
#'
#' @param fields_df A data frame like the output of
#'   [extract_columns_with_table_source()].
#' @param max_pairs Maximum number of candidate pairs to return (default 50,000).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{left_table}{Left table name.}
#'   \item{left_field}{Left field name.}
#'   \item{right_table}{Right table name.}
#'   \item{right_field}{Right field name.}
#'   \item{reason}{Why the pair was suggested.}
#' }
#'
#' @examples
#' \dontrun{
#' fields <- extract_columns_with_table_source(xml)
#' infer_implicit_relationships(fields)
#' }
#'
#' @export
#' @importFrom dplyr mutate filter transmute distinct rowwise ungroup select inner_join rename coalesce na_if
#' @importFrom tibble tibble
infer_implicit_relationships <- function(fields_df, max_pairs = 50000L) {
  empty <- tibble::tibble(
    left_table  = character(), left_field = character(),
    right_table = character(), right_field = character(),
    reason      = character()
  )
  if (is.null(fields_df) || nrow(fields_df) == 0) {
    return(empty)
  }

  # ensure columns exist + correct types
  if (!"is_parameter" %in% names(fields_df)) fields_df$is_parameter <- FALSE
  for (nm in c("table_clean", "field_clean", "table", "name", "semantic_role")) {
    if (!nm %in% names(fields_df)) fields_df[[nm]] <- NA_character_
  }

  # --- vectorized cleaners ---
  vec_clean_table <- function(x) {
    x <- as.character(x)
    x <- gsub("^\\[.*?\\]\\.", "", x)
    x <- gsub("\\[|\\]", "", x)
    x <- gsub("_[0-9A-Fa-f]{32}$", "", x)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
  }
  vec_clean_field <- function(x) {
    x <- as.character(x)
    x <- gsub("\\[|\\]", "", x)
    x <- sub("^([^:]+:)+", "", x)
    parts <- strsplit(x, "\\.", perl = TRUE)
    vapply(parts, function(p) {
      p <- p[nzchar(p)]
      if (!length(p)) NA_character_ else tail(p, 1)
    }, character(1))
  }

  fields_df <- fields_df |>
    dplyr::mutate(
      table_clean = as.character(table_clean),
      field_clean = as.character(field_clean),
      table       = as.character(table),
      name        = as.character(name),
      table_use   = dplyr::coalesce(dplyr::na_if(table_clean, ""), vec_clean_table(table)),
      field_use   = dplyr::coalesce(dplyr::na_if(field_clean, ""), vec_clean_field(name))
    )

  f <- fields_df |>
    dplyr::filter(!is_parameter) |>
    dplyr::filter(
      !is.na(table_use), !is.na(field_use),
      table_use != "", field_use != ""
    ) |>
    dplyr::transmute(
      table = table_use,
      field = field_use,
      semantic_role = semantic_role
    ) |>
    dplyr::distinct()

  if (nrow(f) == 0) {
    return(empty)
  }

  f_role <- f |>
    dplyr::filter(!is.na(semantic_role) & semantic_role != "") |>
    dplyr::rename(role = semantic_role)

  by_role <- dplyr::inner_join(
    f_role, f_role,
    by = "role", suffix = c("_l", "_r")
  ) |>
    dplyr::filter(table_l != table_r) |>
    dplyr::transmute(
      left_table  = table_l,
      left_field  = field_l,
      right_table = table_r,
      right_field = field_r,
      reason      = "matched semantic-role"
    )


  f2 <- f |>
    dplyr::mutate(field_lower = tolower(field))

  by_name <- f2 |>
    dplyr::inner_join(f2, by = "field_lower", suffix = c("_l", "_r")) |>
    dplyr::filter(table_l != table_r) |>
    dplyr::transmute(
      left_table  = table_l,
      left_field  = field_l,
      right_table = table_r,
      right_field = field_r,
      reason      = "matched field name"
    )

  out <- dplyr::bind_rows(by_role, by_name) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      key = paste(sort(c(
        paste0(left_table, ".", left_field),
        paste0(right_table, ".", right_field)
      )), collapse = "||")
    ) |>
    dplyr::ungroup() |>
    dplyr::distinct(key, .keep_all = TRUE) |>
    dplyr::select(-key)

  if (nrow(out) > max_pairs) {
    out <- utils::head(out, max_pairs)
    warning("infer_implicit_relationships(): capped at max_pairs = ", max_pairs)
  }
  out
}
