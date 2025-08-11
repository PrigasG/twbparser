#' Validate relationships against available datasources and fields
#'
#' Checks that relationship endpoints reference known datasource tables and that
#' the predicate fields appear somewhere in the workbook (calculated, raw, or
#' parameter fields), using a lenient token match (e.g., `INT([GEOID])` → `GEOID`).
#'
#' @param parser A `TwbParser`-like object that exposes:
#'   `get_relationships()`, `get_datasources()`, `get_fields()`,
#'   and `get_calculated_fields()`. (S3/R6 both fine.)
#' @param strict Logical. Reserved for future table‑scoped checks that can be
#'   overly conservative with federated sources. Currently not used.
#'
#' @return A list with:
#' \describe{
#'   \item{ok}{`TRUE` if no issues; `FALSE` otherwise.}
#'   \item{issues}{A named list of tibbles. Possible elements:
#'     \itemize{
#'       \item `unknown_tables`: endpoints not found among known tables.
#'       \item `unknown_fields`: predicate fields not found in the field pool.
#'     }
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' res <- validate_relationships(parser)
#' if (!res$ok) print(res$issues)
#' }
#'
#' @export
#' @importFrom dplyr filter mutate select
validate_relationships <- function(parser, strict = FALSE) {
  rels <- parser$get_relationships()
  ds <- parser$get_datasources()
  flds <- parser$get_fields()
  calcs <- parser$get_calculated_fields()

  if (nrow(rels) == 0) {
    return(list(ok = TRUE, issues = list()))
  }

  # Known logical endpoints from get_datasources()
  known_tables <- unique(ds$datasource)

  # Lenient cleaners for field tokens -----------------------------------------
  clean <- function(x) {
    x <- as.character(x)
    x <- gsub("\\[|\\]", "", x)
    x <- trimws(x)
    x[nzchar(x)]
  }
  base_token <- function(x) {
    # e.g., INT(GEOID) -> GEOID; UPPER(schema.field) -> field
    x <- as.character(x)
    inside <- sub("^[A-Za-z_][A-Za-z0-9_]*\\((.*)\\)$", "\\1", x)
    inside <- gsub("\\[|\\]", "", inside)
    inside <- trimws(inside)
    parts <- strsplit(inside, "\\.", perl = TRUE)
    vapply(
      parts,
      function(p) {
        p <- p[nzchar(p)]
        if (length(p)) utils::tail(p, 1) else inside[1]
      },
      character(1)
    )
  }

  # Workbook-wide field pool (lenient)
  field_pool <- unique(tolower(c(
    clean(flds$field_clean), clean(flds$name), clean(flds$caption),
    clean(calcs$name), clean(calcs$tableau_internal_name)
  )))
  field_pool <- field_pool[nzchar(field_pool)]

  # 1) Unknown endpoint tables -------------------------------------------------
  unknown_tables <- rels |>
    dplyr::filter(!left_table %in% known_tables | !right_table %in% known_tables)

  # 2) Unknown fields (lenient: token OR base token must appear) ---------------
  rels2 <- rels |>
    dplyr::mutate(
      left_tok   = tolower(clean(left_field)),
      right_tok  = tolower(clean(right_field)),
      left_base  = tolower(base_token(left_field)),
      right_base = tolower(base_token(right_field))
    )

  unknown_fields <- rels2 |>
    dplyr::mutate(
      left_ok  = (left_tok %in% field_pool) | (left_base %in% field_pool),
      right_ok = (right_tok %in% field_pool) | (right_base %in% field_pool)
    ) |>
    dplyr::filter(!left_ok | !right_ok) |>
    dplyr::select(left_table, left_field, right_table, right_field, left_ok, right_ok)

  issues <- list()
  if (nrow(unknown_tables)) issues$unknown_tables <- unknown_tables
  if (nrow(unknown_fields)) issues$unknown_fields <- unknown_fields

  list(ok = length(issues) == 0, issues = issues)
}

#' @keywords internal
assert_valid <- function(parser) {
  parser$validate(error = TRUE)
}
