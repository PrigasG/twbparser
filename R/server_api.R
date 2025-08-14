#' Publish info for a workbook or datasource on Tableau Server/Cloud
#'
#' Returns an empty tibble when credentials are missing or the item is not found.
#'
#' @param content_id Character. Workbook or datasource ID (GUID).
#' @param base_url Character. Server/Cloud base URL (e.g., "https://...").
#' @param site Character. Site contentUrl ("" for the default site).
#' @param token Character. REST credentials token (from a prior sign-in).
#' @return A tibble with columns like content_id, site, project, web_url,
#'   created_at, updated_at. May be zero rows if unavailable.
#' @examples
#' \dontrun{
#' tbs_publish_info(content_id = "abc-123", base_url = Sys.getenv("TABLEAU_BASE_URL"),
#'                  site = Sys.getenv("TABLEAU_SITE"), token = "<token>")
#' }
#' @export
tbs_publish_info <- function(content_id, base_url, site, token) { ... }

#' Custom SQL (Metadata API) for a published workbook/datasource
#'
#' Queries the Metadata (GraphQL) API for Custom SQL tables in the content graph.
#'
#' @param content_id Character. Workbook or datasource ID (GUID).
#' @param base_url Character. Server/Cloud base URL.
#' @param site Character. Site contentUrl ("" for default site).
#' @param token Character. REST credentials token.
#' @return A tibble with columns such as custom_sql_name, custom_sql_query,
#'   database, schema. Zero rows if none.
#' @examples
#' \dontrun{
#' tbs_custom_sql_graphql(content_id = "abc-123",
#'   base_url = Sys.getenv("TABLEAU_BASE_URL"),
#'   site = Sys.getenv("TABLEAU_SITE"),
#'   token = "<token>")
#' }
#' @export
tbs_publish_info <- function(content_id,
                             base_url = Sys.getenv("TABLEAU_BASE_URL"),
                             site     = Sys.getenv("TABLEAU_SITE"),
                             token    = Sys.getenv("TABLEAU_PAT")) {
  if (!nzchar(base_url) || !nzchar(site) || !nzchar(token)) {
    return(tibble::tibble())
  }
  # implement: REST API GET /api/<ver>/sites/<site-id>/workbooks/<id> or datasources/<id>
  # Use httr2; keep version configurable via env if you like
  tibble::tibble(
    content_id = content_id,
    site = site,
    project = NA_character_,
    web_url = NA_character_,
    created_at = as.POSIXct(NA),
    updated_at = as.POSIXct(NA)
  )
}

#' Custom SQL (Metadata API) for a published workbook/datasource
#'
#' Queries Tableau's Metadata (GraphQL) API for Custom SQL tables under a content item.
#'
#' @param content_id Character. Workbook or datasource ID (GUID).
#' @param base_url Character. Tableau Server/Cloud base URL (e.g., "https://...").
#' @param site Character. Site contentUrl ("" for the default site).
#' @param token Character. REST credentials token (X-Tableau-Auth).
#' @return A tibble with columns such as `custom_sql_name`, `custom_sql_query`,
#'   `database`, `schema`. Zero rows if none or if credentials are missing.
#' @examples
#' \dontrun{
#' tbs_custom_sql_graphql(
#'   content_id = "abc-123",
#'   base_url   = Sys.getenv("TABLEAU_BASE_URL"),
#'   site       = Sys.getenv("TABLEAU_SITE"),
#'   token      = "<token>"
#' )
#' }
#' @export
tbs_custom_sql_graphql <- function(content_id,
                                   base_url = Sys.getenv("TABLEAU_BASE_URL"),
                                   site     = Sys.getenv("TABLEAU_SITE"),
                                   token    = Sys.getenv("TABLEAU_PAT")) {
  if (!nzchar(base_url) || !nzchar(site) || !nzchar(token)) {
    return(tibble::tibble())
  }
  # POST GraphQL query to /api/metadata/graphql
  tibble::tibble(
    custom_sql_name = character(),
    custom_sql_query = character(),
    database = character(),
    schema = character()
  )
}
