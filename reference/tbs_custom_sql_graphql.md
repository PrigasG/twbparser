# Custom SQL (Metadata API) for a published item

Queries the Metadata (GraphQL) API for Custom SQL tables in the content
graph.

## Usage

``` r
tbs_custom_sql_graphql(
  content_id,
  base_url = Sys.getenv("TABLEAU_BASE_URL"),
  site = Sys.getenv("TABLEAU_SITE"),
  token = Sys.getenv("TABLEAU_PAT")
)
```

## Arguments

- content_id:

  Character. Workbook or datasource ID (GUID).

- base_url:

  Character. Server/Cloud base URL (e.g., "https://...").

- site:

  Character. Site contentUrl ("" for default site).

- token:

  Character. REST credentials token.

## Value

A tibble with columns such as `custom_sql_name`, `custom_sql_query`,
`database`, `schema`. Zero rows if none.

## Examples

``` r
if (FALSE) { # all(nzchar(Sys.getenv(c("TABLEAU_BASE_URL", "TABLEAU_SITE", "TABLEAU_PAT"))))
tbs_custom_sql_graphql("abc-123")
}
```
