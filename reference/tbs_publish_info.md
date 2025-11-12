# Publish info for a workbook or datasource on 'Tableau' Server/Cloud

Returns an empty tibble when credentials are missing or the item is not
found.

## Usage

``` r
tbs_publish_info(
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

  Character. Site contentUrl ("" for the default site).

- token:

  Character. REST credentials token (from a prior sign-in).

## Value

A tibble with columns like `content_id`, `site`, `project`, `web_url`,
`created_at`, `updated_at`. May be zero rows if unavailable.

## Examples

``` r
if (FALSE) { # all(nzchar(Sys.getenv(c("TABLEAU_BASE_URL", "TABLEAU_SITE", "TABLEAU_PAT"))))
tbs_publish_info("abc-123")
}
```
