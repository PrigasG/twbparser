# Extract datasource details from a Tableau TWB

Gathers runtime tables (from the object graph), merges in
named-connection metadata (class, caption, targets), and augments with
top-level datasource definitions (field counts, connection type,
location). Also returns a filtered table of parameter datasources.

## Usage

``` r
extract_datasource_details(xml_doc)
```

## Arguments

- xml_doc:

  An `xml2` document for a Tableau `.twb`.

## Value

A named list with:

- data_sources:

  Tibble of datasources joined with connection metadata.

- parameters:

  Tibble of parameter datasources (if present).

- all_sources:

  Same as `data_sources` (placeholder for future variants).

## Examples

``` r
# Preferred: from a tiny .twb
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
if (nzchar(twb) && file.exists(twb)) {
  xml <- xml2::read_xml(twb)
  res <- extract_datasource_details(xml)
  head(res$data_sources)
}
#> # A tibble: 2 × 10
#>   datasource     primary_table connection_id connection_caption connection_class
#>   <chr>          <chr>         <chr>         <chr>              <chr>           
#> 1 Municipal_Bou… [Municipal_B… ogrdirect.07… Municipal_Boundar… ogrdirect       
#> 2 Sheet1         [Sheet1$]     excel-direct… test_county        excel-direct    
#> # ℹ 5 more variables: connection_target <chr>, datasource_name <chr>,
#> #   field_count <int>, connection_type <chr>, location <chr>

# Alternative: from a tiny .twbx (guarded)
twbx <- system.file("extdata", "test_for_zip.twbx", package = "twbparser")
if (nzchar(twbx) && file.exists(twbx)) {
 members  <- twbx_list(twbx)
 twb_rows <- members$name[grepl("\\.twb$", members$name)]
 if (length(twb_rows) > 0L && !is.na(twb_rows[1])) {
   twb_member <- twb_rows[1]
   xml <- xml2::read_xml(utils::unzip(twbx, twb_member, exdir = tempdir()))
   res <- extract_datasource_details(xml)
   head(res$data_sources)
 }
}
#> # A tibble: 1 × 10
#>   datasource primary_table connection_id     connection_caption connection_class
#>   <chr>      <chr>         <chr>             <chr>              <chr>           
#> 1 Orders     [Orders$]     excel-direct.1ao… Sample - Supersto… excel-direct    
#> # ℹ 5 more variables: connection_target <chr>, datasource_name <chr>,
#> #   field_count <int>, connection_type <chr>, location <chr>
```
