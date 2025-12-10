# Extract `<named-connection>` entries from a TWB

Rich, safe extraction of `<named-connection>` nodes and their
`<connection>` attributes into a tidy tibble.

## Usage

``` r
extract_named_connections(xml_doc)
```

## Arguments

- xml_doc:

  An `xml2` document for a Tableau `.twb`.

## Value

Tibble with columns like `connection_id`, `connection_caption`,
`connection_class`, `connection_target`, `dbname`, `schema`,
`warehouse`, `region`, `filename`, and `location_named`.

## Examples

``` r
# Preferred: read from a tiny '.twb'
twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
if (nzchar(twb) && file.exists(twb)) {
xml <- xml2::read_xml(twb)
extract_named_connections(xml)
}
#> # A tibble: 2 × 10
#>   connection_id     connection_caption connection_class connection_target dbname
#>   <chr>             <chr>              <chr>            <chr>             <chr> 
#> 1 excel-direct.0df… test_county        excel-direct     ""                NA    
#> 2 ogrdirect.07lza2… Municipal_Boundar… ogrdirect        ""                NA    
#> # ℹ 5 more variables: schema <chr>, warehouse <chr>, region <chr>,
#> #   filename <chr>, location_named <chr>

twbx <- system.file("extdata", "test_for_zip.twbx", package = "twbparser")
if (nzchar(twbx) && file.exists(twbx)) {
 members <- twbx_list(twbx)
 twb_rows <- members$name[grepl("\\.twb$", members$name)]
 if (length(twb_rows) > 0L && !is.na(twb_rows[1])) {
   twb_member <- twb_rows[1]
   xml <- xml2::read_xml(utils::unzip(twbx, twb_member, exdir = tempdir()))
   extract_named_connections(xml)
 }
}
#> # A tibble: 1 × 10
#>   connection_id     connection_caption connection_class connection_target dbname
#>   <chr>             <chr>              <chr>            <chr>             <chr> 
#> 1 excel-direct.1ao… Sample - Supersto… excel-direct     ""                NA    
#> # ℹ 5 more variables: schema <chr>, warehouse <chr>, region <chr>,
#> #   filename <chr>, location_named <chr>
```
