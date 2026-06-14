# Extract specific files from a .twbx

Extract specific files from a .twbx

## Usage

``` r
twbx_extract_files(
  twbx_path,
  files = NULL,
  pattern = NULL,
  types = NULL,
  exdir = NULL
)
```

## Arguments

- twbx_path:

  Path to a `.twbx`.

- files:

  Vector of archive paths to extract (optional).

- pattern:

  Perl regex to match archive paths (optional).

- types:

  Subset by `.twbx` entry `type` (see
  [`twbx_list()`](https://prigasg.github.io/twbparser/reference/twbx_list.md))
  (optional).

- exdir:

  Output directory (defaults to temp).

## Value

Tibble with `name`, `type`, and `out_path` of extracted files.

## Examples

``` r
twbx <- system.file("extdata", "test_for_zip.twbx", package = "twbparser")
files <- twbx_extract_files(twbx, types = c("workbook"))
head(files)
#> # A tibble: 1 × 3
#>   name             type     out_path                                            
#>   <chr>            <chr>    <chr>                                               
#> 1 test-for_zip.twb workbook /tmp/RtmpQRJiOJ/twbx_extract_20260614234124/test-fo…
```
