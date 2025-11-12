# Extract the .twb (and optionally all files) from a .twbx

Extract the .twb (and optionally all files) from a .twbx

## Usage

``` r
extract_twb_from_twbx(
  twbx_path,
  extract_dir = file.path(tempdir(), paste0("twbx_",
    tools::file_path_sans_ext(basename(twbx_path)), "_", format(Sys.time(),
    "%Y%m%d%H%M%S"))),
  extract_all = FALSE
)
```

## Arguments

- twbx_path:

  Path to a `.twbx` file.

- extract_dir:

  Directory to extract into (defaults to a timestamped temp dir).

- extract_all:

  If `TRUE`, extract entire archive; otherwise only the largest `.twb`.

## Value

List with `twb_path`, `exdir`, `twbx_path`, and `manifest` (tibble).

## Examples

``` r
twbx <- system.file("extdata", "test_for_zip.twbx", package = "twbparser")
res  <- extract_twb_from_twbx(twbx, extract_all = FALSE)
#> Extracted .twb from .twbx: test-for_zip.twb
basename(res$twb_path)
#> [1] "test-for_zip.twb"
```
