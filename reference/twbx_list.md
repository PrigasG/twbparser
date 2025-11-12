# List contents of a Tableau .twbx

List contents of a Tableau .twbx

## Usage

``` r
twbx_list(twbx_path)
```

## Arguments

- twbx_path:

  Path to a `.twbx` file.

## Value

Tibble with columns: `name`, `size_bytes`, `modified`, `type`.

## Examples

``` r
twbx <- system.file("extdata", "test_for_zip.twbx", package = "twbparser")
twbx_list(twbx)
#> # A tibble: 2 × 4
#>   name                                  size_bytes modified            type    
#>   <chr>                                      <dbl> <dttm>              <chr>   
#> 1 test-for_zip.twb                           76627 2025-09-23 07:15:00 workbook
#> 2 Data/en_US-US/Sample - Superstore.xls    3453440 2024-10-08 08:01:00 excel   
```
