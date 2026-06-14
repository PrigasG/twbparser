# Safely evaluate and return fallback on error

Safely evaluate and return fallback on error

## Usage

``` r
safe_call(expr, fallback, warn = FALSE)
```

## Arguments

- expr:

  Expression to evaluate

- fallback:

  Value if an error occurs

- warn:

  If `TRUE`, emit a warning with the caught error message.

## Value

Result of `expr` or `fallback`
