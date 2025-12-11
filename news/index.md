# Changelog

## twbparser 0.3.1

CRAN release: 2025-12-10

- Remove use of [`unlockBinding()`](https://rdrr.io/r/base/bindenv.html)
  in internal TwbParser active-binding helpers. This avoids CRAN’s
  “possibly unsafe call” NOTE while keeping the same user-facing
  behaviour for no-parens properties (overview, pages, datasources,
  etc.).

------------------------------------------------------------------------

## twbparser 0.3.0

- Added a `NEWS.md` file to track changes to the package.
