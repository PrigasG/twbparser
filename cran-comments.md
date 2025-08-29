# CRAN submission for twbparser

## Package overview
`twbparser` parses Tableau workbooks (`.twb`) and packaged workbooks (`.twbx`) to extract
calculated fields, parameters, field dependencies, data sources, filters, and dashboard zones.
It uses only local files and writes only to temp directories during examples/tests.

## Test environments
- local R installation, R 4.4.1
- ubuntu-latest (GitHub Actions): devel, release, oldrel-1
- macOS-latest (GitHub Actions): release
- windows-latest (GitHub Actions): release
- win-builder: release, devel
- rhub: Linux (R-devel, R-release), Windows, macOS

## R CMD check results
0 errors | 0 warnings | 0 notes

- `--as-cran` run locally and in CI.
- All examples complete in <5s total on GH Actions runners.

## Notes for CRAN
- This is the **first CRAN release** of `twbparser`.
- No external network resources are used in examples, vignettes, or tests.
- No non-temporary files are written; tests/examples use `tempdir()`/`withr::with_tempdir()`.
- No `:::` usage; all symbols are imported via `NAMESPACE`.
- Spelling checked with `devtools::spell_check()`; product names like “Tableau”, “Shiny” are proper nouns.
- Encodings are UTF-8; tested on Windows to avoid path/encoding issues.
- Examples that would require large `.twb/.twbx` files are trimmed; larger flows are demonstrated in vignettes using small bundled fixtures.
- Dependencies in `Imports` are minimal and widely available; code guards optional `Suggests` with `requireNamespace()` in examples/tests.

## Reverse dependencies
- Not applicable (new package).

## rhub / win-builder summary
- win-builder (release & devel): OK
- rhub multi-platform matrix: OK
  - No platform-specific warnings; Solaris-equivalent checks not available but
    code uses only base R + portable dependencies.

## URL checks
- All URLs (README/vignette) verified with `urlchecker::url_check()`. No redirects or failures.

## Additional policies
- The package does not download or execute code at build/check time.
- No use of long-running examples, network calls, or parallelization by default.
- Memory usage is bounded and tested with large XML inputs in CI (sanity checks).

## Maintainer
George Arthur <prigasgenthian48@gmail.com>
