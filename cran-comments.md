# CRAN resubmission for twbparser 0.2.1

## Changes since 0.2.0

-   Replaced a Unicode arrow in docs (PDF manual now builds on all platforms).
-   Added `Depends: R (>= 4.2.0)` due to native pipe placeholder usage.
-   Ensured vignettes are built into the tarball (`inst/doc`) and vignette index is created at install.
-   Canonicalized URLs (GitHub Pages trailing slash; Codecov `app.` host).
-   Spell-check pass; added a WORDLIST for domain terms and proper nouns (e.g., “TWB”, “TWBX”, “GraphQL”).

## Notes addressed

-   **File URI**: Changed README to link the license via a full https URL.
-   **Spelling**: “TWB/TWBX” are Tableau file formats; updated Title/Description to use the acronyms and documented them in WORDLIST.
-   **Vignette index**: We ship source vignettes and prebuilt vignette(s) in `inst/doc`; the vignette index (`Meta/vignette.rds`) is created at install time by R CMD INSTALL. Verified `inst/doc/twbparser-intro.html` is present in the source bundle.

## R CMD check results

0 errors \| 0 warnings \| 0 notes

------------------------------------------------------------------------

# CRAN submission for twbparser

## Package overview

`twbparser` parses Tableau workbooks (`.twb`) and packaged workbooks (`.twbx`) to extract calculated fields, parameters, field dependencies, data sources, filters, and dashboard zones. It uses only local files and writes only to temp directories during examples/tests.

## Test environments

-   local R 4.4.1
-   GitHub Actions: ubuntu-latest (devel, release, oldrel-1); macos-latest (release); windows-latest (release)
-   win-builder: release, devel
-   rhub: Linux (R-devel, R-release), Windows, macOS

## R CMD check results

0 errors \| 0 warnings \| 0 notes\
- `--as-cran` run locally and in CI.\
- All examples complete in \<5s total on GH Actions runners.

## Notes for CRAN

-   This is the **first CRAN release** of `twbparser`.
-   No external network resources are used in examples, vignettes, or tests.
-   No non-temporary files are written; tests/examples use `tempdir()`/`withr::with_tempdir()`.
-   No `:::` usage; all symbols are imported via `NAMESPACE`.
-   Spelling checked with `devtools::spell_check()`; product names like “Tableau” and “Shiny” are proper nouns.
-   Examples that would require large `.twb/.twbx` files are trimmed; larger flows are demonstrated in vignettes using small bundled fixtures.
-   Dependencies in `Imports` are minimal and widely available; optional `Suggests` guarded via `requireNamespace()`.

## URL checks

-   All URLs (README/vignette) verified with `urlchecker::url_check()`; no redirects or failures.

## Additional policies

-   The package does not download or execute code at build/check time.
-   No long-running examples, network calls, or parallelization by default.
-   Memory usage is bounded and tested with large XML inputs in CI (sanity checks).

## Maintainer

George Arthur [prigasgenthian48\@gmail.com](mailto:prigasgenthian48@gmail.com){.email}
