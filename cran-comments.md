## twbparser 0.5.1

### Changes since 0.5.0

* Added tools for workbook audits, lineage, migration checks, calculation
  translation, migration exports, and Shiny or Quarto starter projects.
* Added worksheet specifications, dashboard chart summaries, unused-field
  checks, calculated-field build order, and parameter usage reports.
* Added `parse_twb()` to export a workbook report and its main tables.
* Fixed chart mark detection and workbook-name lookup.
* Improved the README, help files, cheat sheet, and tests.
* Removed `tbs_publish_info()` and `tbs_custom_sql_graphql()`. These functions
  were empty stubs and did not connect to Tableau Server or Tableau Cloud.
* Kept the `strict` argument in `validate_relationships()` for compatibility.
  It is deprecated and ignored.

## Test environments

* Windows 11, R 4.5.1
* GitHub Actions: Windows release, macOS R 4.5, and Ubuntu devel, release,
  and oldrel-1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are currently no reverse dependencies on CRAN.
