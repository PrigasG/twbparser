## twbparser 0.3.1

* Removed use of `unlockBinding()` in internal active-binding helpers.
  The package no longer calls `unlockBinding()` or `assignInNamespace()`,
  addressing the previous CRAN NOTE about possibly unsafe calls.


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
