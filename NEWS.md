# twbparser 0.3.1

* Remove use of `unlockBinding()` in internal TwbParser active-binding helpers.
  This avoids CRAN's "possibly unsafe call" NOTE while keeping the same
  user-facing behaviour for no-parens properties (overview, pages, datasources, etc.).

---

# twbparser 0.3.0

* Added a `NEWS.md` file to track changes to the package.
