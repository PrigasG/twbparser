test_that("parse_twb() writes a batch export to output_dir", {
  demo <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  if (!nzchar(demo) || !file.exists(demo)) skip("demo .twb not available")

  out_dir <- file.path(tempdir(), "twbparser-parse-twb-test")
  unlink(out_dir, recursive = TRUE)

  out <- parse_twb(demo, output_dir = out_dir, quiet = TRUE)

  expect_true(dir.exists(out))
  written <- list.files(out)
  # human-readable artefacts
  expect_true("report.txt" %in% written)
  expect_true("replication_brief.txt" %in% written)
  expect_true("sheet_specs.txt" %in% written)
  specs_txt <- paste(readLines(file.path(out, "sheet_specs.txt")), collapse = "\n")
  expect_true(grepl("Sheet: Sheet 1", specs_txt, fixed = TRUE))
  expect_true(grepl("Mark type: map", specs_txt, fixed = TRUE))
  # key tables
  for (f in c("overview.csv", "datasources.csv", "parameters.csv",
              "fields.csv", "calculated_fields.csv", "relationships.csv",
              "joins.csv", "custom_sql.csv", "pages.csv", "dashboards.csv")) {
    expect_true(f %in% written, info = f)
  }
  # the overview csv really is the one-row overview
  ov <- utils::read.csv(file.path(out, "overview.csv"))
  expect_equal(nrow(ov), 1L)

  # a second run into the same non-empty dir refuses without overwrite = TRUE
  expect_error(
    parse_twb(demo, output_dir = out_dir, quiet = TRUE),
    "overwrite"
  )

  unlink(out_dir, recursive = TRUE)
})

test_that("parse_twb() validates its inputs", {
  expect_error(parse_twb(123), "single non-empty")
  expect_error(parse_twb("", output_dir = ""), "single non-empty")
  expect_error(
    parse_twb("definitely-not-a-workbook.twb",
              output_dir = file.path(tempdir(), "twbparser-missing-file-test")),
    "File not found"
  )
})

test_that("parse_twb(overwrite = TRUE) replaces its own outputs, keeps others", {
  demo <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  if (!nzchar(demo) || !file.exists(demo)) skip("demo .twb not available")

  out_dir <- file.path(tempdir(), "twbparser-parse-twb-overwrite-test")
  unlink(out_dir, recursive = TRUE)
  dir.create(out_dir, recursive = TRUE)

  # a stale parse_twb output from a "previous" run, plus an unrelated user file
  writeLines("stale", file.path(out_dir, "report.txt"))
  writeLines("mine", file.path(out_dir, "notes.txt"))

  out <- parse_twb(demo, output_dir = out_dir, overwrite = TRUE, quiet = TRUE)

  # our own stale file was replaced ...
  report <- paste(readLines(file.path(out, "report.txt")), collapse = "\n")
  expect_false(grepl("stale", report, fixed = TRUE))
  # ... while the unrelated file survived
  expect_equal(readLines(file.path(out, "notes.txt")), "mine")

  unlink(out_dir, recursive = TRUE)
})
