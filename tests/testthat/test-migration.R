test_that("twb_lineage returns node and edge tables", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")

  parser <- TwbParser$new(twb)
  lineage <- twb_lineage(parser)

  expect_type(lineage, "list")
  expect_s3_class(lineage$nodes, "tbl_df")
  expect_s3_class(lineage$edges, "tbl_df")
  expect_true(all(c("id", "label", "type", "workbook") %in% names(lineage$nodes)))
  expect_true(all(c("from", "to", "relationship", "workbook") %in% names(lineage$edges)))
})

test_that("twb_lineage can return Mermaid text", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")

  parser <- TwbParser$new(twb)
  mermaid <- twb_lineage(parser, format = "mermaid")

  expect_type(mermaid, "character")
  expect_length(mermaid, 1L)
  expect_match(mermaid, "flowchart LR")
})

test_that("TwbParser exposes lineage method and active binding", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")

  parser <- TwbParser$new(twb)
  method_out <- parser$get_lineage()
  binding_out <- parser$lineage

  expect_s3_class(method_out$nodes, "tbl_df")
  expect_identical(binding_out, method_out)
})

test_that("audit_tableau_folder summarizes available workbook files", {
  extdata <- system.file("extdata", package = "twbparser")
  skip_if_not(nzchar(extdata) && dir.exists(extdata), "example data not found")

  audit <- audit_tableau_folder(extdata, recursive = FALSE)

  expect_s3_class(audit$workbooks, "tbl_df")
  expect_true(all(c("workbook", "file", "worksheets", "dashboards",
                    "migration_complexity_score", "migration_complexity") %in%
                    names(audit$workbooks)))
  expect_true("issues" %in% names(audit))
})

test_that("audit_tableau_folder can write CSV sections", {
  extdata <- system.file("extdata", package = "twbparser")
  skip_if_not(nzchar(extdata) && dir.exists(extdata), "example data not found")

  out_dir <- file.path(tempdir(), paste0("twbparser-audit-", Sys.getpid()))
  audit <- audit_tableau_folder(extdata, recursive = FALSE,
                                write_csv = TRUE, output_dir = out_dir)

  expect_true(file.exists(file.path(out_dir, "workbooks.csv")))
  expect_true(file.exists(file.path(out_dir, "issues.csv")))
  expect_s3_class(audit$workbooks, "tbl_df")
})

test_that("twb_compatibility returns target feature rows", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")

  parser <- TwbParser$new(twb)
  out <- twb_compatibility(parser, targets = c("powerbi", "shiny"))

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("target", "support", "feature", "detected", "impact", "note") %in% names(out)))
  expect_true(all(c("powerbi", "shiny") %in% out$target))
})

test_that("twb_migration_assessment returns summary and recommendations", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")

  parser <- TwbParser$new(twb)
  assessment <- twb_migration_assessment(parser, target = "shiny")

  expect_type(assessment, "list")
  expect_s3_class(assessment$summary, "tbl_df")
  expect_s3_class(assessment$compatibility, "tbl_df")
  expect_type(assessment$recommendations, "character")
  expect_equal(assessment$summary$target, "shiny")
})

test_that("translate_tableau_calc returns review metadata", {
  out <- translate_tableau_calc(
    c("IF [Sales] > 0 THEN [Profit] ELSE 0 END", "{ FIXED [Region] : SUM([Sales]) }"),
    target = "sql"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2L)
  expect_true(all(c("translated_formula", "confidence", "notes") %in% names(out)))
  expect_true(any(out$confidence == "low"))
})

test_that("render_migration_brief returns Markdown and can write output", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")

  parser <- TwbParser$new(twb)
  out <- file.path(tempdir(), paste0("migration-brief-", Sys.getpid(), ".md"))
  txt <- render_migration_brief(parser, target = "quarto", output = out)

  expect_type(txt, "character")
  expect_length(txt, 1L)
  expect_match(txt, "# Tableau Migration Brief:")
  expect_true(file.exists(out))
})

test_that("scaffold helpers write Shiny and Quarto files", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")

  parser <- TwbParser$new(twb)
  shiny_file <- scaffold_shiny_dashboard(parser, path = file.path(tempdir(), paste0("shiny-", Sys.getpid())))
  quarto_file <- scaffold_quarto_dashboard(parser, path = file.path(tempdir(), paste0("quarto-", Sys.getpid())))

  expect_true(file.exists(shiny_file))
  expect_true(file.exists(quarto_file))
})

test_that("export_migration_bundle writes migration artifacts", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")

  parser <- TwbParser$new(twb)
  out_dir <- file.path(tempdir(), paste0("bundle-", Sys.getpid()))
  files <- export_migration_bundle(parser, target = "shiny", path = out_dir)

  expect_s3_class(files, "tbl_df")
  expect_true(file.exists(file.path(out_dir, "migration_brief.md")))
  expect_true(file.exists(file.path(out_dir, "compatibility.csv")))
  expect_true(file.exists(file.path(out_dir, "shiny-scaffold", "app.R")))
})

test_that("TwbParser exposes compatibility and migration assessment bindings", {
  twb <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(twb) && file.exists(twb), "example .twb not found")

  parser <- TwbParser$new(twb)

  expect_s3_class(parser$get_compatibility(), "tbl_df")
  expect_s3_class(parser$compatibility, "tbl_df")
  expect_type(parser$get_migration_assessment(), "list")
  expect_type(parser$migration_assessment, "list")
})
