release_demo_workbook <- function() {
  path <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")
  skip_if_not(nzchar(path) && file.exists(path), "bundled app workbook not available")
  path
}

test_that("0.5.1 analysis functions work with the bundled app workbook", {
  twb <- release_demo_workbook()
  parser <- TwbParser$new(twb)

  spec <- twb_sheet_spec(parser)
  expect_type(spec, "list")
  expect_gt(length(spec), 0L)

  expect_s3_class(twb_dashboard_charts(parser), "data.frame")
  expect_s3_class(twb_unused_fields(parser), "data.frame")
  expect_s3_class(twb_calc_build_order(parser), "data.frame")
  expect_s3_class(twb_parameter_usage(parser), "data.frame")

  audit <- audit_tableau_folder(twb)
  expect_s3_class(audit, "twbparser_audit")
  expect_equal(nrow(audit$workbooks), 1L)
  expect_equal(nrow(audit$issues), 0L)
})

test_that("0.5.1 lineage and migration functions support every output target", {
  parser <- TwbParser$new(release_demo_workbook())

  lineage <- twb_lineage(parser, format = "tables")
  expect_named(lineage, c("nodes", "edges"))
  expect_s3_class(twb_lineage(parser, format = "igraph"), "igraph")
  expect_match(twb_lineage(parser, format = "mermaid"), "^flowchart (LR|TD)")

  targets <- c("powerbi", "shiny", "quarto", "looker", "superset")
  assessments <- lapply(targets, function(target) {
    twb_migration_assessment(parser, target = target)
  })
  expect_true(all(vapply(
    assessments,
    function(x) all(c("summary", "compatibility", "recommendations") %in% names(x)),
    logical(1L)
  )))

  compatibility <- twb_compatibility(parser, targets = targets)
  expect_setequal(unique(compatibility$target), targets)

  formula <- "IF [Sales] > 0 THEN [Profit] ELSE 0 END"
  translations <- lapply(c("dax", "sql", "r"), function(target) {
    translate_tableau_calc(formula, target = target)
  })
  expect_true(all(vapply(
    translations,
    function(x) nrow(x) == 1L && nzchar(x$translated_formula[[1L]]),
    logical(1L)
  )))
})

test_that("0.5.1 output functions create usable artifacts from the app workbook", {
  parser <- TwbParser$new(release_demo_workbook())
  output_root <- withr::local_tempdir(pattern = "twbparser-release-")

  brief_path <- file.path(output_root, "migration-brief.md")
  brief <- render_migration_brief(parser, target = "shiny", output = brief_path)
  expect_length(brief, 1L)
  expect_true(file.exists(brief_path))
  expect_gt(file.size(brief_path), 0)

  shiny_path <- scaffold_shiny_dashboard(
    parser,
    path = file.path(output_root, "shiny-scaffold")
  )
  expect_true(file.exists(shiny_path))
  expect_silent(parse(file = shiny_path))

  quarto_path <- scaffold_quarto_dashboard(
    parser,
    path = file.path(output_root, "quarto-scaffold")
  )
  expect_true(file.exists(quarto_path))
  expect_true(any(grepl(
    "format: dashboard",
    readLines(quarto_path, warn = FALSE),
    fixed = TRUE
  )))

  shiny_bundle <- export_migration_bundle(
    parser,
    target = "shiny",
    path = file.path(output_root, "bundle-shiny"),
    include_scaffold = TRUE
  )
  quarto_bundle <- export_migration_bundle(
    parser,
    target = "quarto",
    path = file.path(output_root, "bundle-quarto"),
    include_scaffold = TRUE
  )
  expect_true(all(file.exists(shiny_bundle$file)))
  expect_true(all(file.exists(quarto_bundle$file)))

  batch_path <- parse_twb(
    release_demo_workbook(),
    output_dir = file.path(output_root, "batch-export"),
    quiet = TRUE
  )
  expected <- c(
    "overview.csv", "datasources.csv", "parameters.csv", "fields.csv",
    "calculated_fields.csv", "pages.csv", "report.txt", "sheet_specs.txt",
    "replication_brief.txt"
  )
  expect_true(all(file.exists(file.path(batch_path, expected))))
})

test_that("0.5.1 parser methods and active bindings work with the app workbook", {
  parser <- TwbParser$new(release_demo_workbook())
  methods <- c(
    "get_sheet_spec", "get_dashboard_charts", "get_unused_fields",
    "get_calc_build_order", "get_parameter_usage", "get_lineage",
    "get_migration_assessment", "get_compatibility"
  )
  expect_true(all(vapply(methods, function(name) is.function(parser[[name]]), logical(1L))))

  properties <- list(
    parser$sheet_spec,
    parser$dashboard_charts,
    parser$unused_fields,
    parser$calc_build_order,
    parser$parameter_usage,
    parser$lineage,
    parser$migration_assessment,
    parser$compatibility
  )
  expect_length(properties, 8L)
})
