test_that("bundled shiny app is present and syntactically valid", {
  app_file <- system.file("shiny", "twbparser", "app.R", package = "twbparser")
  if (!nzchar(app_file)) skip("shiny app not installed")

  expect_no_error(parse(file = app_file))

  src <- paste(readLines(app_file, warn = FALSE), collapse = "\n")
  expect_match(src, "shinyApp(ui, server)", fixed = TRUE)
})

test_that("bundled shiny app object builds headlessly", {
  skip_if_not_installed("shiny")
  app_file <- system.file("shiny", "twbparser", "app.R", package = "twbparser")
  if (!nzchar(app_file)) skip("shiny app not installed")

  # the app file sets an option at source time; restore it afterwards
  old_opt <- getOption("shiny.maxRequestSize")
  on.exit(options(shiny.maxRequestSize = old_opt), add = TRUE)

  # sourcing runs the trailing shinyApp(ui, server), which constructs
  # (but does not launch) the app object
  app <- source(app_file, local = new.env())$value
  expect_true(inherits(app, "shiny.appobj"))
})
