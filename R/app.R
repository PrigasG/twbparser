#' Launch the twbparser Shiny app
#'
#' Opens an interactive workbook inspector for `.twb` and `.twbx` files.
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return The value returned by [shiny::runApp()].
#' @export
run_twbparser_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "The Shiny app requires the 'shiny' package. Install it with install.packages('shiny').",
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny", "twbparser", package = "twbparser")
  if (!nzchar(app_dir)) {
    stop("Could not find the installed twbparser Shiny app.", call. = FALSE)
  }

  shiny::runApp(app_dir, ...)
}
