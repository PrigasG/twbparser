## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(twbparser)

twb_path <- system.file("extdata", "test_for_wenjie.twb", package = "twbparser")

if (nzchar(twb_path) && file.exists(twb_path)) {
  parser <- TwbParser$new(twb_path)
  parser$get_calculated_fields()
} else {
  cat("> Demo .twb not found in installed package. Skipping executable example.\n")
  cat("  To enable examples, add `inst/extdata/test_for_wenjie.twb` to the package.\n")
}

