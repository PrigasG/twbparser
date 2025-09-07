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
} else {
  cat("> Demo .twb not found in installed package. Skipping executable example.\n")
  cat("  To enable examples, add `inst/extdata/test_for_wenjie.twb` to the package.\n")
}

## ----parse-twb, eval=exists("parser")-----------------------------------------
parser$summary()

## ----datasources, eval=exists("parser")---------------------------------------
datasources <- parser$get_datasources()
parameters <- parser$get_parameters()

print(head(datasources))
print(head(parameters))

## ----relationships-joins, eval=exists("parser")-------------------------------
relations <- parser$get_relations()
joins <- parser$get_joins()
relationships <- parser$get_relationships()
inferred_relationships <- parser$get_inferred_relationships()

cat("Legacy relations:\n")
print(head(relations))

cat("Join clauses:\n")
print(head(joins))

cat("Modern relationships:\n")
print(head(relationships))

cat("Inferred relationships:\n")
print(head(inferred_relationships))

## ----fields-calculated, eval=exists("parser")---------------------------------
fields <- parser$get_fields()
calculated_fields <- parser$get_calculated_fields(pretty = TRUE)

cat("Sample raw fields:\n")
print(head(fields))

cat("Sample calculated fields:\n")
print(head(calculated_fields))

## ----twbx, eval=exists("parser") && !is.null(parser$twbx_path)----------------
# cat("TWBX manifest contents:\n")
# print(parser$get_twbx_manifest())
# 
# cat("Listing TWBX extract files:\n")
# print(parser$get_twbx_extracts())
# 
# cat("Listing TWBX images:\n")
# print(parser$get_twbx_images())
# 
# # Example: Extract all image files to temporary directory
# # temp_images_dir <- tempdir()
# # parser$extract_twbx_assets(types = "image", exdir = temp_images_dir)
# # cat("Extracted TWBX images to:", temp_images_dir, "\n")

## ----validate, eval=exists("parser")------------------------------------------
validation <- parser$validate()
if (validation$ok) {
  cat("Relationships validated successfully.\n")
} else {
  cat("Validation issues found:\n")
  print(validation$issues)
}

