# light-touch cleanup for twbparser
clean_file <- function(path) {
  x <- readLines(path, warn = FALSE)

  # remove banner-only comments like "---- section ----" (keep roxygen #' and normal comments)
  x <- x[!grepl("^#\\s*-{3,}.*$", x)]

  # collapse runs of >1 blank lines to a single blank
  x <- gsub("\r", "", x)
  while (any(grepl("^\\s*$", x) & c(FALSE, grepl("^\\s*$", head(x, -1))))) {
    x <- x[!(grepl("^\\s*$", x) & c(FALSE, grepl("^\\s*$", head(x, -1))))]
  }

  # trim trailing whitespace
  x <- sub("[ \t]+$", "", x)

  writeLines(x, path, useBytes = TRUE)
}

paths <- list.files("R", pattern = "[.]R$", full.names = TRUE)
invisible(lapply(paths, clean_file))
