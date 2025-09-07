#' @keywords internal
.unquote_formula <- function(x) {
  if (is.na(x) || !nzchar(x)) return(x)
  # remove surrounding quotes if the whole thing is quoted
  x <- sub('^"(.*)"$', '\\1', x, perl = TRUE)
  # unescape \" -> "
  x <- gsub('\\"', '"', x, fixed = TRUE)
  x
}

#' @keywords internal
.normalize_ws <- function(x) {
  x <- gsub("\r\n|\r", "\n", x)
  # collapse long runs of spaces, keep single space
  x <- gsub("[ \t]+", " ", x)
  # trim lines
  x <- paste(trimws(unlist(strsplit(x, "\n", fixed = TRUE))), collapse = "\n")
  trimws(x)
}

#' @keywords internal
.break_before_keywords <- function(x) {
  if (is.na(x) || !nzchar(x)) return(x)
  # add a newline BEFORE these keywords when not already at line start
  rules <- list(
    "(?i)(?<!\\n)\\bELSEIF\\b" = "\nELSEIF",
    "(?i)(?<!\\n)\\bELSE\\b"   = "\nELSE",
    "(?i)(?<!\\n)\\bWHEN\\b"   = "\nWHEN",
    "(?i)(?<!\\n)\\bTHEN\\b"   = "\nTHEN",
    "(?i)(?<!\\n)\\bAND\\b"    = "\nAND",
    "(?i)(?<!\\n)\\bOR\\b"     = "\nOR",
    "(?i)(?<!\\n)\\bEND\\b"    = "\nEND"
  )
  for (p in names(rules)) x <- gsub(p, rules[[p]], x, perl = TRUE)
  x
}

#' @keywords internal
.indent_tableau <- function(x, indent_step = 2L) {
  if (is.na(x) || !nzchar(x)) return(x)
  lines <- unlist(strsplit(x, "\n", fixed = TRUE))
  cur <- 0L
  out <- character(length(lines))
  for (i in seq_along(lines)) {
    ln <- trimws(lines[[i]])
    up <- toupper(ln)

    # decrease BEFORE printing for END and leading ELSE/ELSEIF
    dec_now <- grepl("^(END)\\b", up) ||
      grepl("^(ELSEIF|ELSE)\\b", up)
    if (dec_now) cur <- max(0L, cur - 1L)

    out[[i]] <- paste0(strrep(" ", cur * indent_step), ln)

    # increase AFTER printing for IF/CASE and lines with THEN/ELSE (block starts)
    inc_now <- grepl("^IF\\b|^CASE\\b", up) ||
      grepl("\\bTHEN\\b", up) ||
      grepl("^ELSE(IF)?\\b", up)
    if (inc_now && !grepl("^END\\b", up)) cur <- cur + 1L
  }
  paste(out, collapse = "\n")
}

#' Prettify a Tableau calculation formula for display
#'
#' @param formula character scalar
#' @param strip_brackets logical; remove [ ] around field names (default FALSE)
#' @param wrap optional integer to hard-wrap lines (use NA to disable)
#' @return character scalar (multi-line, indented)
#' @export
tableau_formula_pretty <- function(formula, strip_brackets = FALSE, wrap = NA_integer_) {
  if (length(formula) == 0) return(character())
  f <- .unquote_formula(formula)
  f <- .normalize_ws(f)
  f <- .break_before_keywords(f)
  f <- .indent_tableau(f, indent_step = 2L)
  if (isTRUE(strip_brackets)) {
    f <- gsub("\\[|\\]", "", f)
  }
  if (is.na(wrap) || !is.numeric(wrap) || wrap < 10) return(f)
  # base::strwrap preserves words; reassemble lines individually
  paste(unlist(lapply(strsplit(f, "\n", fixed = TRUE)[[1]], function(line) {
    paste(strwrap(line, width = wrap), collapse = "\n")
  })), collapse = "\n")
}

#' Add a prettified formula column to calculated fields table
#' @param calcs tibble from extract_calculated_fields()
#' @param strip_brackets logical
#' @param wrap integer wrap width; default 100
#' @return tibble with extra column `formula_pretty`
#' @export
prettify_calculated_fields <- function(calcs, strip_brackets = FALSE, wrap = 100L) {
  if (nrow(calcs) == 0) return(calcs)
  calcs$formula_pretty <- vapply(
    calcs$formula,
    tableau_formula_pretty,
    FUN.VALUE = character(1),
    strip_brackets = strip_brackets,
    wrap = wrap
  )
  calcs
}
