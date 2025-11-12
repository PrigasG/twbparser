# tools/cran_examples_sanitizer.R
# Sanitize @examples for CRAN:
# - Replace \dontrun{ with \donttest{ inside roxygen @examples blocks
# - Dry-run by default, with clear summary
# - Creates timestamped backups before writing

sanitize_examples <- function(
    root = ".",
    dry_run = TRUE,
    replace_dontrun = TRUE,
    flag_unwrap_candidates = TRUE
) {
  r_dir <- file.path(root, "R")
  stopifnot(dir.exists(r_dir))
  files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)

  changed <- integer()
  found_dontrun <- integer()
  unwrap_flags <- list()

  ts <- format(Sys.time(), "%Y%m%d-%H%M%S")

  for (f in files) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    orig  <- lines

    in_rox <- FALSE
    in_examples <- FALSE
    dontrun_hits <- 0L

    # Simple heuristics for "very likely fast" example content
    unwrap_markers <- character()

    for (i in seq_along(lines)) {
      ln <- lines[i]
      trimmed <- sub("^\\s+", "", ln)

      if (startsWith(trimmed, "#'")) {
        # still in roxygen
        in_rox <- TRUE

        # entering/exiting @examples
        if (grepl("^#'\\s*@examples\\b", trimmed)) {
          in_examples <- TRUE
        } else if (in_examples && grepl("^#'\\s*@\\w+", trimmed)) {
          # another tag begins → examples end
          in_examples <- FALSE
        }

        if (in_examples) {
          # Replace \dontrun{ with \donttest{
          if (replace_dontrun && grepl("\\\\dontrun\\{", ln, perl = TRUE)) {
            lines[i] <- sub("\\\\dontrun\\{", "\\\\donttest{", ln, perl = TRUE)
            dontrun_hits <- dontrun_hits + 1L
          }

          if (flag_unwrap_candidates) {
            # Heuristic flags to suggest "easy unwrapping" later (manual review)
            if (grepl("system\\.file\\(", ln) ||
                grepl("#'\\s*#\\s*no\\s*plot", ln, ignore.case = TRUE) ||
                grepl("readRDS\\(|read\\.csv\\(|tibble::tibble\\(", ln) ||
                grepl("library\\(", ln) == FALSE && nchar(gsub("^#'\\s*", "", trimmed)) < 120) {
              unwrap_markers <- c(unwrap_markers, paste0(basename(f), ":", i))
            }
          }
        }
      } else {
        # left roxygen block
        in_rox <- FALSE
        in_examples <- FALSE
      }
    }

    if (!identical(lines, orig)) {
      changed <- c(changed, which(files == f))
      found_dontrun <- c(found_dontrun, dontrun_hits)
      if (!dry_run) {
        bak <- sprintf("%s.%s.bak", f, ts)
        file.copy(f, bak, overwrite = TRUE)
        writeLines(lines, f, useBytes = TRUE)
      }
    } else if (dontrun_hits > 0) {
      # just in case, though identical check would have caught it
      found_dontrun <- c(found_dontrun, dontrun_hits)
    }

    if (length(unwrap_markers)) {
      unwrap_flags[[f]] <- unique(unwrap_markers)
    }
  }

  cat("\n== CRAN examples sanitizer summary ==\n")
  cat("Scanned:", length(files), "files under", r_dir, "\n")
  cat("Dry run :", dry_run, "\n")
  cat("Changed :", length(changed), "file(s)\n")
  if (length(changed)) {
    for (idx in changed) {
      cat("  -", files[idx], "\n")
    }
  }

  if (length(unwrap_flags)) {
    cat("\nPotential unwrap candidates (manual review suggested):\n")
    for (nm in names(unwrap_flags)) {
      cat("  *", nm, "→ lines:", paste(unwrap_flags[[nm]], collapse = ", "), "\n")
    }
  }

  invisible(list(
    changed_files = files[changed],
    unwrap_flags = unwrap_flags
  ))
}

# Convenience runner when invoked via `Rscript tools/cran_examples_sanitizer.R`
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  dry  <- if (length(args) && tolower(args[1]) %in% c("false","0","no")) FALSE else TRUE
  res <- sanitize_examples(root = ".", dry_run = dry)
  if (!dry) {
    cat("\nTip: re-run docs after changes:\n  devtools::document()\n")
  } else {
    cat("\nDry run only. To apply changes:\n  Rscript tools/cran_examples_sanitizer.R false\n")
  }
}



# tools/bench_examples.R
bench_examples <- function(pkg_dir = ".", time_limit = 5) {
  if (!requireNamespace("R.utils", quietly = TRUE)) {
    stop("Please install.packages('R.utils') first.")
  }
  man_dir <- file.path(pkg_dir, "man")
  if (!dir.exists(man_dir)) stop("No 'man/' directory found at: ", normalizePath(pkg_dir))

  rd_files <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
  if (!length(rd_files)) stop("No .Rd files found in 'man/' — run devtools::document() first.")

  results <- data.frame(
    file = basename(rd_files),
    elapsed = NA_real_,
    ok = FALSE,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(rd_files)) {
    rd <- rd_files[i]
    exfile <- tempfile(fileext = ".R")

    # Convert Rd -> runnable R example script
    tools::Rd2ex(rd, out = exfile)

    # Time with a hard timeout
    env <- new.env(parent = baseenv())
    elapsed <- NA_real_; ok <- FALSE
    try({
      elapsed <- R.utils::withTimeout({
        t0 <- proc.time()
        sys.source(exfile, envir = env, keep.source = FALSE)
        unname((proc.time() - t0)[["elapsed"]])
      }, timeout = time_limit, onTimeout = "error")
      ok <- is.finite(elapsed) && elapsed <= time_limit
    }, silent = TRUE)

    results$elapsed[i] <- elapsed
    results$ok[i] <- ok
  }

  results[order(results$ok, results$elapsed, na.last = TRUE), ]
}



# tools/unwrap_donttest_all.R
unwrap_donttest_all <- function(root = ".", dry_run = TRUE) {
  r_dir <- file.path(root, "R")
  stopifnot(dir.exists(r_dir))
  files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)

  changed <- character()
  ts <- format(Sys.time(), "%Y%m%d-%H%M%S")

  for (f in files) {
    x <- readLines(f, warn = FALSE, encoding = "UTF-8")
    y <- x

    in_rox <- FALSE
    in_examples <- FALSE

    for (i in seq_along(y)) {
      ln <- y[i]
      tr <- sub("^\\s+", "", ln)
      if (startsWith(tr, "#'")) {
        in_rox <- TRUE
        if (grepl("^#'\\s*@examples\\b", tr)) {
          in_examples <- TRUE
        } else if (in_examples && grepl("^#'\\s*@\\w+", tr)) {
          in_examples <- FALSE
        }
        if (in_examples) {
          # Remove \donttest{ markers (inline or on their own line)
          y[i] <- sub("\\\\donttest\\{", "", y[i], perl = TRUE)
          # Drop closing '}' lines inside examples if they are standalone
          if (grepl("^#'\\s*\\}\\s*$", y[i])) y[i] <- sub("\\}", "", y[i])
        }
      } else {
        in_rox <- FALSE
        in_examples <- FALSE
      }
    }

    if (!identical(x, y)) {
      changed <- c(changed, f)
      if (!dry_run) {
        bak <- sprintf("%s.%s.bak", f, ts)
        file.copy(f, bak, overwrite = TRUE)
        writeLines(y, f, useBytes = TRUE)
      }
    }
  }

  cat("\n== Unwrap donttest summary ==\n")
  cat("Scanned:", length(files), "files\n")
  cat("Dry run:", dry_run, "\n")
  cat("Changed:", length(changed), "file(s)\n")
  if (length(changed)) cat(paste("  -", changed), sep = "\n")
  invisible(changed)
}

# CLI
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  dry <- if (length(args) && tolower(args[1]) %in% c("false","0","no")) FALSE else TRUE
  unwrap_donttest_all(dry_run = dry)
  if (!dry) cat("\nRe-run docs: devtools::document(); then devtools::check()\n")
}


# tools/patch_systemfile_backrefs.R
patch_systemfile_backrefs <- function(pkg = "twbparser",
                                      good_file = "test_for_wenjie.twb",
                                      root = ".", dry_run = TRUE) {
  r_dir <- file.path(root, "R")
  stopifnot(dir.exists(r_dir))
  files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)

  # Match: system.file("extdata", "<ARG>", package = "<pkg>")
  pat <- paste0('system\\.file\\(\\s*"extdata"\\s*,\\s*"([^"]*)"\\s*,\\s*package\\s*=\\s*"', pkg, '"\\s*\\)')

  need_fix <- function(arg) {
    arg == "" || grepl('^\\\\?\\d+$', arg)  # empty or a backref like \2 / \\2
  }

  ts <- format(Sys.time(), "%Y%m%d-%H%M%S")
  changed <- character()

  for (f in files) {
    x <- readLines(f, warn = FALSE, encoding = "UTF-8"); y <- x
    for (i in seq_along(y)) {
      ln <- y[i]
      m <- regexec(pat, ln, perl = TRUE); rm <- regmatches(ln, m)[[1]]
      if (length(rm)) {
        arg <- rm[2]
        if (need_fix(arg)) {
          fixed <- paste0('system.file("extdata", "', good_file, '", package = "', pkg, '")')
          y[i] <- sub(pat, fixed, ln, perl = TRUE)
        }
      }
    }
    if (!identical(x, y)) {
      changed <- c(changed, f)
      if (!dry_run) {
        bak <- sprintf("%s.%s.bak", f, ts)
        file.copy(f, bak, overwrite = TRUE)
        writeLines(y, f, useBytes = TRUE)
      }
    }
  }

  cat("Patched", length(changed), "file(s)\n")
  if (length(changed)) cat(paste(" -", changed), sep = "\n")
  invisible(changed)
}

# Usage:
# source("tools/patch_systemfile_backrefs.R")
# patch_systemfile_backrefs(dry_run = TRUE)   # preview
# patch_systemfile_backrefs(dry_run = FALSE)  # apply, writes .bak files
# devtools::document(); devtools::check()
