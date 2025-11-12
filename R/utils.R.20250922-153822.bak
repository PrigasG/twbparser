#' Null-coalescing helper
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Escape square/round brackets with backslashes
#' @param string Character vector (or NULL)
#' @return Character vector with [], () escaped; NULL passes through.
#' @keywords internal
add_back_slash <- function(string) {
  if (is.null(string)) {
    return(string)
  }
  stringr::str_replace_all(string, "([\\[\\]\\(\\)])", "\\\\\\1")
}

#' Wrap a string in Tableau-style square brackets
#' @param string Character vector; NA/NULL returns `NA_character_`
#' @return Character vector wrapped like `[name]`
#' @keywords internal
put_in_sq_bracket <- function(string) {
  if (is.null(string) || is.na(string)) {
    return(NA_character_)
  }
  paste0("[", string, "]")
}

#' Safely extract an attribute from a named list
#' @param attrs Named list (e.g., `xml2::xml_attrs()` result)
#' @param name Attribute to retrieve
#' @param default Fallback value
#' @return Scalar character
#' @keywords internal
attr_safe_get <- function(attrs, name, default = NA_character_) {
  if (!is.null(attrs) && name %in% names(attrs)) attrs[[name]] else default
}

#' Safe basename with fallback
#' @param x Path or file name
#' @param fallback Value to use if `x` is NULL/NA/empty
#' @return Basename or fallback
#' @keywords internal
basename_safe <- function(x, fallback = "<unknown>") {
  if (is.null(x) || is.na(x) || !nzchar(x)) {
    return(fallback)
  }
  base::basename(x)
}

#' Is an xml2 node missing?
#' @param node An `xml2` node
#' @return TRUE/FALSE
#' @keywords internal
is_xml_missing <- function(node) {
  inherits(node, "xml_missing") || isTRUE(xml2::xml_type(node) != "element")
}

#' Safely evaluate and return fallback on error (with warning)
#' @param expr Expression to evaluate
#' @param fallback Value if an error occurs
#' @return Result of `expr` or `fallback`
#' @keywords internal
safe_call <- function(expr, fallback) {
  tryCatch(expr, error = function(e) {
    warning(conditionMessage(e), call. = FALSE)
    fallback
  })
}

#' Classify a TWBX entry by file extension
#' @param name Path/filename
#' @return One of "workbook","extract","image","text","excel","other"
#' @keywords internal
.twbx_classify <- function(name) {
  ext <- tolower(tools::file_ext(name))
  dplyr::case_when(
    ext == "twb" ~ "workbook",
    ext %in% c("hyper", "tde") ~ "extract",
    ext %in% c("png", "jpg", "jpeg", "gif", "svg") ~ "image",
    ext %in% c("csv", "txt", "tsv") ~ "text",
    ext %in% c("xlsx", "xls") ~ "excel",
    TRUE ~ "other"
  )
}

#' List contents of a Tableau .twbx
#'
#' @param twbx_path Path to a `.twbx` file.
#' @return Tibble with columns: `name`, `size_bytes`, `modified`, `type`.
#' @examples
#' \dontrun{
#' twbx_list(system.file("extdata", "example.twbx", package = "twbparser"))
#' }
#' @export
twbx_list <- function(twbx_path) {
  tibble::as_tibble(unzip(twbx_path, list = TRUE)) |>
    dplyr::transmute(
      name       = Name,
      size_bytes = as.double(Length),
      modified   = Date,
      type       = .twbx_classify(Name)
    )
}

#' Extract the .twb (and optionally all files) from a .twbx
#'
#' @param twbx_path Path to a `.twbx` file.
#' @param extract_dir Directory to extract into (defaults to a timestamped temp dir).
#' @param extract_all If `TRUE`, extract entire archive; otherwise only the largest `.twb`.
#' @return List with `twb_path`, `exdir`, `twbx_path`, and `manifest` (tibble).
#' @export
extract_twb_from_twbx <- function(
    twbx_path,
    extract_dir = file.path(
      tempdir(),
      paste0("twbx_", tools::file_path_sans_ext(basename(twbx_path)), "_", format(Sys.time(), "%Y%m%d%H%M%S"))
    ),
    extract_all = FALSE) {
  if (!file.exists(twbx_path)) stop("File does not exist: ", twbx_path)
  manifest <- twbx_list(twbx_path)

  twb_rows <- manifest |>
    dplyr::filter(type == "workbook") |>
    dplyr::arrange(dplyr::desc(size_bytes))

  if (nrow(twb_rows) == 0) stop("No .twb file found inside .twbx")

  twb_rel <- twb_rows$name[[1]]

  dir.create(extract_dir, showWarnings = FALSE, recursive = TRUE)
  if (isTRUE(extract_all)) {
    unzip(twbx_path, exdir = extract_dir, junkpaths = FALSE)
  } else {
    unzip(twbx_path, files = twb_rel, exdir = extract_dir, junkpaths = FALSE)
  }
  twb_path <- file.path(extract_dir, twb_rel)
  message("Extracted .twb from .twbx: ", basename(twb_path))

  list(twb_path = twb_path, exdir = extract_dir, twbx_path = normalizePath(twbx_path), manifest = manifest)
}

#' Extract specific files from a .twbx
#'
#' @param twbx_path Path to a `.twbx`.
#' @param files Vector of archive paths to extract (optional).
#' @param pattern Perl regex to match archive paths (optional).
#' @param types Subset by `.twbx` entry `type` (see [twbx_list()]) (optional).
#' @param exdir Output directory (defaults to temp).
#' @return Tibble with `name`, `type`, and `out_path` of extracted files.
#' @export
twbx_extract_files <- function(twbx_path, files = NULL, pattern = NULL, types = NULL, exdir = NULL) {
  stopifnot(file.exists(twbx_path))
  man <- twbx_list(twbx_path)

  sel <- man
  if (!is.null(types)) sel <- dplyr::filter(sel, type %in% types)
  if (!is.null(pattern)) sel <- dplyr::filter(sel, grepl(pattern, name, perl = TRUE))
  if (!is.null(files)) sel <- dplyr::filter(sel, name %in% files)
  if (nrow(sel) == 0) {
    return(tibble::tibble(name = character(), out_path = character(), type = character()))
  }

  if (is.null(exdir)) {
    exdir <- file.path(tempdir(), paste0("twbx_extract_", format(Sys.time(), "%Y%m%d%H%M%S")))
  }
  dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
  unzip(twbx_path, files = sel$name, exdir = exdir, junkpaths = FALSE)

  tibble::tibble(
    name     = sel$name,
    type     = sel$type,
    out_path = file.path(exdir, sel$name)
  )
}

#' Log a one-line summary of .twbx contents
#' @param twbx_path Path to a `.twbx`
#' @return (Invisibly) the manifest tibble
#' @keywords internal
log_twbx_contents <- function(twbx_path) {
  man <- twbx_list(twbx_path)
  cat("Contents of .twbx:", basename(twbx_path), "\n")
  print(man |> dplyr::count(type, sort = TRUE), n = 99)
  invisible(man)
}

#' Print a quick data-source summary from a parser object
#' @param parser An object with `get_datasources()`, `get_parameters()`, `get_datasources_all()`
#' @return Invisibly prints summary
#' @keywords internal
print_datasource_summary <- function(parser) {
  cat("DATA SOURCES SUMMARY\n")
  cat("=======================\n")

  data_sources <- try(parser$get_datasources(), silent = TRUE)
  parameters <- try(parser$get_parameters(), silent = TRUE)
  all_sources <- try(parser$get_datasources_all(), silent = TRUE)

  .p <- function(x, title) {
    cat(title, "\n")
    if (inherits(x, "try-error") || is.null(x) || NROW(x) == 0) {
      cat("  (none)\n\n")
    } else {
      print(x, n = NROW(x))
      cat("\n")
    }
  }

  .p(data_sources, "Real Data Sources:")
  .p(parameters, "Parameter Sets:")
  .p(all_sources, "All Raw Sources (Unfiltered)::")

  n_ds <- if (!inherits(data_sources, "try-error") && !is.null(data_sources)) NROW(data_sources) else 0L
  n_pa <- if (!inherits(parameters, "try-error") && !is.null(parameters)) NROW(parameters) else 0L
  n_al <- if (!inherits(all_sources, "try-error") && !is.null(all_sources)) NROW(all_sources) else 0L

  cat("Totals:\n")
  cat(sprintf("%d real datasource(s)\n", n_ds))
  cat(sprintf("%d parameter set(s)\n", n_pa))
  cat(sprintf("%d total source(s) in workbook\n", n_al))
}

#' Extract \verb{<named-connection>} entries from a TWB
#'
#' Rich, safe extraction of \verb{<named-connection>} nodes and their \verb{<connection>}
#' attributes into a tidy tibble.
#'
#' @param xml_doc An `xml2` document for a Tableau `.twb`.
#' @return Tibble with columns like `connection_id`, `connection_caption`,
#'   `connection_class`, `connection_target`, `dbname`, `schema`, `warehouse`,
#'   `region`, `filename`, and `location_named`.
#' @examples
#' \dontrun{
#' xml <- xml2::read_xml("inst/extdata/sample.twb")
#' extract_named_connections(xml)
#' }
#' @export
extract_named_connections <- function(xml_doc) {
  ncs <- xml2::xml_find_all(xml_doc, "//named-connection")
  if (length(ncs) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(ncs, function(nc) {
    id <- xml2::xml_attr(nc, "name")
    cap <- xml2::xml_attr(nc, "caption") %||% id

    conn <- xml2::xml_find_first(nc, "./connection")
    a <- if (!is_xml_missing(conn)) xml2::xml_attrs(conn) else list()

    cls <- attr_safe_get(a, "class")
    server <- attr_safe_get(a, "server")
    dir <- attr_safe_get(a, "directory")
    fn <- attr_safe_get(a, "filename")
    dbname <- attr_safe_get(a, "dbname")
    schema <- attr_safe_get(a, "schema")
    warehouse <- attr_safe_get(a, "warehouse")

    target <- dplyr::coalesce(server, dir, fn)

    region <- if (!is.na(server) && grepl("athena\\.", server, ignore.case = TRUE)) {
      sub(".*athena\\.([^.]+)\\..*", "\\1", tolower(server))
    } else {
      NA_character_
    }

    location_named <- dplyr::case_when(
      identical(cls, "athena") ~ paste0(
        "Athena",
        ifelse(!is.na(region), paste0(" (", region, ")"), ""),
        ": ",
        dbname %||% "<catalog>",
        ifelse(!is.na(schema) && nzchar(schema), paste0(".", schema), "")
      ),
      identical(cls, "ogrdirect") ~ paste0(" Shapefile: ", basename_safe(fn)),
      identical(cls, "excel") ~ paste0("Excel: ", basename_safe(fn)),
      identical(cls, "textscan") ~ paste0("CSV: ", basename_safe(fn)),
      TRUE ~ paste0(
        "No Known File ", stringr::str_to_title(cls %||% "Unknown"),
        ": ", target %||% "<unknown>"
      )
    )

    tibble::tibble(
      connection_id      = id,
      connection_caption = cap,
      connection_class   = cls,
      connection_target  = target,
      dbname             = dbname,
      schema             = schema,
      warehouse          = warehouse,
      region             = region,
      filename           = fn,
      location_named     = location_named
    )
  })
}

#' Redact AWS access keys in strings
#' @param x Character vector
#' @return Character vector with keys replaced by `[REDACTED_AWS_KEY]`
#' @keywords internal
redact <- function(x) gsub("\\bAKIA[0-9A-Z]{16}\\b", "[REDACTED_AWS_KEY]", x %||% "")
