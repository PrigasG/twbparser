#' Tableau Workbook Parser (R6)
#'
#' Create a parser for Tableau `.twb` / `.twbx` files. On initialization, the
#' parser reads the XML and precomputes relationships, joins, fields, calculated
#' fields, inferred relationships, and datasource details. For `.twbx`, it also
#' extracts the largest `.twb` and records a manifest.
#'
#' @format An R6 class generator.
#'
#' @section Fields:
#' \describe{
#'   \item{path}{Path to the `.twb` or `.twbx` file on disk.}
#'   \item{xml_doc}{Parsed `xml2` document of the workbook.}
#'   \item{twbx_path}{Original `.twbx` path if the workbook was packaged.}
#'   \item{twbx_dir}{Directory where the `.twbx` was extracted.}
#'   \item{twbx_manifest}{Tibble of `.twbx` contents from `twbx_list()`.}
#'   \item{relations}{Tibble of \verb{<relation>} nodes from `extract_relations()`.}
#'   \item{joins}{Tibble of join clauses from `extract_joins()`.}
#'   \item{relationships}{Tibble of modern relationships from `extract_relationships()`.}
#'   \item{inferred_relationships}{Tibble of inferred relationship pairs by name and role.}
#'   \item{datasource_details}{List containing `data_sources`, `parameters`, and `all_sources`.}
#'   \item{fields}{Tibble of raw fields with table information.}
#'   \item{calculated_fields}{Tibble of calculated fields.}
#'   \item{last_validation}{Result from `validate()` as list with `ok` and `issues` elements.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{new(path)}{Create a parser from `.twb` or `.twbx` file.}
#'   \item{get_twbx_manifest()}{Return `.twbx` manifest tibble.}
#'   \item{get_twbx_extracts()}{Return `.twbx` extract entries.}
#'   \item{get_twbx_images()}{Return `.twbx` image entries.}
#'   \item{extract_twbx_assets(types, pattern, files, exdir)}{Extract files from `.twbx` archive.}
#'   \item{get_relations()}{Return relations tibble.}
#'   \item{get_joins()}{Return joins tibble.}
#'   \item{get_relationships()}{Return modern relationships tibble.}
#'   \item{get_inferred_relationships()}{Return inferred relationship pairs.}
#'   \item{get_datasources()}{Return datasource details tibble.}
#'   \item{get_parameters()}{Return parameters tibble.}
#'   \item{get_datasources_all()}{Return all sources tibble.}
#'   \item{get_fields()}{Return raw fields tibble.}
#'   \item{get_calculated_fields(pretty = FALSE, strip_brackets = FALSE, wrap = 100L)}{
#'     Return calculated fields tibble. When `pretty = TRUE`, includes a
#'     `formula_pretty` column with line breaks and indentation.
#'   }
#'   \item{validate(error = FALSE)}{Validate relationships. Stops execution if `error = TRUE`.}
#'   \item{summary()}{Print a brief summary to console.}
#' }
#'
#' @name TwbParser
#' @aliases TwbParser TWBParser
#' @export
TwbParser <- R6::R6Class(
  "TwbParser",
  public = list(
    # state
    path = NULL,
    xml_doc = NULL,

    # twbx
    twbx_path = NULL,
    twbx_dir = NULL,
    twbx_manifest = NULL,

    # caches
    relations = NULL,
    joins = NULL,
    relationships = NULL,
    inferred_relationships = NULL,
    datasource_details = NULL,
    fields = NULL,
    calculated_fields = NULL,
    last_validation = NULL,
    custom_sql = NULL,
    initial_sql = NULL,
    published_refs = NULL,
    # publish_info_cache = NULL,

    #' @description
    #' Initialize the parser from a `.twb` or `.twbx` path.
    #' @param path Path to a `.twb` or `.twbx` file.
    initialize = function(path) {
      if (!file.exists(path)) stop("File not found: ", path)

      ext <- tolower(tools::file_ext(path))
      if (ext == "twbx") {
        info <- extract_twb_from_twbx(path, extract_all = FALSE)
        path <- info$twb_path
        self$twbx_dir <- info$exdir
        self$twbx_path <- info$twbx_path
        self$twbx_manifest <- info$manifest
      } else if (ext != "twb") {
        stop("Unsupported file type: ", ext)
      }

      self$path <- path
      self$xml_doc <- xml2::read_xml(path)
      message("TWB loaded: ", basename(path))

      # caches (each safe-guarded)
      self$relations <- safe_call(extract_relations(self$xml_doc), tibble::tibble())
      self$joins <- safe_call(extract_joins(self$xml_doc), tibble::tibble())
      self$relationships <- safe_call(extract_relationships(self$xml_doc), tibble::tibble())
      self$fields <- safe_call(extract_columns_with_table_source(self$xml_doc), tibble::tibble())
      self$inferred_relationships <- safe_call(infer_implicit_relationships(self$fields), tibble::tibble())
      self$datasource_details <- safe_call(
        extract_datasource_details(self$xml_doc),
        list(
          data_sources = tibble::tibble(),
          parameters   = tibble::tibble(),
          all_sources  = tibble::tibble()
        )
      )
      self$calculated_fields <- safe_call(extract_calculated_fields(self$xml_doc), tibble::tibble())
      self$custom_sql <- safe_call(twb_custom_sql(self$xml_doc), tibble::tibble())
      self$initial_sql <- safe_call(twb_initial_sql(self$xml_doc), tibble::tibble())
      self$published_refs <- safe_call(twb_published_refs(self$xml_doc), tibble::tibble())


      message("TWB parsed and ready")
    },

    # --- TWBX helpers ---
    #' @description Return the TWBX manifest (if available).
    get_twbx_manifest = function() {
      self$twbx_manifest %||% tibble::tibble()
    },

    #' @description Return TWBX extract entries.
    get_twbx_extracts = function() {
      man <- self$get_twbx_manifest()
      if (nrow(man) == 0) {
        return(man)
      }
      dplyr::filter(man, type == "extract")
    },

    #' @description Return TWBX image entries.
    get_twbx_images = function() {
      man <- self$get_twbx_manifest()
      if (nrow(man) == 0) {
        return(man)
      }
      dplyr::filter(man, type == "image")
    },

    #' @description Extract files from the TWBX to disk.
    #' @param types Optional vector of types (e.g., `"image"`, `"extract"`).
    #' @param pattern Optional regex to match archive paths.
    #' @param files Optional explicit archive paths to extract.
    #' @param exdir Output directory (defaults to parser's twbx dir or tempdir()).
    extract_twbx_assets = function(types = NULL, pattern = NULL, files = NULL, exdir = NULL) {
      if (is.null(self$twbx_path) || !file.exists(self$twbx_path)) {
        stop("No TWBX path recorded. Re-open from a .twbx or call twbx_extract_files() with an explicit path.")
      }
      twbx_extract_files(
        self$twbx_path,
        files   = files,
        pattern = pattern,
        types   = types,
        exdir   = exdir %||% self$twbx_dir %||% tempdir()
      )
    },

    # --- accessors  ---
    get_relations = function() self$relations,
    get_joins = function() self$joins,
    get_relationships = function() self$relationships,
    get_inferred_relationships = function() self$inferred_relationships,
    get_datasources = function() self$datasource_details$data_sources,
    get_parameters = function() self$datasource_details$parameters,
    get_datasources_all = function() self$datasource_details$all_sources,
    get_fields = function() self$fields,
    #get_calculated_fields = function() self$calculated_fields,
    get_custom_sql = function() self$custom_sql,
    get_initial_sql = function() self$initial_sql,
    get_published_refs = function() self$published_refs,
    get_calculated_fields = function(pretty = FALSE, strip_brackets = FALSE, wrap = 100L) {
      if (!isTRUE(pretty)) return(self$calculated_fields)
      df <- prettify_calculated_fields(self$calculated_fields, strip_brackets = strip_brackets, wrap = wrap)
      df |>
        dplyr::select(
          datasource, name, datatype, role, is_table_calc,calc_class,
          formula_pretty, tableau_internal_name, table_clean
        )
    },

    # --- validator bridge ---
    #' @description Validate relationships; optionally stop on failure.
    #' @param error If `TRUE`, `stop()` when validation fails.
    validate = function(error = FALSE) {
      v <- validate_relationships(self) # lenient by default
      self$last_validation <- v
      if (isTRUE(error) && !v$ok) {
        stop("Validation failed. See parser$last_validation$issues.", call. = FALSE)
      }
      invisible(v)
    },

    # --- summary ---
    #' @description Print a one‑line summary of parsed content.
    summary = function() {
      cat("TWB PARSER SUMMARY\n")
      cat("---------------------\n")
      cat(sprintf("File: %s\n", basename(self$path)))
      cat(sprintf("Datasources: %d\n", NROW(self$datasource_details$data_sources)))
      cat(sprintf("Parameters:  %d\n", NROW(self$datasource_details$parameters)))
      cat(sprintf("Relationships: %d\n", NROW(self$relationships)))
      cat(sprintf("Calculated fields: %d\n", NROW(self$calculated_fields)))
      cat(sprintf("Raw fields: %d\n", NROW(self$fields)))
      cat(sprintf("Inferred joins: %d\n", NROW(self$inferred_relationships)))
      invisible(NULL)
    }
  )
)
