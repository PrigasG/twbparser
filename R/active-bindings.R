# Internal helpers for TwbParser active bindings
# nocov start

#' Install no-parens properties on a TwbParser instance
#' @keywords internal
#' @noRd
twb_install_active_properties <- function(x, cache = TRUE) {
  stopifnot("TwbParser" %in% class(x))

  # optional: idempotent guard
  if (isTRUE(x$.__active_installed__)) return(invisible(x))
  x$.__active_installed__ <- TRUE

  if (!exists(".cache", envir = x, inherits = FALSE)) {
    x$.cache <- new.env(parent = emptyenv())
  }

  # helper
  is_active <- function(nm, env) {
    if (!exists(nm, envir = env, inherits = FALSE)) return(FALSE)
    bindingIsActive(nm, env)
  }

  rebind <- function(name, getter) {
    # only call bindingIsActive if the name exists
    if (is_active(name, x)) {
      unlockBinding(name, x); rm(list = name, envir = x)
    } else if (exists(name, envir = x, inherits = FALSE) &&
               !is.function(get(name, envir = x, inherits = FALSE))) {
      # a concrete field lives here; do not replace
      return(invisible())
    } else if (exists(name, envir = x, inherits = FALSE)) {
      # preserve original callable as *_fn
      orig <- get(name, envir = x, inherits = FALSE)
      assign(paste0(name, "_fn"), orig, envir = x)
      rm(list = name, envir = x)
    }
    makeActiveBinding(name, getter, x)
    invisible()
  }

  wrap_cache <- function(key, f) {
    force(key); force(f)
    function() {
      if (cache && !is.null(x$.cache[[key]])) return(x$.cache[[key]])
      val <- f()
      if (cache) x$.cache[[key]] <- val
      val
    }
  }

  # summary as a property (prints when accessed)
  rebind("summary", wrap_cache("summary", function() { x$summary_fn(); invisible(NULL) }))

  # High-level read-only conveniences (new names; originals untouched)
  rebind("overview",          wrap_cache("overview",          function() x$get_overview()))
  rebind("pages",             wrap_cache("pages",             function() .ins_pages(x$xml_doc)))
  rebind("pages_summary",     wrap_cache("pages_summary",     function() .ins_pages_summary(x$xml_doc)))
  rebind("dashboard_summary", wrap_cache("dashboard_summary", function() .ins_dashboard_summary(x$xml_doc)))


  # Safe getters as properties (same names; originals stashed as *_fn)
  rebind("relations", wrap_cache("relations", function() x$get_relations_fn()))
  rebind("joins", wrap_cache("joins", function() x$get_joins_fn()))
  rebind("relationships", wrap_cache("relationships", function() x$get_relationships_fn()))
  rebind("inferred_relationships", wrap_cache("inferred_relationships", function() x$get_inferred_relationships_fn()))
  # Keep only NEW read-only properties (snapshots). Do NOT override get_* methods.

  # Data snapshot properties (NEW names; originals remain callable as get_*())
  rebind("datasources",        wrap_cache("datasources",        function() x$get_datasources()))
  rebind("parameters_tbl",     wrap_cache("parameters_tbl",     function() x$get_parameters()))
  rebind("datasources_all",    wrap_cache("datasources_all",    function() x$get_datasources_all()))
  rebind("fields_tbl",         wrap_cache("fields_tbl",         function() x$get_fields()))
  rebind("custom_sql_tbl",     wrap_cache("custom_sql_tbl",     function() x$get_custom_sql()))
  rebind("initial_sql_tbl",    wrap_cache("initial_sql_tbl",    function() x$get_initial_sql()))
  rebind("published_refs_tbl", wrap_cache("published_refs_tbl", function() x$get_published_refs()))
  rebind("twbx_manifest_tbl",  wrap_cache("twbx_manifest_tbl",  function() x$get_twbx_manifest()))
  rebind("twbx_extracts_tbl",  wrap_cache("twbx_extracts_tbl",  function() x$get_twbx_extracts()))
  rebind("twbx_images_tbl",    wrap_cache("twbx_images_tbl",    function() x$get_twbx_images()))


  # Validation snapshot (read-only); runs a lenient validate once if missing
  rebind("validation", wrap_cache("validation", function() {
    if (is.null(x$last_validation)) invisible(x$validate_fn(error = FALSE))
    x$last_validation %||% list(ok = NA, issues = tibble::tibble())
  }))

  invisible(x)
}

#' Invalidate cached no-parens properties
#' @keywords internal
#' @noRd
twb_invalidate_active_cache <- function(x, which = NULL) {
  if (!exists(".cache", envir = x, inherits = FALSE)) return(invisible(x))
  if (is.null(which)) {
    rm(list = ls(envir = x$.cache, all.names = TRUE), envir = x$.cache)
  } else {
    rm(list = intersect(which, ls(envir = x$.cache, all.names = TRUE)), envir = x$.cache)
  }
  invisible(x)
}

