# Internal helpers for TwbParser active bindings
# nocov start

#' Install no-parens properties on a TwbParser instance
#' @keywords internal
#' @noRd
twb_install_active_properties <- function(x, cache = TRUE) {
  stopifnot(inherits(x, "TwbParser"))

  # idempotent guard
  if (isTRUE(x$.__active_installed__)) return(invisible(x))
  x$.__active_installed__ <- TRUE

  # cache env
  if (!exists(".cache", envir = x, inherits = FALSE) || !is.environment(x$.cache)) {
    x$.cache <- new.env(parent = emptyenv())
  }

  is_active <- function(nm, env) {
    if (!exists(nm, envir = env, inherits = FALSE)) return(FALSE)
    bindingIsActive(nm, env)
  }

  rebind <- function(name, getter, keep_function = TRUE) {
    # if it's already an active binding, assume it's ours and leave it alone
    if (is_active(name, x)) {
      return(invisible())
    }

    if (exists(name, envir = x, inherits = FALSE)) {
      val <- get(name, envir = x, inherits = FALSE)

      # concrete field: never overwrite
      if (!is.function(val)) {
        return(invisible())
      }

      # stash original callable as *_fn (once)
      if (keep_function) {
        fn_name <- paste0(name, "_fn")
        if (!exists(fn_name, envir = x, inherits = FALSE)) {
          assign(fn_name, val, envir = x)
        }
      }

      # remove original symbol so we can bind actively
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

  ## summary as a property (prints when accessed)
  rebind(
    "summary",
    wrap_cache("summary", function() { x$summary_fn(); invisible(NULL) })
  )

  ## High-level read-only conveniences (new names; originals untouched)
  rebind("overview",          wrap_cache("overview",          function() x$get_overview()))
  rebind("pages",             wrap_cache("pages",             function() .ins_pages(x$xml_doc)))
  rebind("pages_summary",     wrap_cache("pages_summary",     function() .ins_pages_summary(x$xml_doc)))
  rebind("dashboard_summary", wrap_cache("dashboard_summary", function() .ins_dashboard_summary(x$xml_doc)))

  ## Safe getters as properties (same names; originals stashed as *_fn)
  rebind("relations",              wrap_cache("relations",              function() x$get_relations_fn()))
  rebind("joins",                  wrap_cache("joins",                  function() x$get_joins_fn()))
  rebind("relationships",          wrap_cache("relationships",          function() x$get_relationships_fn()))
  rebind("inferred_relationships", wrap_cache("inferred_relationships", function() x$get_inferred_relationships_fn()))

  ## Data snapshot properties (NEW names; originals remain callable as get_*())
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

  ## Phase 2/3: sheet & dashboard intelligence
  rebind("sheet_shelves",      wrap_cache("sheet_shelves",      function() x$get_sheet_shelves()))
  rebind("sheet_filters",      wrap_cache("sheet_filters",      function() x$get_sheet_filters()))
  rebind("sheet_axes",         wrap_cache("sheet_axes",         function() x$get_sheet_axes()))
  rebind("sheet_sorts",        wrap_cache("sheet_sorts",        function() x$get_sheet_sorts()))
  rebind("dashboard_sheets",   wrap_cache("dashboard_sheets",   function() x$get_dashboard_sheets()))
  rebind("dashboard_layout",   wrap_cache("dashboard_layout",   function() x$get_dashboard_layout()))
  rebind("dashboard_actions",  wrap_cache("dashboard_actions",  function() x$get_dashboard_actions()))

  ## Validation snapshot (read-only)
  rebind(
    "validation",
    wrap_cache("validation", function() {
      if (is.null(x$last_validation)) invisible(x$validate_fn(error = FALSE))
      x$last_validation %||% list(ok = NA, issues = tibble::tibble())
    })
  )

  invisible(x)
}

#' Invalidate cached no-parens properties
#' @keywords internal
#' @noRd
twb_invalidate_active_cache <- function(x, which = NULL) {
  if (!exists(".cache", envir = x, inherits = FALSE) || !is.environment(x$.cache)) {
    return(invisible(x))
  }
  if (is.null(which)) {
    rm(list = ls(envir = x$.cache, all.names = TRUE), envir = x$.cache)
  } else {
    rm(list = intersect(which, ls(envir = x$.cache, all.names = TRUE)), envir = x$.cache)
  }
  invisible(x)
}
# nocov end
