test_that("plot_relationship_graph uses left_field for 'from' edge (not right_field)", {
  rels <- tibble::tibble(
    left_table  = "Orders",
    left_field  = "OrderID",
    right_table = "Returns",
    right_field = "ReturnID",
    operator    = "="
  )
  g <- plot_relationship_graph(rels, seed = 1L)
  vnames <- igraph::V(g)$name
  expect_true(any(grepl("Orders\\.OrderID",   vnames)))
  expect_true(any(grepl("Returns\\.ReturnID", vnames)))
  # must NOT contain left_table.right_field (old bug)
  expect_false(any(grepl("Orders\\.ReturnID", vnames)))
})

test_that("plot_source_join_graph uses left_table/right_table columns", {
  joins <- tibble::tibble(
    left_table  = "Orders",
    left_field  = "OrderID",
    right_table = "Returns",
    right_field = "ReturnID",
    join_type   = "inner",
    operator    = "="
  )
  rels <- tibble::tibble(
    left_table  = "Orders",
    right_table = "Returns",
    left_field  = "OrderID",
    right_field = "ReturnID",
    operator    = "="
  )
  # Should not error
  g_list <- expect_no_error(plot_source_join_graph(joins, rels, seed = 1L))
  expect_true(is.list(g_list) || inherits(g_list, "igraph"))
})

test_that("infer_implicit_relationships does not explode on duplicate field names", {
  # create a scenario with many tables sharing the same field name
  fields <- tibble::tibble(
    datasource  = rep("ds", 12L),
    name        = rep(c("[id]", "[name]", "[value]"), 4L),
    table_clean = rep(c("A", "B", "C", "D"), each = 3L),
    field_clean = rep(c("id", "name", "value"), 4L),
    semantic_role = NA_character_,
    is_parameter = FALSE
  )
  out <- infer_implicit_relationships(fields)
  expect_s3_class(out, "tbl_df")
  # no duplicated canonical pairs
  pairs <- paste(
    pmin(paste0(out$left_table, ".", out$left_field),
         paste0(out$right_table, ".", out$right_field)),
    pmax(paste0(out$left_table, ".", out$left_field),
         paste0(out$right_table, ".", out$right_field)),
    sep = "||"
  )
  expect_equal(length(pairs), length(unique(pairs)))
})
