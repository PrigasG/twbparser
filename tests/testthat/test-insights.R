test_that("pages and summaries do not error", {
  p <- TwbParser$new(system.file("extdata", "test_for_wenjie.twb", package = "twbparser"))
  expect_true(is.data.frame(p$get_pages()))
  expect_true(is.data.frame(p$get_pages_summary()))
  expect_true(is.data.frame(p$get_dashboard_summary()))
})

test_that("page composition returns a tibble for existing pages", {
  p <- TwbParser$new(system.file("extdata", "test_for_wenjie.twb", package = "twbparser"))
  pages <- p$get_pages()
  if (nrow(pages)) {
    nm <- pages$name[[1]]
    comp <- p$get_page_composition(nm)
    expect_true(is.data.frame(comp))
  } else {
    succeed()
  }
})
