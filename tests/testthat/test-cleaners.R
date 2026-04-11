test_that(".twb_clean_table removes brackets, prefix, hex suffix", {
  expect_equal(.twb_clean_table("[Extract].[Orders_A1B2C3D4E5F6A1B2C3D4E5F6A1B2C3D4]"),
               "Orders")
  expect_equal(.twb_clean_table("[Orders]"), "Orders")
  expect_equal(.twb_clean_table(NA_character_),   NA_character_)
  expect_equal(.twb_clean_table(""),              NA_character_)
  # vectorised
  out <- .twb_clean_table(c("[A]", NA, "[Extract].[B]"))
  expect_equal(out, c("A", NA_character_, "B"))
})

test_that(".twb_clean_field strips brackets and derivation prefixes", {
  expect_equal(.twb_clean_field("[none:Category:nk]"),  "Category")
  expect_equal(.twb_clean_field("[clct:Geometry:ok]"),  "Geometry")
  expect_equal(.twb_clean_field("[Sales]"),             "Sales")
  expect_equal(.twb_clean_field("[ds].[Field]"),        "Field")
  expect_equal(.twb_clean_field(NA_character_),         NA_character_)
  # vectorised
  out <- .twb_clean_field(c("[none:Sales:qk]", NA, "[Region]"))
  expect_equal(out, c("Sales", NA_character_, "Region"))
})

test_that(".twb_clean_table and .twb_clean_field are idempotent on clean input", {
  expect_equal(.twb_clean_table("Orders"), "Orders")
  expect_equal(.twb_clean_field("Sales"),  "Sales")
})
