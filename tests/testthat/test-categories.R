context("categories")

key <- Sys.getenv("EIA_KEY")

test_that("category functions returns as expected", {
  message(getwd())
  if(key == "") skip("API key not available.")

  x <- eia_cats(key, tidy = FALSE)
  expect_is(x, "list")
  expect_equal(names(x), c("request", "category"))

  x <- eia_cats(key)
  expect_is(x, "list")
  expect_equal(names(x), c("category", "childcategories"))

  x <- eia_child_cats(key, 389)
  expect_is(x, "tbl_df")
  x <- eia_child_cats(key, 742)
  expect_is(x, "NULL")

  x <- eia_parent_cats(key, 742)
  expect_is(x, "tbl_df")
  expect_equal(dim(x), c(6, 4))
  x <- eia_parent_cats(key, 371)
  expect_is(x, "tbl_df")
  expect_equal(dim(x), c(1, 3))
})
