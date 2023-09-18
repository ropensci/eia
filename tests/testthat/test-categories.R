context("categories")

options(eia_antidos = 0)
suppressWarnings(key <- eia_get_key())

test_that("category functions returns as expected", {
  if(is.null(key)) skip("API key not available.")

  x <- eia_categories(tidy = FALSE, key = key)
  expect_is(x, "list")
  expect_equal(names(x), c("response", "request", "apiVersion"))

  x2 <- eia_categories(tidy = FALSE)
  expect_identical(x, x2)
  x2 <- eia_categories(tidy = FALSE, cache = FALSE)
  expect_identical(x, x2)

  x <- eia_categories(tidy = NA)
  expect_is(x, "character")

  x <- eia_categories()
  expect_is(x, "tbl_df")
  expect_equal(dim(x), c(14, 3))
  expect_equal(names(x), c("id", "name", "description"))

  x <- eia_subcategories("electricity")
  expect_is(x, "tbl_df")
  expect_equal(dim(x), c(6, 3))
})
