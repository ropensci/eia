context("categories")

options(eia_antidos = 0)
suppressWarnings(key <- eia_get_key())

test_that("category functions returns as expected", {
  if(is.null(key)) skip("API key not available.")

  x <- eia_cats(tidy = FALSE, key = key)
  expect_is(x, "list")
  expect_equal(names(x), c("request", "category"))

  x2 <- eia_cats(tidy = FALSE)
  expect_identical(x, x2)

  x <- eia_cats(tidy = NA)
  expect_is(x, "character")

  x <- eia_cats()
  expect_is(x, "list")
  expect_equal(names(x), c("category", "childcategories"))

  x <- eia_child_cats(389)
  expect_is(x, "tbl_df")
  x <- eia_child_cats(742)
  expect_is(x, "NULL")

  x <- eia_parent_cats(742)
  expect_is(x, "tbl_df")
  expect_equal(dim(x), c(6, 4))
  x <- eia_parent_cats(371)
  expect_is(x, "tbl_df")
  expect_equal(dim(x), c(1, 3))
})

test_that("series updates by category return as expected", {
  if(is.null(key)) skip("API key not available.")

  x1 <- eia_updates()
  x2 <- eia_updates(tidy = FALSE)
  x3 <- eia_updates(tidy = NA)
  expect_is(x1, "tbl_df")
  expect_equal(dim(x1), c(0, 2))
  expect_is(x2, "list")
  expect_is(x3, "character")

  x1 <- eia_updates(742, n = 5)
  x2 <- eia_updates(742, n = 5, TRUE)
  x3 <- eia_updates(742)
  expect_is(x1, "tbl_df")
  expect_equal(dim(x1), c(5, 2))
  expect_equal(dim(x2), c(5, 2))
  expect_equal(dim(x3), c(50, 2))

  x1 <- eia_updates(389, n = 1000)
  x2 <- eia_updates(389, n = 1000, deep = TRUE)
  expect_equal(nrow(x1), 0)
  expect_true(nrow(x2) > 500)
})
