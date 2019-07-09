context("categories")

options(eia_antidos = 0)
key <- Sys.getenv("EIA_KEY")

test_that("category functions returns as expected", {
  message(getwd())
  if(key == "") skip("API key not available.")

  x <- eia_cats(key, tidy = FALSE)
  expect_is(x, "list")
  expect_equal(names(x), c("request", "category"))
  x <- eia_cats(key, tidy = NA)
  expect_is(x, "character")

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

test_that("series updates by category return as expected", {
  x1 <- eia_updates(key)
  x2 <- eia_updates(key, tidy = FALSE)
  x3 <- eia_updates(key, tidy = NA)
  expect_is(x1, "tbl_df")
  expect_equal(dim(x1), c(0, 2))
  expect_is(x2, "list")
  expect_is(x3, "character")

  x1 <- eia_updates(key, 742, n = 5)
  x2 <- eia_updates(key, 742, n = 5, TRUE)
  x3 <- eia_updates(key, 742)
  expect_is(x1, "tbl_df")
  expect_equal(dim(x1), c(5, 2))
  expect_equal(dim(x2), c(5, 2))
  expect_equal(dim(x3), c(50, 2))

  x1 <- eia_updates(key, 389, n = 1000)
  x2 <- eia_updates(key, 389, n = 1000, deep = TRUE)
  expect_equal(nrow(x1), 0)
  expect_true(nrow(x2) > 500)
})
