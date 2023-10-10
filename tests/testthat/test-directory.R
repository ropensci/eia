context("directory")

options(eia_antidos = 0)
suppressWarnings(key <- eia_get_key())

test_that("category functions returns as expected", {
  if(is.null(key)) skip("API key not available.")

  x <- eia_directory(tidy = FALSE, key = key)
  expect_type(x, "list")
  expect_equal(names(x), c("response", "request", "apiVersion"))

  x2 <- eia_directory(tidy = FALSE)
  expect_identical(x, x2)
  x2 <- eia_directory(tidy = FALSE, cache = FALSE)
  expect_identical(x, x2)

  x <- eia_directory(tidy = NA)
  expect_type(x, "character")

  x <- eia_directory()
  expect_s3_class(x, "tbl_df")
  expect_equal(dim(x), c(14, 3))
  expect_equal(names(x), c("id", "name", "description"))

  x <- eia_directory("electricity")
  expect_s3_class(x, "tbl_df")
  expect_equal(dim(x), c(6, 3))
})
