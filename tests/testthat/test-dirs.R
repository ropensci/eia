options(eia_antidos = 0)
suppressWarnings(key <- eia_get_key())

test_that("directory functions returns as expected", {
  if(is.null(key)) skip("API key not available.")

  x <- eia_dir(tidy = NA)
  expect_type(x, "character")

  x <- eia_dir(tidy = FALSE, key = key)
  expect_type(x, "list")
  expect_equal(names(x), c("response", "request", "apiVersion"))

  x2 <- eia_dir(tidy = FALSE)
  expect_identical(x, x2)
  x2 <- eia_dir(tidy = FALSE, cache = FALSE)
  expect_identical(x, x2)

  x <- eia_dir()
  expect_s3_class(x, "tbl_df")
  expect_equal(dim(x), c(14, 3))
  expect_equal(names(x), c("id", "name", "description"))

  x <- eia_dir("electricity")
  expect_s3_class(x, "tbl_df")
  expect_equal(dim(x), c(6, 3))

  suppressMessages(msg <- message(
    "No further sub-directories to discover.\n",
    "Use `eia_metadata(\"electricity/retail-sales\")` to explore this data."
  ))
  expect_message(eia_dir("electricity/retail-sales", cache = F), msg)

})

options(eia_antidos = 1)
