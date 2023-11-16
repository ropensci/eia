options(eia_antidos = 1)
suppressWarnings(key <- eia_get_key())

test_that("metadata helper returns as expected", {
  if(is.null(key)) skip("API key not available.")

  x <- eia_metadata("electricity/retail-sales", cache = FALSE)
  expect_type(x, "list")

  x <- eia_metadata("electricity/retail-sales", tidy = FALSE, cache = FALSE)
  expect_type(x, "list")
  expect_length(x, 4)
  expect_equal(names(x), c("response", "request", "apiVersion", "ExcelAddInVersion"))

  x <- eia_metadata("electricity/retail-sales", tidy = NA, cache = FALSE)
  expect_type(x, "character")

})
