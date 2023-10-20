options(eia_antidos = 0)
suppressWarnings(key <- eia_get_key())

test_that("data queries returns as expected", {
  if(is.null(key)) skip("API key not available.")

  expect_warning(eia_data(dir = "electricity/retail-sales"))

  x <- eia_data(
    dir = "electricity/retail-sales",
    data = "price",
    facets = list(stateid = "OH", sectorid = "RES"),
    freq = "annual",
    start = "2011",
    end = "2020",
  )
  expect_s3_class(x, "tbl_df")
  expect_equal(ncol(x), 7)
  expect_equal(nrow(x), 10)
  expect_equal(unique(x$stateid), "OH")
  expect_equal(unique(x$sectorid), "RES")

})

test_that("metadata helper returns as expected", {
  if(is.null(key)) skip("API key not available.")

  x <- eia_metadata("electricity/retail-sales", cache = FALSE)
  expect_type(x, "list")

  x <- eia_metadata("electricity/retail-sales", tidy = FALSE, cache = FALSE)
  expect_type(x, "list")
  expect_length(x, 3)

})
