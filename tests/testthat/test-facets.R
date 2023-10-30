suppressWarnings(key <- eia_get_key())

test_that("facets working as expected", {
  x <- eia_facets("electricity/retail-sales", facet = "sectorid")
  expect_s3_class(x, "tbl_df")
  expect_equal(ncol(x), 3)
  expect_equal(nrow(x), 6)

  # Test JSON list returned given `tidy = FALSE`
  x <- eia_facets("electricity/retail-sales", facet = "sectorid", tidy = FALSE)
  expect_type(x, "list")
  expect_equal(length(x), 3)
  expect_equal(names(x), c("response", "request", "apiVersion"))

  # Test character object returned given `tidy = NA`
  x <- eia_facets("electricity/retail-sales", facet = "sectorid", tidy = NA)
  expect_type(x, "character")
})
