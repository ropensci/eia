context("geoset")

key <- Sys.getenv("eia_api_key")

test_that("geoset functions returns as expected", {
  if(key == "") skip("API key not available.")

  id <- paste0("ELEC.GEN.ALL-99.", c("A", "Q", "M"))
  region <- c("USA-CA", "USA-NY")

  x1 <- eia_geoset(key, id[1], region[1], start = 2016)
  x2 <- eia_geoset(key, id[2], region, n = 10)
  x3 <- eia_geoset(key, id[3], region[2], end = 2016, n = 10)
  expect_is(x1, "tbl_df")
  expect_is(x2, "tbl_df")
  expect_is(x3, "tbl_df")
  expect_equal(nrow(x2), 20)
  expect_equal(nrow(x3), 10)

  x <- eia_geoset(key, id, region, end = 2016, n = 10)
  expect_is(x, "tbl_df")
  expect_equal(nrow(x), 60)

  x <- eia_geoset(key, id, region, end = 2016, n = 10, tidy = FALSE)
  expect_is(x, "list")
  expect_equal(length(x), 3)
  expect_is(x[[1]], "list")
})
