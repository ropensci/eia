context("series")

options(eia_antidos = 0)
key <- Sys.getenv("EIA_KEY")

test_that("time series functions returns as expected", {
  if(key == "") skip("API key not available.")

  id <- paste0("ELEC.CONS_TOT_BTU.COW-AK-1.", c("A", "Q", "M"))
  x1 <- eia_series(key, id[1], start = 2016)
  x2 <- eia_series(key, id[2], n = 10)
  x3 <- eia_series(key, id[3], end = 2016, n = 10)
  expect_is(x1, "tbl_df")
  expect_is(x2, "tbl_df")
  expect_is(x3, "tbl_df")
  expect_equal(dim(x2$data[[1]]), c(10, 3))
  expect_equal(dim(x3$data[[1]]), c(10, 3))

  x <- eia_series(key, id, end = 2016, n = 10)
  expect_equal(nrow(x), 3)
  expect_equal(nrow(x$data[[3]]), 10)

  x <- eia_series(key, id, end = 2016, n = 10, tidy = FALSE)
  expect_is(x, "list")
  expect_equal(names(x), c("request", "series"))
  expect_is(x$series, "data.frame")
  expect_equal(nrow(x$series), 3)
})
