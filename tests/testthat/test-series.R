context("series")

options(eia_antidos = 0)
suppressWarnings(key <- eia_get_key())

test_that("time series queries returns as expected", {
  if(is.null(key)) skip("API key not available.")

  id <- paste0("ELEC.CONS_TOT_BTU.COW-AK-1.", c("A", "Q", "M"))
  x1 <- eia_series(id[1], start = 2016, key = key)
  x1b <- eia_series(id[1], start = 2016)
  expect_identical(x1, x1b)

  x2 <- eia_series(id[2], n = 10)
  x3 <- eia_series(id[3], end = 2016, n = 10)
  expect_is(x1, "tbl_df")
  expect_is(x2, "tbl_df")
  expect_is(x3, "tbl_df")
  expect_equal(ncol(x2$data[[1]]), 4)
  expect_equal(ncol(x3$data[[1]]), 4)
  expect_identical(x1, eia_series(id[1], start = 2016, cache = FALSE))

  x <- eia_series(id, end = 2016, n = 10)
  expect_equal(nrow(x), 3)
  expect_equal(nrow(x$data[[3]]), 10)

  x <- eia_series(id, end = 2016, n = 10, tidy = FALSE)
  expect_is(x, "list")
  expect_equal(names(x), c("request", "series"))
  expect_is(x$series, "data.frame")
  expect_equal(nrow(x$series), 3)

  x <- eia_series(id, end = 2016, n = 10, tidy = NA)
  expect_is(x, "character")
})

test_that("time series metadata helpers return as expected", {
  if(is.null(key)) skip("API key not available.")

  id <- paste0("ELEC.CONS_TOT_BTU.COW-AK-1.", c("A", "Q", "M"))
  x <- eia_series_metadata(id)
  expect_equal(nrow(x), 3)
  expect_true(!"data" %in% names(x))

  expect_equal(dim(eia_series_updates(id)), c(3, 2))
  expect_true(nrow(eia_series_dates(id)) > 300)
  x <- eia_series_range(id)
  expect_true(x$n[1] >= 18)
  expect_true(x$n[2] >= 72)
  expect_true(x$n[3] >= 220)
})
