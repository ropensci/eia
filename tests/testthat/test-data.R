suppressWarnings(key <- eia_get_key())

test_that("data queries return as expected", {
  if(is.null(key)) skip("API key not available.")

  # Test character object returned given `tidy = NA`
  x <- eia_data("electricity/retail-sales", tidy = NA)
  expect_type(x, "character")

  # Test JSON list returned given `tidy = FALSE`
  x <- eia_data("electricity/retail-sales", tidy = FALSE)
  expect_type(x, "list")
  expect_equal(length(x), 3)
  expect_equal(names(x), c("response", "request", "apiVersion"))

  options(eia_antidos = 3)

  # Test tidy-returned data
  x <- eia_data(
    dir = "electricity/retail-sales",
    data = "price",
    facets = list(stateid = "OH", sectorid = "RES"),
    freq = "annual", start = "2011", end = "2020",
    sort = list(cols = "period", order = "asc")
  )

  options(eia_antidos = 0)

  expect_s3_class(x, "tbl_df")
  expect_equal(ncol(x), 7)
  expect_equal(nrow(x), 10)
  expect_equal(unique(x$stateid), "OH")
  expect_equal(unique(x$sectorid), "RES")
  expect_true("price" %in% names(x))

  # Test warning that API limit has been reached
  expect_warning(eia_data("electricity/retail-sales"))

  # Test warning that more data is available even if API limit not reached
  wrn <- paste("Rows returned: 8\nRows available: 10")
  expect_warning(
    eia_data(
      dir = "electricity/retail-sales",
      data = "price",
      facets = list(stateid = "OH", sectorid = "RES"),
      freq = "annual", start = "2011", end = "2020",
      sort = list(cols = "period", order = "asc"),
      length = 8
    ),
    wrn
  )

  # Test no data
  expect_error(suppressMessages(eia_data("electricity/zzz")), "Page not found")

  # Test "freq" input value
  err <- "'freq' must be one of: 'annual', 'yearly', 'monthly', 'daily', or 'hourly'."
  expect_error(eia_data("electricity/retail-sales", freq = "anual"), err)
  err <- "'freq' must be a character value of length 1."
  expect_error(eia_data("electricity/retail-sales", freq = c("annual", "monthly")), err)

  # Test non-character "start" input value
  err <- "'start' must be a character matching the required frequency format."
  expect_error(eia_data("electricity/retail-sales", freq = "annual", start = 2010), err)

  # Test non-character "end" input value
  err <- "'end' must be a character matching the required frequency format."
  expect_error(eia_data("electricity/retail-sales", freq = "annual", end = 2010), err)

  # Test "length" input value
  expect_warning(
    x <- eia_data("electricity/retail-sales", length = 10)
  )
  expect_equal(nrow(x), 10)
  err <- "'length' must be a single numeric value between 0 and 5000."
  expect_error(eia_data("electricity/retail-sales", length = "10"), err)

  # Test "offset" input value
  expect_warning(
    x <- eia_data("electricity/retail-sales", length = 10, offset = 10)
  )
  expect_equal(nrow(x), 10)
  err <- "'offset' must be a single numeric value greater than 0."
  expect_error(eia_data("electricity/retail-sales", offset = "10"), err)

  # Test misspelling of sort objects
  err <- paste0(
    "'sort' must be a named list of length 2 containing the following:\n",
    "'cols' and 'order' of arbitrary length and of length 1, respectively."
  )
  expect_error(
    eia_data(
      dir = "electricity/retail-sales",
      data = "price",
      facets = list(stateid = "OH", sectorid = "RES"),
      freq = "annual", start = "2011", end = "2020",
      sort = list(cols = "period", ordr = "asc")
    ),
    err
  )

  # Test sort input where length of "order" > 1
  err <- "must provide a single value for 'order': 'asc' or 'desc'."
  expect_error(
    eia_data(
      dir = "electricity/retail-sales",
      data = "price",
      facets = list(stateid = "OH", sectorid = c("RES", "COM")),
      freq = "annual", start = "2011", end = "2020",
      sort = list(cols = c("sectorid", "period"), order = c("asc", "desc"))
    ),
    err
  )

  # Test misspelling of sort objects
  err <- "'order' must be one of 'asc' or 'desc'."
  expect_error(
    eia_data(
      dir = "electricity/retail-sales",
      data = "price",
      facets = list(stateid = "OH", sectorid = "RES"),
      freq = "annual", start = "2011", end = "2020",
      sort = list(cols = "period", order = "ascc")
    ),
    err
  )

})

test_that("metadata helper returns as expected", {
  if(is.null(key)) skip("API key not available.")

  x <- eia_metadata("electricity/retail-sales", cache = FALSE)
  expect_type(x, "list")

  x <- eia_metadata("electricity/retail-sales", tidy = FALSE, cache = FALSE)
  expect_type(x, "list")
  expect_length(x, 3)

  x <- eia_metadata("electricity/retail-sales", tidy = NA, cache = FALSE)
  expect_type(x, "character")

})

options(eia_antidos = 1)
