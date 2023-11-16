options(eia_antidos = 1)
suppressWarnings(key <- eia_get_key())

test_that("data queries return data as expected", {
  if(is.null(key)) skip("API key not available.")

  # Test character object returned given `tidy = NA`
  x <- eia_data("electricity/retail-sales", tidy = NA)
  expect_type(x, "character")

  # Test JSON list returned given `tidy = FALSE`
  x <- eia_data("electricity/retail-sales", tidy = FALSE)
  expect_type(x, "list")
  expect_equal(length(x), 4)
  expect_equal(names(x), c("response", "request", "apiVersion", "ExcelAddInVersion"))

  # Test tidy-returned data
  x <- eia_data(
    dir = "electricity/retail-sales",
    data = "price",
    facets = list(stateid = "OH", sectorid = "RES"),
    freq = "annual", start = 2011, end = 2020,
    sort = list(cols = "period", order = "asc")
  )
  expect_s3_class(x, "tbl_df")
  expect_equal(ncol(x), 7)
  expect_equal(nrow(x), 10)
  expect_equal(unique(x$stateid), "OH")
  expect_equal(unique(x$sectorid), "RES")
  expect_true("price" %in% names(x))

  # Test warning that API limit has been reached
  expect_warning(eia_data("electricity/retail-sales", data = "price"))

  options(antidos = 0)

  # Test warning that more data is available even if API limit not reached
  wrn <- paste("Rows returned: 8\nRows available: 10")
  expect_warning(
    eia_data(
      dir = "electricity/retail-sales",
      data = "price",
      facets = list(stateid = "OH", sectorid = "RES"),
      freq = "annual", start = 2011, end = 2020,
      sort = list(cols = "period", order = "asc"),
      length = 8
    ),
    wrn
  )

  # Test no data
  expect_error(suppressMessages(eia_data("electricity/zzz")), "Page not found")

  options(antidos = 1)

})


test_that("'data', 'facets', and 'freq' error/warning messages return as expected", {
  if(is.null(key)) skip("API key not available.")

  # Test "data" input value
  err <- "Invalid data 'prce' provided. The only valid data are 'revenue', 'sales', 'price', and 'customers'."
  expect_error(eia_data("electricity/retail-sales", data = "prce"), err)
  err <- "Invalid 'data' provided. Options are: 'revenue', 'sales', 'price', 'customers'"
  expect_error(eia_data("electricity/retail-sales", data = "prce", check_metadata = TRUE), err)

  # Test "facet" input value
  err <- "Invalid facet 'state' provided. The only valid facets are 'stateid', and 'sectorid'."
  expect_error(eia_data("electricity/retail-sales", facets = list(state = "OH")), err)
  err <- "Invalid 'facets' provided. Options are: 'stateid', 'sectorid'"
  expect_error(eia_data("electricity/retail-sales", facets = list(state = "OH"), check_metadata = TRUE), err)

  # Test "freq" input value
  err <- "Invalid frequency 'anual' provided. The only valid frequencies are 'monthly', 'quarterly', and 'annual'."
  expect_error(eia_data("electricity/retail-sales", freq = "anual"), err)
  err <- "Invalid 'freq' provided. Must be one of: 'monthly', 'quarterly', 'annual'"
  expect_error(eia_data("electricity/retail-sales", freq = "anual", check_metadata = TRUE), err)
  expect_warning(eia_data("electricity/retail-sales", freq = c("annual", "monthly")))
  expect_error(eia_data("electricity/retail-sales", freq = c("monthly", "annual"), check_metadata = TRUE), err)

})


test_that("'start' and 'end' error/warning messages return as expected", {
  if(is.null(key)) skip("API key not available.")

  # Test "start" input value
  err <- "'start' requires 'freq' be non-NULL."
  expect_error(eia_data("electricity/retail-sales", start = 2020), err)
  expect_error(eia_data("electricity/retail-sales", start = 2020, check_metadata = TRUE), err)
  expect_no_error(eia_data("electricity/retail-sales", freq = "annual", start = 2020))
  err <- "No data available - check inputs."
  expect_error(eia_data("electricity/retail-sales", freq = "annual", start = 2099), err)
  err <- "'start' is beyond the end of available data."
  expect_error(eia_data("electricity/retail-sales", freq = "annual", start = 2099, check_metadata = TRUE), err)
  eia_clear_data()
  expect_error(eia_data("electricity/retail-sales", freq = "annual", start = "2099", check_metadata = TRUE), err)
  expect_no_error(eia_data("electricity/retail-sales", freq = "annual", start = "2020-06"))
  err <- "'start' must be a string of format: YYYY"
  expect_error(eia_data("electricity/retail-sales", freq = "annual", start = "2020-06", check_metadata = TRUE), err)
  err <- "'start' must be a string of format: YYYY-MM"
  expect_error(eia_data("electricity/retail-sales", freq = "monthly", start = 2020, check_metadata = TRUE), err)
  eia_clear_data()
  expect_error(eia_data("electricity/retail-sales", freq = "monthly", start = "2020", check_metadata = TRUE), err)
  wrn <- "'start' is beyond available history. Earliest available: 2001-01"
  expect_warning(
    eia_data(
      "electricity/retail-sales", "price",
      facets = list(stateid = "OH", sectorid = "RES"),
      freq = "annual", start = 1980,
      check_metadata = TRUE
    )
  )

  # Test "end" input value
  err <- "'end' requires 'freq' be non-NULL."
  expect_error(eia_data("electricity/retail-sales", end = 2001), err)
  expect_error(eia_data("electricity/retail-sales", end = 2001, check_metadata = TRUE), err)
  expect_no_error(eia_data("electricity/retail-sales", freq = "annual", end = 2001))
  err <- "No data available - check inputs."
  expect_error(eia_data("electricity/retail-sales", freq = "annual", end = 1980), err)
  err <- "'end' is before the start of available data."
  expect_error(eia_data("electricity/retail-sales", freq = "annual", end = 1980, check_metadata = TRUE), err)
  eia_clear_cache()
  expect_error(eia_data("electricity/retail-sales", freq = "annual", end = "1980", check_metadata = TRUE), err)
  err <- "'end' must be a string of format: YYYY"
  expect_error(eia_data("electricity/retail-sales", freq = "annual", end = "2020-06", check_metadata = TRUE), err)
  err <- "'end' must be a string of format: YYYY-MM"
  expect_error(eia_data("electricity/retail-sales", freq = "monthly", end = 2020, check_metadata = TRUE), err)
  eia_clear_cache()
  expect_error(eia_data("electricity/retail-sales", freq = "monthly", end = "2020", check_metadata = TRUE), err)
  wrn <- "'end' is beyond available history. Latest available: 2023-08"
  expect_warning(
    eia_data(
      "electricity/retail-sales", "price",
      facets = list(stateid = "OH", sectorid = "RES"),
      freq = "annual", end = 2099,
      check_metadata = TRUE
    )
  )

})


test_that("'sort' error/warning messages return as expected", {
  if(is.null(key)) skip("API key not available.")

  # Test "sort" input values
  expect_no_error(
    eia_data(
      "electricity/retail-sales", "price",
      facets = list(stateid = "OH", sectorid = "RES"),
      freq = "annual", start = 2020, end = 2020,
      sort = list(cols = c("period", "stateid"), order = "asc")
    )
  )
  expect_no_error(
    eia_data(
      "electricity/retail-sales", "price",
      facets = list(stateid = "OH", sectorid = "RES"),
      freq = "annual", start = 2020, end = 2020,
      sort = list(cols = "period", order = c("asc", "desc")))
  )
  err <- paste0(
    "'sort' must be a named list of length 2 containing the following:\n",
    "'cols' and 'order' of arbitrary length and of length 1, respectively."
  )
  expect_error(
    eia_data("electricity/retail-sales", "price", sort = list(cols = "period", o = "asc")),
    err
  )
  err <- paste(
    "Invalid sort direction 'as' provided for column 'period'.",
    "The only valid sort directions are 'asc' and 'desc'."
  )
  expect_error(
    eia_data("electricity/retail-sales", "price", sort = list(cols = "period", order = "as")),
    err
  )
  err <- paste(
    "Invalid sort 'perid' provided.",
    "The only valid sort parameters are 'revenue', 'sales', 'price', 'customers', 'period', 'stateid', and 'sectorid' in either 'asc' or 'desc' order.",
    "If direction is not provided or is incorrect, 'asc' is the default"
  )
  expect_error(
    eia_data("electricity/retail-sales", "price", sort = list(cols = "perid", order = "asc")),
    err
  )

})


test_that("'length' and 'offset' error/warning messages return as expected", {
  if(is.null(key)) skip("API key not available.")

  # Test "length" input value
  expect_warning(x <- eia_data("electricity/retail-sales", length = 10))
  expect_equal(ncol(x), 5)
  expect_equal(nrow(x), 10)
  expect_warning(x <- eia_data("electricity/retail-sales", length = "10"))
  expect_equal(ncol(x), 5)
  expect_equal(nrow(x), 10)
  err <- "'length' must be a single value between 0 and 5000."
  expect_error(eia_data("electricity/retail-sales", length = 5001))
  expect_error(eia_data("electricity/retail-sales", length = "5001"))

  # Test "offset" input value
  expect_warning(x <- eia_data("electricity/retail-sales", length = 10, offset = 10))
  expect_equal(ncol(x), 5)
  expect_equal(nrow(x), 10)
  expect_warning(x <- eia_data("electricity/retail-sales", length = 10, offset = "10"))
  expect_equal(ncol(x), 5)
  expect_equal(nrow(x), 10)
  err <- "'offset' must be a single value greater than 0."
  expect_error(eia_data("electricity/retail-sales", offset = -1), err)
  expect_error(eia_data("electricity/retail-sales", offset = "-1"), err)

})
