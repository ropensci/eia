suppressWarnings(key <- eia_get_key())

test_that("key storage and retrieval works as expected", {
  if(is.null(key)) skip("API key not available.")

  msg <- c(
    "Key stored successfully in package environment.",
    "Key stored successfully in options().",
    "Key stored successfully in system environment."
  )
  wrn <- paste(
    "EIA API key not found in package environment,",
    "global options, or system enivronment variables."
  )

  # Package environment
  expect_warning(
    x <- eia_get_key("env"),
    "EIA API key not found in package environment."
  )
  expect_type(x, "NULL")
  expect_message(x <- eia_set_key("fake1"), msg[1])
  expect_type(x, "NULL")
  expect_equal(eia_get_key("env"), "fake1")
  .session_eia_env$key <- NULL

  # Global options
  expect_warning(
    eia_get_key("options"),
    "EIA API key not found in global options."
  )
  expect_type(x, "NULL")
  expect_message(x <- eia_set_key("fake2", "options"), msg[2])
  expect_type(x, "NULL")
  expect_equal(eia_get_key("options"), "fake2")
  options(EIA_KEY = NULL)

  # System environment variables
  expect_error(eia_set_key("", "sysenv"), "Failed to set key.")
  expect_warning(
    x <- eia_get_key("sysenv"),
    "EIA API key not found in system enivronment variables."
  )
  expect_type(x, "NULL")
  expect_message(x <- eia_set_key("fake3", "sysenv"), msg[3])
  expect_type(x, "NULL")
  expect_equal(eia_get_key("sysenv"), "fake3")
  Sys.unsetenv("EIA_KEY")

  # No key in any storage location
  expect_warning(eia_get_key(), wrn)

  # Cleanup: put original key back in place
  expect_message(eia_set_key(key, "sysenv"), msg[3])

  # No key available when required
  expect_error(eia_dir(tidy = NA, key = NULL), "Key is missing.")
})
