context("key storage")

test_that("key storage and retrieval works as expected", {
  msg <- "Key stored successfully in package environment."
  wrn <- "EIA API key not found in package environment, global options, or system enivronment variables."

  expect_warning(x <- eia_get_key("env"), wrn)
  expect_is(x, "NULL")
  expect_message(x <- eia_set_key("fake"), msg)
  expect_is(x, "NULL")
  expect_equal(eia_get_key(), "fake")
  expect_warning(eia_get_key("options"), wrn)
  .session_eia_env$key <- NULL
})
