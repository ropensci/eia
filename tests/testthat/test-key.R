context("key storage")

test_that("key storage and retrieval works as expected", {
  msg <- "Key stored successfully in package environment."
  err <- "EIA API key not found in package environment, global options, or system enivronment variables."

  expect_error(eia_get_key("env"), err)
  expect_message(x <- eia_set_key("fake"), msg)
  expect_is(x, "NULL")
  expect_equal(eia_get_key(), "fake")
  expect_error(eia_get_key("options"), err)
})
