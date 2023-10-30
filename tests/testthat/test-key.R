# test_that("EIA key availability startup message works as expected", {
#   detach(package:eia, unload = TRUE)
#   tmp_dir <- tempdir()
#   write("EIA_KEY = ''", file = file.path(tmp_dir, ".Renviron"))
#   readRenviron(file.path(tmp_dir, ".Renviron"))
#   rm(temp_dir)
#   expect_message(library(eia))
#   detach(package:eia, unload = TRUE)
#   readRenviron("~/.Renviron")
# })

test_that("key storage and retrieval works as expected", {
  msg <- c(
    "Key stored successfully in package environment.",
    "Key stored successfully in options()."
  )
  wrn <- paste(
    "EIA API key not found in package environment,",
    "global options, or system enivronment variables."
  )

  expect_warning(x <- eia_get_key("env"), wrn)
  expect_type(x, "NULL")
  expect_message(x <- eia_set_key("fake1"), msg[1])
  expect_type(x, "NULL")
  expect_equal(eia_get_key("env"), "fake1")
  expect_warning(eia_get_key("options"), wrn)
  .session_eia_env$key <- NULL
  expect_message(eia_set_key("fake2", "options"), msg[2])
  expect_equal(eia_get_key("options"), "fake2")
  options(EIA_KEY = NULL)
})

test_that("cache clearing functions execute", {
  expect_type(eia_clear_cache(), "NULL")
  expect_type(eia_clear_metadata(), "NULL")
  expect_type(eia_clear_data(), "NULL")
  expect_type(eia_clear_facets(), "NULL")
})
