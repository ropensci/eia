test_that("cache clearing functions execute", {
  expect_type(eia_clear_cache(), "NULL")
  expect_type(eia_clear_metadata(), "NULL")
  expect_type(eia_clear_data(), "NULL")
  expect_type(eia_clear_facets(), "NULL")
})
