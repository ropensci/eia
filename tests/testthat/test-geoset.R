# context("geoset")
#
# options(eia_antidos = 0)
# suppressWarnings(key <- eia_get_key())
# library(dplyr)
#
# test_that("geoset queries return as expected", {
#   if(is.null(key)) skip("API key not available.")
#
#   id <- paste0("ELEC.GEN.ALL-99.", c("A", "Q", "M"))
#   region <- c("USA-CA", "USA-NY")
#
#   x1 <- eia_geoset(id[1], region[1], start = 2016, key = key)
#   x1b <- eia_geoset(id[1], region[1], start = 2016)
#   expect_identical(x1, x1b)
#
#   x2 <- eia_geoset(id[2], region, n = 10)
#   x3 <- eia_geoset(id[3], region[2], end = 2016, n = 10)
#   expect_is(x1, "tbl_df")
#   expect_is(x2, "tbl_df")
#   expect_is(x3, "tbl_df")
#   expect_equal(nrow(x2), 2)
#   expect_equal(nrow(x3), 1)
#   expect_equal(nrow(bind_rows(x2$data)), 20)
#   expect_equal(nrow(bind_rows(x3$data)), 10)
#   expect_identical(x2, eia_geoset(id[2], region, n = 10, cache = FALSE))
#
#   x <- eia_geoset(id[1], region[1])
#   expect_is(x, "tbl_df")
#   expect_equal(nrow(x), 1)
#   expect_true(nrow(bind_rows(x$data)) >= 18)
#
#   x <- eia_geoset(id, region, end = 2016, n = 10)
#   expect_is(x, "tbl_df")
#   expect_equal(nrow(bind_rows(x$data)), 60)
#
#   x <- eia_geoset(id, region, end = 2016, n = 10, tidy = FALSE)
#   expect_is(x, "list")
#   expect_equal(length(x), 3)
#   expect_is(x[[1]], "list")
#
#   x <- eia_geoset(id, region, end = 2016, n = 10, tidy = NA)
#   expect_is(x, "character")
#
#   region <- c("AK", "New England")
#   x <- eia_geoset(id, region, n = 2)
#   expect_is(x, "tbl_df")
#   expect_equal(nrow(x), 21)
#   expect_equal(length(unique(x$region)), 7)
#   x <- bind_rows(x$data)
#   expect_equal(nrow(x), 42)
#   expect_true(all(c("value", "date", "year", "qtr", "month") %in% names(x)))
# })
#
# test_that("geoset relation queries returns as expected", {
#   if(is.null(key)) skip("API key not available.")
#
#   # This API endpoint does not seem to function
#   id <- "ELEC.GEN.ALL-99.A"
#   id2 <- "ELEC.GEN.ALL-99.A|SEC"
#   region <- "USA-AK"
#
#   expect_message(x <- eia_geoset(id, region, id2),
#                  "API `relation` endpoint did not return any data.")
#   expect_is(x, "tbl_df")
#   expect_equal(dim(x), c(0, 0))
# })
