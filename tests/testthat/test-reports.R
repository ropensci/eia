# test_that("eia_report returns as expected", {
#   skip_on_cran()
#   x <- eia_report(id = "drilling productivity")
#   expect_type(x, "list")
#   expect_equal(length(x), 2)
#   expect_equal(names(x), c("data", "counties"))
#   v <- c("Region", "Fuel", "Month", "Rig count", "Production per rig",
#          "Legacy production change", "Total production")
#   expect_equal(names(x$data), v)
#   v <- c("State", "County", "StateID", "CountyID", "Region")
#   expect_equal(names(x$counties), v)
# })
