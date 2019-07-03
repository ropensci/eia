context("dates")

options(eia_antidos = 0)
key <- Sys.getenv("EIA_KEY")

test_that("date helpers return as expected", {
  if(key == "") skip("API key not available.")

  x <- eiadate_to_date(c("201803", "201804"))
  expect_is(x, "Date")
  expect_true(!any(is.na(x)))

  expect_equal(date_to_eiadate("2018-05-14", "A"), "2018")
  expect_equal(date_to_eiadate("2018-05-14", "Q"), "2018Q2")
  expect_equal(date_to_eiadate("2018-05-14", "M"), "201805")

  x <- eiadate_to_date_seq("2018Q1", "2018Q4")
  expect_equal(as.character(x),
               c("2018-01-01", "2018-04-01", "2018-07-01", "2018-10-01"))
  expect_equal(date_to_eiadate(x), rep("2018", 4))
  expect_equal(date_to_eiadate(x, "Q"), paste0("2018Q", 1:4))
  expect_equal(date_to_eiadate(x, "M"),
               c("201801", "201804", "201807", "201810"))
})
