context("dates")

test_that("date helpers return as expected", {
  x <- eiadate_to_date(c("201803", "201804"))
  expect_is(x, "Date")
  expect_true(!any(is.na(x)))
  x <- eiadate_to_date(c("20180302", "20180312"))
  expect_is(x, "Date")
  expect_true(!any(is.na(x)))

  expect_equal(date_to_eiadate("2018-05-14", "A"), "2018")
  expect_equal(date_to_eiadate("2018-05-14", "Q"), "2018Q2")
  expect_equal(date_to_eiadate("2018-05-14", "M"), "201805")
  expect_equal(date_to_eiadate("2018-05-14", "W"), "20180514")
  expect_equal(date_to_eiadate("2018-05-14", "D"), "20180514")

  x <- eiadate_to_date_seq("20180105", "20180108")
  expect_equal(as.character(x),
               c("2018-01-05", "2018-01-06", "2018-01-07", "2018-01-08"))
  x <- eiadate_to_date_seq("20180105", "20180108", TRUE)
  expect_equal(as.character(x), "2018-01-05")
  x <- eiadate_to_date_seq("20180101", "20180112", TRUE)
  expect_equal(as.character(x), c("2018-01-01", "2018-01-08"))
  x <- eiadate_to_date_seq("2018Q1", "2018Q4")
  expect_equal(as.character(x),
               c("2018-01-01", "2018-04-01", "2018-07-01", "2018-10-01"))
  expect_equal(date_to_eiadate(x), rep("2018", 4))
  expect_equal(date_to_eiadate(x, "Q"), paste0("2018Q", 1:4))
  expect_equal(date_to_eiadate(x, "M"),
               c("201801", "201804", "201807", "201810"))
})
