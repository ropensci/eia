test_that("date helpers return as expected", {
  x <- eiadate_to_date(c("2018-03", "2018-04"))
  expect_s3_class(x, "Date")
  expect_true(!any(is.na(x)))
  x <- eiadate_to_date(c("2018-03-02", "2018-03-12"))
  expect_s3_class(x, "Date")
  expect_true(!any(is.na(x)))

  expect_equal(date_to_eiadate("2018-05-14", "A"), "2018")
  expect_equal(date_to_eiadate("2018-05-14", "Q"), "2018-Q2")
  expect_equal(date_to_eiadate("2018-05-14", "M"), "2018-05")
  expect_equal(date_to_eiadate("2018-05-14", "W"), "2018-05-14")
  expect_equal(date_to_eiadate("2018-05-14", "D"), "2018-05-14")

  x <- eiadate_to_date_seq("2018-01-05", "2018-01-08")
  expect_equal(as.character(x),
               c("2018-01-05", "2018-01-06", "2018-01-07", "2018-01-08"))
  x <- eiadate_to_date_seq("2018-01-05", "2018-01-08", TRUE)
  expect_equal(as.character(x), "2018-01-05")
  x <- eiadate_to_date_seq("2018-01-01", "2018-01-12", TRUE)
  expect_equal(as.character(x), c("2018-01-01", "2018-01-08"))
  x <- eiadate_to_date_seq("2018-Q1", "2018-Q4")
  expect_equal(as.character(x),
               c("2018-01-01", "2018-04-01", "2018-07-01", "2018-10-01"))
  expect_equal(date_to_eiadate(x), rep("2018", 4))
  expect_equal(date_to_eiadate(x, "Q"), paste0("2018-Q", 1:4))
  expect_equal(date_to_eiadate(x, "M"),
               c("2018-01", "2018-04", "2018-07", "2018-10"))

  hours <- c("2019-01-02T16Z", "2019-01-02T20Z")
  x <- eiadate_to_date(hours)
  expect_equal(as.character(x),
               c("2019-01-02 16:00:00", "2019-01-02 20:00:00"))
  x2 <- date_to_eiadate(x, "H")
  expect_identical(hours, x2)

  expect_warning(date_to_eiadate(1),
                 "All formats failed to parse. No formats found.")
  expect_error(eiadate_to_date(1), "Not an EIA format date string.")
})
