context("Check the fire_data() function")

test_that("Check if Bad Request is recieved", {
  testdf <- fireball_data(date_min = "2010-01-01", lim = 7)

  expect_equal(nrow(testdf), 7)
  expect_equal(ncol(testdf),12)
  expect_equal(sum(is.na(testdf)), 0)
  expect_gt(min(as.Date(testdf$date, "%Y-%m-%d %H:%M:%S")), as.Date("2010-01-01"))
})
