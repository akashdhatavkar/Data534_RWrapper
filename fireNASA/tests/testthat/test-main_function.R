context("Check the fire_data() function")

test_that("Check if Data is pulled Correctly", {
  testdf <- fireball_data()
  expect_gt(nrow(testdf), 0)
  expect_equal(ncol(testdf),13)
  expect_equal(sum(is.na(testdf)), 0)
  expect_gt(min(testdf$date), as.Date("1900-01-01"))
  expect_true(is.numeric(testdf$energy))
  expect_true(is.numeric(testdf$vel))

})
