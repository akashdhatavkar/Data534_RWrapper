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

test_that("Check if invalid paramater returns HTTP Status 400", {
  err400 <- "The request contained invalid keywords and/or content: details returned"
  expect_equal(fireball_data(min_energy=-5),err400)
})

test_that("Check if an empty dataset is caught properly", {
  warn_msg<- "Please Change Input Parameters as there is no data matching Input Parameters!"
  expect_equal(fireball_data(date_min="2050-01-01"),warn_msg)
})
