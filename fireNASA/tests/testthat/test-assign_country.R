context("Check the assign_country() function")

test_that("Check 6 map locations and one bogus value", {
  #Country, Latitude, Longitude
  #Canada 49.941677, -119.396158
  #Spain 41.376738, 2.143959
  #Russian Federation 65.849246, 174.294309
  #South Africa -33.943763, 18.575812
  #New Zealand -45.085220, 169.340462
  #Argentina -49.035113, -68.843300

  lat <-abs(c(49.941677,41.376738,65.849246,-33.943763,-45.085220,-49.035113,NA))
  lon<-abs(c(-119.396158,2.143959,174.294309,18.575812,169.340462,-68.843300,10))

  londir<-c("W","E","E","E","E","W","E")
  latdir<-c("N","N","N","S","S","S","S")
  test_df <- data.frame(lat,lon,latdir,londir)
  test_df$"lat-dir"<-as.character(test_df$latdir)
  test_df$"lon-dir"<-as.character(test_df$londir)

  expect_equal(assign_country(test_df)$country[1], "Canada")
  expect_equal(assign_country(test_df)$country[2], "Spain")
  expect_equal(assign_country(test_df)$country[3], "Russian Federation")
  expect_equal(assign_country(test_df)$country[4], "South Africa")
  expect_equal(assign_country(test_df)$country[5], "New Zealand")
  expect_equal(assign_country(test_df)$country[6], "Argentina")
  expect_equal(assign_country(test_df)$country[7], "UNKNOWN")

})


context("Check various helper functions")

test_that("Check for proper responses in clean_latlon helper function", {

  expect_equal(clean_latlon(25.1,"N"),25.1)
  expect_equal(clean_latlon(25.1,"S"),-25.1)
  expect_equal(clean_latlon(25.1,"E"),25.1)
  expect_equal(clean_latlon(25.1,"W"),-25.1)
  expect_equal(clean_latlon(-25.1,"N"),25.1)
  expect_equal(clean_latlon(-25.1,"S"),-25.1)
  expect_equal(clean_latlon(-25.1,"E"),25.1)
  expect_equal(clean_latlon(-25.1,"W"),-25.1)
  expect_equal(clean_latlon(25.1,"n"),25.1)
  expect_equal(clean_latlon(25.1,"s"),25.1)
  expect_equal(clean_latlon(25.1,"e"),25.1)
  expect_equal(clean_latlon(25.1,"w"),25.1)
  expect_equal(clean_latlon(-25.1,"n"),25.1)
  expect_equal(clean_latlon(-25.1,"s"),25.1)
  expect_equal(clean_latlon(-25.1,"e"),25.1)
  expect_equal(clean_latlon(-25.1,"w"),25.1)
  expect_null(clean_latlon(NA,"N"),abs(NA))
  expect_equal(clean_latlon(25.1,NA),25.1)
  expect_equal(clean_latlon(-25.1,NA),25.1)
  expect_equal(clean_latlon(NA,NA),abs(NA))

})
