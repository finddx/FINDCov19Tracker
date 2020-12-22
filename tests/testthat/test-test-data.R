test_that("get_daily_test_data() works as expected", {
  get_daily_test_data()

  countries_error <- read.csv("countries-error.csv")
  expect_s3_class(countries_error, "data.frame")

  automated <- jsonlite::read_json("automated-tests.json",
    simplifyVector = TRUE
  )
  expect_s3_class(automated, "data.frame")
  expect_equal(colnames(automated), c(
    "country", "tests_cumulative",
    "new_tests", "date", "source"
  ))
  unlink("automated-tests.json")
  unlink("countries-error.csv")
})

test_that("calc_manual_countries() works as expected", {
  calc_manual_countries()

  countries_manual <- read.csv("countries-manual.csv")
  expect_s3_class(countries_manual, "data.frame")

  unlink("countries-manual.csv")
})

test_that("combine_all_tests() works as expected", {
  combine_all_tests()

  all <- read.csv("countries-tests-all-dates.csv")
  expect_s3_class(all, "data.frame")
  expect_equal(colnames(automated), c(
    "country", "tests_cumulative",
    "new_tests", "date", "source"
  ))

  unlink("countries-tests-all-dates.csv")
})
