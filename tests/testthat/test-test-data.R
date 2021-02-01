test_that("get_daily_test_data() works as expected", {
  get_daily_test_data()

  countries_error <- read.csv("countries-error.csv")
  expect_s3_class(countries_error, "data.frame")

  automated <- readr::read_csv("automated-tests.csv",
    col_types = cols(
      country = col_character(),
      tests_cumulative = col_double(),
      new_tests = col_double(),
      date = col_date(format = ""),
      source = col_character()
    ),
    quoted_na = FALSE
  )
  expect_s3_class(automated, "data.frame")
  expect_named(automated, c(
    "country", "tests_cumulative",
    "new_tests", "tests_cumulative_corrected", "new_tests_corrected",
     "date", "source"
  ), ignore.order = TRUE)
  unlink("automated-tests.csv")
  unlink("countries-error.csv")
})

test_that("calc_manual_countries() works as expected", {
  calc_manual_countries()

  countries_manual <- read.csv("need-manual-processing.csv")
  expect_s3_class(countries_manual, "data.frame")

  unlink("need-manual-processing.csv")
})

test_that("combine_all_tests() works as expected", {
  combine_all_tests()

  all <- read.csv("countries-tests-all-dates.csv"
  )
  expect_s3_class(all, "data.frame")
  expect_named(all, c(
    "country", "tests_cumulative",
    "new_tests", "tests_cumulative_corrected",
    "new_tests_corrected", "date", "source"
  ), ignore.order = TRUE)

  unlink("countries-tests-all-dates.csv")
})
