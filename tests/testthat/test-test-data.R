test_that("get_test_data() works as expected", {
  get_test_data()

  today <- format(Sys.time(), "%Y-%m-%d")

  countries_error <- read.csv(sprintf("%s-countries-error.csv", today))
  expect_s3_class(countries_error, "data.frame")

  automated <- readr::read_csv(sprintf("%s-automated-tests.csv", today),
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

  all <- read.csv("coronavirus_tests_new.csv"
  )
  expect_s3_class(all, "data.frame")
  expect_named(all, c(
    "country", "tests_cumulative",
    "new_tests", "tests_cumulative_corrected",
    "new_tests_corrected", "date", "source"
  ), ignore.order = TRUE)

  unlink("coronavirus_tests_new.csv")
})
