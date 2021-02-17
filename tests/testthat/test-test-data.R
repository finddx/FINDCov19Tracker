test_that("get_test_data() works as expected", {
  automated <- get_test_data(write = FALSE)

  today <- format(Sys.time(), "%Y-%m-%d")

  expect_s3_class(automated[["countries_error"]], "data.frame")

  expect_s3_class(automated[["test_combined"]], "data.frame")
  expect_named(automated[["test_combined"]], c(
    "country", "tests_cumulative",
    "new_tests", "tests_cumulative_corrected", "new_tests_corrected",
     "date", "source"
  ), ignore.order = TRUE)

  expect_equal(unique(algo[["test_combined"]]$date), today)

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
