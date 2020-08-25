#' Check and process country specific tasks
#'
#' This function ensures that valid values are returned for countries.
check_country <- function(dots, ...) {
  ellip <- list(...)
  if (dots$country == "Argentina") {
    checkmate::assert_int(ellip$new_tests)
    checkmate::assert_int(ellip$tests_cumulative)
  } else if (dots$country == "Afghanistan") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Armenia") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Albania") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  }  else if (dots$country == "Argentina") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Croatia") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Romania") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Scotland") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Uruguay") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  }
}
