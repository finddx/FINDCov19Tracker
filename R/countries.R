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
  } else if (dots$country == "Argentina") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Austria") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Azerbaijan") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Bahrain") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Belgium") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Bermuda") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Bulgaria") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Canada") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Chile") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Colombia") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Croatia") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Czech Republic") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  } else if (dots$country == "Denmark") {
    checkmate::assert_number(ellip$new_tests)
    checkmate::assert_number(ellip$tests_cumulative)
  }




  else if (dots$country == "Croatia") {
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
