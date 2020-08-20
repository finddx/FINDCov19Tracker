#' Check and process country specific tasks
#'
#' This function ensures that valid values are returned for countries.
check_country <- function(dots, ...) {
  ellip <- list(...)
  if (dots$country == "Argentina") {
    # check the previous report and calculate new tests
    pdf_prev <- ellip$pdfs[2]

    content_prev <- pdftools::pdf_text(pdf_prev)
    tests_cumulative_prev <- as.numeric(gsub(
      "[, .]", "",
      unique(gsub(
        dots$xpath_cumul, "\\1",
        na.omit(stringr::str_extract(content_prev, dots$xpath_cumul))
      ))
    ))

    new_tests <- ellip$tests_cumulative - tests_cumulative_prev

    checkmate::assert_int(new_tests)
    checkmate::assert_int(ellip$tests_cumulative)
  } else if (dots$country == "Armenia") {
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
