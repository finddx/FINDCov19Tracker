#' Check and process country specific tasks
#'
#' This function ensures that valid values are returned for countries.
check_country <- function(dots, ...) {
  if (dots$country == "Argentina") {

    ellip <- list(...)
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
  }
}
