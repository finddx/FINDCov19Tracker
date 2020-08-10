#' Scrape test data from all countries
#'
#' @description
#' Rowwise processing of countries test data reports with support for any file
#' type (csv, pdf, xlsx, etc.)
#' @export
preprocess_test_data <- function() {

  info <- read_urls("https://github.com/dsbbfinddx/FINDCov19TrackerData/raw/master/manual/coronavirus_tests_countries_urls_CH_v7.xlsx")

  res = purrr::pmap(info, process_countries_rowwise)
  return(res)
}

process_countries_rowwise <- function(...) {
  dots <- list(...)

  if (!is.na(dots$data_url)) {
    cli::cli_alert_success("{.strong {dots$country}}: Type: {.code {dots$type}}, URL: {.url {dots$data_url}}.")
  } else {
    cli::cli_alert_danger("{.strong {dots$country}}: 'URL' field: 'NA' -> {.emph Skipping}.")
    res = rep(NA, 2)
    purrr::set_names(res, c("new_tests", "tests_cumulative"))
    return(res)
  }

  res = switch(dots$type,
    xlsx = fetch_from_csv_xlsx(dots),
    csv = fetch_from_csv_xlsx(dots),
    json = fetch_from_json(dots),
    rep(NA, 2) # all other cases
    # more fetch functions here
  )

  if (length(res) != 2) {
    browser()
  }

  purrr::set_names(res, c("new_tests", "tests_cumulative"))
  return(res)
}
