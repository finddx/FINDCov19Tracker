get_status <- function(url) {
  return(data.frame(url = url, response_code = rvest::html_session(url) %>%
    httr::status_code()))
}

read_urls <- function(path) {
  tf <- tempfile(fileext = ".xlsx")
  curl::curl_download(path, tf)
  file <- readxl::read_xlsx(tf, sheet = 1)
  return(file)
}

calculate_new_tests <- function(dots) {
  cli::cli_alert_info("Generating {.strong new_tests} for {.emph dots$country} manually from yesterday's data.")
  tbl <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/coronavirus_tests.csv",
    col_types = list(
      country = readr::col_character(),
      date = readr::col_date(format = ""),
      new_tests = readr::col_double(),
      tests_cumulative = readr::col_double(),
      jhu_ID = readr::col_character(),
      source = readr::col_character()
    ),
    progress = FALSE
  ) %>%
    dplyr::filter(country == dots$country) %>%
    dplyr::filter(date == lubridate::today() - 1)

  tests_yesterday <- tbl$tests_cumulative
  # to ensure we do not get a negative number
  new_tests <- tests_cumulative - tests_yesterday

  if (new_tests < 0) new_tests <- NA

  return(new_tests)

}
