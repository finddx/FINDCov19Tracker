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

calculate_new_tests <- function(dots, tests_cumulative) {
  cli::cli_alert_info("Generating {.strong new_tests} for {.emph {dots$country}} manually from yesterday's data.")
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

  if (new_tests < 0) new_tests <- 0

  return(new_tests)

}

#' @importFrom stringr str_replace_all str_match
clean_selenium <- function(data) {

  data_clean <- data %>%
    mutate(tests_cumulative = str_replace_all(tests_cumulative, ",", "")) %>%
    mutate(tests_cumulative = str_replace_all(tests_cumulative, "\\.", "")) %>%
    mutate(tests_cumulative = str_replace_all(tests_cumulative, " ", "")) %>%
    mutate(tests_cumulative = str_match(tests_cumulative, pattern = "\\d+")) %>%
    mutate(tests_cumulative = as.numeric(tests_cumulative)) %>%
    na.omit() %>%
    mutate(date = as.Date(date))

  return(data_clean)
}


calculate_daily_tests_selenium <- function(data) {
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
    dplyr::filter(country %in% data$country) %>%
    dplyr::filter(date %in% unique(data$date - 1))

  tests_yesterday <- tbl$tests_cumulative
  data$new_tests <- data$tests_cumulative - tests_yesterday

  return(data)

}
