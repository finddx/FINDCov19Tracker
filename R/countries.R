#' Find and write countries which need manual processing
#' @description
#'  This functions reads the list of [automated countries](https://github.com/dsbbfinddx/FINDCov19TrackerData/blob/selenium/manual/countries-all.csv) via
#' Selenium and R fetch functions and calculates which countries still need
#' manual processing.
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr pull
#' @export
calc_manual_countries <- function() {

  # read list of all countries
  countries_all <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/resources/countries-all.csv", col_types = list(readr::col_character())) %>% # nolint
    dplyr::pull(country)

  # read list of automated countries
  countries_automated <- jsonlite::read_json(sprintf("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/automated/merged/%s-automated-tests.json", as.character(Sys.Date(), format = "%Y-%m-%d")), simplifyVector = TRUE) %>% # nolint
    dplyr::pull(country)

  countries_error <- readr::read_csv(sprintf("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/issues/%s-countries-error.csv", as.character(Sys.Date(), format = "%Y-%m-%d"))) %>%
    dplyr::pull(country)

  # remove the countries which errored from the list of automated countries
  countries_automated_no_error <- setdiff(countries_automated, countries_error)

  # calc diff
  countries_manual <- setdiff(countries_all, countries_automated_no_error)

  # write csv
  readr::write_csv(
    data.frame(country = countries_manual),
    "countries-manual.csv"
  )
}
