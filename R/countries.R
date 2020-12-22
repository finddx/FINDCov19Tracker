#' Find and write countries which need manual processing
#' @description
#'  This functions reads the list of [automated countries](https://github.com/dsbbfinddx/FINDCov19TrackerData/blob/master/manual/countries-urls.csv) via
#' Selenium and R fetch functions and calculates which countries still need
#' manual processing.
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr pull
#' @export
calc_manual_countries <- function() {

  # read list of all countries
  countries_all <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/resources/countries-urls.csv") %>% # nolint
    dplyr::select(jhu_ID, source)

  countries_all_sub <- countries_all %>%
    dplyr::pull(jhu_ID)

  # read list of automated countries
  countries_automated <- readr::read_csv(sprintf("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/automated/merged/%s-automated-tests.csv", as.character(Sys.Date(), format = "%Y-%m-%d"))) %>% # nolint
    dplyr::pull(country)

  countries_error <- readr::read_csv(sprintf("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/issues/%s-countries-error.csv", as.character(Sys.Date(), format = "%Y-%m-%d"))) %>%  # nolint
    dplyr::pull(country)

  # remove the countries which errored from the list of automated countries
  countries_automated_no_error <- setdiff(countries_automated, countries_error)

  # calc diff
  countries_manual <- setdiff(countries_all_sub, countries_automated_no_error)

  # only keep countries which need manual processing (including their
  # source URLS)
  countries_manual_csv <- countries_all %>%
    dplyr::filter(jhu_ID %in% countries_manual)

  # write csv
  readr::write_csv(
    data.frame(country = countries_manual_csv),
    "countries-manual.csv"
  )
}
