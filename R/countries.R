#' Find and write countries which need manual processing
#' @description
#'  This functions reads the list of [automated countries](https://github.com/dsbbfinddx/FINDCov19TrackerData/blob/master/manual/countries-urls.csv) via
#' Selenium and R fetch functions and calculates which countries still need
#' manual processing.
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr pull filter rename
#' @importFrom tibble add_column
#' @import readr
#' @export
calc_manual_countries <- function() {

  # read list of all countries
  countries_all <- readr::read_csv(
    "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/resources/countries-urls.csv",
    cols(
      country = col_character(),
      jhu_ID = col_character(),
      source = col_character(),
      `alternative link` = col_character(),
      type = col_character(),
      data_url = col_character(),
      date_format = col_character(),
      xpath_cumul = col_character(),
      xpath_new = col_character(),
      backlog = col_character(),
      comment = col_character(),
      status = col_character()
    ),
    col_names = TRUE, quoted_na = FALSE
  ) %>% # nolint
    dplyr::select(jhu_ID, status, source)

  countries_all_sub <- countries_all %>%
    dplyr::pull(jhu_ID)

  # read list of automated countries
  countries_automated_all <- readr::read_csv(sprintf("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/automated/merged/%s-automated-tests.csv", as.character(Sys.Date(), format = "%Y-%m-%d")), # nolint
    col_types = cols(
      country = col_character(),
      tests_cumulative = col_double(),
      new_tests = col_double(),
      date = col_date(format = ""),
      source = col_character()
    ),
    quoted_na = FALSE
  ) # nolint

  countries_automated <- countries_automated_all %>%
    dplyr::pull(country)

  countries_error <- readr::read_csv(sprintf("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/issues/%s-countries-error.csv", as.character(Sys.Date(), format = "%Y-%m-%d")), # nolint
    col_types = cols(
      country = col_character(),
      date = col_date(format = ""),
      source = col_character()
    ),
    quoted_na = FALSE
  ) %>% # nolint
    dplyr::pull(country)

  # remove the countries which errored from the list of automated countries
  countries_automated_no_error <- setdiff(countries_automated, countries_error)

  # calc diff
  countries_manual <- setdiff(countries_all_sub, countries_automated_no_error)

  # only keep countries which need manual processing (including their
  # source URLS)
  countries_manual_csv <- countries_all %>%
    dplyr::filter(jhu_ID %in% countries_manual) %>%
    dplyr::rename(country = jhu_ID, url = source) %>%
    dplyr::left_join(countries_automated_all, by = "country") %>%
    select(-date, -source) %>%
    tibble::add_column(
      date = as.character(Sys.Date(), format = "%Y-%m-%d"),
      source = "manually"
    ) %>%
    dplyr::relocate(c(status, url), .after = source)

  # write csv
  readr::write_csv(
    data.frame(countries_manual_csv),
    "need-manual-processing.csv"
  )
}
