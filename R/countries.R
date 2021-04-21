#' Find and write countries which need manual processing
#' @description
#'  This functions reads the file with errors for the current date in the folder [issues](https://github.com/dsbbfinddx/FINDCov19TrackerData/tree/master/issues),
#'  and generates a template for the current day in [manual/need-processing/](https://github.com/dsbbfinddx/FINDCov19TrackerData/tree/master/manual/need-processing).
#'
#' This process updates the file `-need-manual-processing.csv` *only* for the current day.
#' To see if there are errors for past dates check the folder [issues](https://github.com/dsbbfinddx/FINDCov19TrackerData/tree/master/issues)
#'
#' For the automated countries with error `test_cumulative` is set to 0 in the templates [manual/need-processing/](https://github.com/dsbbfinddx/FINDCov19TrackerData/tree/master/manual/need-processing).
#'
#'
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
    dplyr::select(jhu_ID, status, source) %>%
    dplyr::rename(country = jhu_ID, url = source)

  today <- format(Sys.time(), "%Y-%m-%d")

  countries_error <- readr::read_csv(sprintf("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/issues/all-countries-error.csv", as.character(Sys.Date(), format = "%Y-%m-%d")), # nolint
    col_types = cols(
      country = col_character(),
      date = col_date(format = ""),
      tests_cumulative = col_double(),
      new_tests = col_double(),
      tests_cumulative_corrected = col_double(),
      new_tests_corrected = col_double(),
      source = col_character()
    ),
    quoted_na = FALSE
  ) %>% # nolint
    dplyr::select(-source) %>%
    dplyr::filter(date == today)

  # only keep countries which need manual processing (including their
  # source URLS)
countries_manual_csv <- countries_error %>%
  dplyr::left_join(countries_all) %>%
  tibble::add_column(
    source = "manually"
  ) %>%
  dplyr::relocate(
    country, tests_cumulative, new_tests,
    tests_cumulative_corrected, new_tests_corrected,
    date, source, status, url
  ) %>%
  dplyr::mutate(new_tests = if_else(
    is.na(tests_cumulative) & is.na(new_tests),
    0,
    new_tests
  ))

  # write csv
  readr::write_csv(
    data.frame(countries_manual_csv),
    "need-manual-processing.csv"
  )
}
