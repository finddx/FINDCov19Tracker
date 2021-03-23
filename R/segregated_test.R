#' Get segregated tests from Selenium and combine them.
#' Using the parameter `days`, all files in [`automated/merged/`](https://github.com/dsbbfinddx/FINDCov19TrackerData/tree/master/automated/merged) are updated for the last dates given the input number.
#'
#
#' @description
#'   **Input:** Daily test data scraped via Selenium
#'   [`automated/fetch`](https://github.com/dsbbfinddx/FINDCov19TrackerData/tree/master/automated/fetch) and [`automated/selenium`](https://github.com/dsbbfinddx/FINDCov19TrackerData/tree/master/automated/selenium) directories.
#'
#'   **Output:**
#'   - `segregated.csv`
#'
#'   which are then deployed by CI to the [`automated/merged/`](https://github.com/dsbbfinddx/FINDCov19TrackerData/tree/master/automated/merged) directory in the `dsbbfinddx/FINDCov19TrackerData` repo.
#' @param days days to combine. It should always be bigger than 0.
#' @param write if csv files should be written. Default TRUE.
#'
#' @examples
#' get_test_data(days = 8, write = FALSE)
#'
#' @importFrom dplyr left_join mutate rename relocate select
#' @export
segregated_test_data <- function(days = 1, write = TRUE) {

  today <- format(Sys.time(), "%Y-%m-%d")

  # it includes the day before to retrieve this date and calculate tests for day 1
  first_date <- as.Date(today) - days

  if (first_date <= as.Date("2021-03-22")) {
    # Due to the implementation of automated workflow
    warning("Segregated data is scraped since 2021-03-22.
            Data before that date is not available")
    first_date <- as.Date("2021-03-22")
    window_update <- seq(first_date, as.Date(today), by = "days")
    } else {
    window_update <- seq(first_date, as.Date(today), by = "days")
  }

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
    dplyr::select(jhu_ID) %>%
    dplyr::rename(country = jhu_ID) %>%
    merge(window_update) %>%
    rename(date = y)

  selenium_list <- sprintf(
    "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/automated/selenium/%s-tests-selenium.csv", # nolint
    window_update
  )
  # When only one day is updated rio::import_list() won't have file column
  if (length(selenium_list) == 1) {
    selenium_tests <- rio::import_list(selenium_list, rbind = TRUE) %>%
      dplyr::mutate(source = "selenium") %>%
      dplyr::mutate(date = as.Date(date))
  } else {
    selenium_tests <- rio::import_list(selenium_list, rbind = TRUE) %>%
      dplyr::mutate(source = "selenium") %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::select(-`_file`)
  }
  selenium_tests_clean <- clean_selenium_segregated(selenium_tests)
  selenium_tests_daily <- selenium_tests_clean %>%
    dplyr::mutate(pcr_tests_new = if_else(
      date == "2021-03-22",
      pcr_tests_cum,
      NA_real_
    )) %>%
    dplyr::mutate(pcr_tests_cum_corrected = if_else(
      date == "2021-03-22",
      pcr_tests_cum,
      NA_real_
    )) %>%
    dplyr::mutate(pcr_tests_new_corrected = if_else(
      date == "2021-03-22",
      pcr_tests_cum,
      NA_real_
    )) %>%
    dplyr::mutate(rapid_test_new = if_else(
      date == "2021-03-22",
      rapid_test_cum,
      NA_real_
    )) %>%
    dplyr::mutate(rapid_test_cum_corrected = if_else(
      date == "2021-03-22",
      rapid_test_cum,
      NA_real_
    )) %>%
    dplyr::mutate(rapid_test_new_corrected = if_else(
      date == "2021-03-22",
      rapid_test_cum,
      NA_real_
    )) %>%
    dplyr::relocate(
      country, date, tests_cumulative, pcr_tests_cum, pcr_tests_new,
      rapid_test_cum, rapid_test_new, pcr_tests_cum_corrected,
      pcr_tests_new_corrected, rapid_test_cum_corrected,
      rapid_test_new_corrected, source
    )


  segregated_test <- selenium_tests_daily %>%
    dplyr::arrange(country, date) %>%
    dplyr::group_by(country) %>%
    # calculating tests_cumulative when new_tests is available
    dplyr::mutate(pcr_tests_cum = calc_cumulative_t(
      pcr_tests_cum,
      pcr_tests_new)) %>%
    dplyr::mutate(rapid_test_cum = calc_cumulative_t(
      rapid_test_cum,
      rapid_test_new)) %>%
    # calculating new_tests when tests_cumulative is available
    dplyr::mutate(pcr_tests_new = calc_new_t(pcr_tests_cum, pcr_tests_new)) %>%
    dplyr::mutate(rapid_test_new = calc_new_t(rapid_test_cum, rapid_test_new)) %>%
    dplyr::ungroup() %>%
    # populating corrected columns
    dplyr::mutate(pcr_tests_new_corrected = if_else(
      is.na(pcr_tests_new_corrected),
      pcr_tests_new,
      pcr_tests_new_corrected
    )) %>%
    dplyr::mutate(pcr_tests_cum_corrected = if_else(
      is.na(pcr_tests_cum_corrected),
      pcr_tests_cum,
      pcr_tests_cum_corrected
    )) %>%dplyr::mutate(rapid_test_new_corrected = if_else(
      is.na(rapid_test_new_corrected),
      rapid_test_new,
      rapid_test_new_corrected
    )) %>%
    dplyr::mutate(rapid_test_cum_corrected = if_else(
      is.na(rapid_test_cum_corrected),
      rapid_test_cum,
      rapid_test_cum_corrected
    )) %>%
    dplyr::arrange(country, date) %>%
    select(-source)


  if (write == TRUE) {
    readr::write_csv(segregated_test, "segregated_tests.csv")
  }

  return(segregated_test)
}
