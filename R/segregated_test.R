#' Get segregated tests from Selenium and combine them.
#' Using the parameter `days`, all files in [`automated/merged/`](https://github.com/finddx/FINDCov19TrackerData/tree/master/automated/merged) are updated for the last dates given the input number.
#'
#
#' @description
#'   **Input:** Daily test data scraped via Selenium
#'   [`automated/fetch`](https://github.com/finddx/FINDCov19TrackerData/tree/master/automated/fetch) and [`automated/selenium`](https://github.com/finddx/FINDCov19TrackerData/tree/master/automated/selenium) directories.
#'
#'   **Output:**
#'   - `segregated.csv`
#'
#'   which are then deployed by CI to the [`automated/merged/`](https://github.com/finddx/FINDCov19TrackerData/tree/master/automated/merged) directory in the `finddx/FINDCov19TrackerData` repo.
#' @param write if csv files should be written. Default TRUE.
#'
#' @examples
#' segregated_test_data()
#'
#' @importFrom dplyr left_join mutate rename relocate select
#' @export
#'
segregated_test_data <- function(write = TRUE) {

  #countries with segregated tests
  segregated_countries <- c(
    "Austria",
    #"Benin",
    "Colombia",
    "Czechia",
    "Denmark",
    #"Greece",
    "Indonesia",
    "Italy",
    "Lithuania",
    #"Romania",
    "Slovakia",
    "Slovenia",
    "Spain",
    "Switzerland"
  )

  # Tests cumulative corrected
  test_data <- readr::read_csv(
    "https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/processed/coronavirus_tests.csv") %>%
    dplyr::select(country, date, tests_cumulative, tests_cumulative_corrected, new_tests_corrected, source) %>%
    dplyr::filter(date >= "2021-03-22") %>%
    dplyr::filter(country %in% segregated_countries)

  today <- format(Sys.time(), "%Y-%m-%d")

  # it includes the day before to retrieve this date and calculate tests for day 1
  first_date <- as.Date("2021-03-22")

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
    "https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/resources/countries-urls.csv",
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
    "https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/automated/selenium/%s-tests-selenium.csv", # nolint
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

  segregated_tests <- selenium_tests_clean %>%
    dplyr::select(-tests_cumulative, -source)

  combined_segregated <- dplyr::left_join(test_data, segregated_tests) %>%
    dplyr::arrange(country, date) %>%
    dplyr::group_by(country) %>%
    tidyr::fill(pcr_tests_cum,
                .direction = "down") %>%
    tidyr::fill(rapid_test_cum,
                .direction = "down") %>%
    #calculating new tests
    dplyr::mutate(pcr_test_new =
                    pcr_tests_cum - lag(pcr_tests_cum)) %>%
    dplyr::mutate(rapid_test_new =
                    rapid_test_cum - lag(rapid_test_cum)) %>%
    dplyr::mutate(
      percentage_rapid_daily = rapid_test_new/ (pcr_test_new + rapid_test_new)) %>%
    #Setting NA negative values
    dplyr::mutate(percentage_rapid_daily = if_else(
      percentage_rapid_daily < 0 | percentage_rapid_daily > 1,
      NA_real_,
      percentage_rapid_daily
    )) %>%
    # changing NA for value of the previous day
    tidyr::fill(percentage_rapid_daily,
                .direction = "down") %>%
    # excluding first days without segregated data
    dplyr::filter(!is.na(percentage_rapid_daily)) %>%
    #calculating new tests corrected based on percentage
    dplyr::mutate(pcr_test_new_corrected =
                    round((1-percentage_rapid_daily) * new_tests_corrected)) %>%
    dplyr::mutate(rapid_test_new_corrected =
                    round((percentage_rapid_daily) * new_tests_corrected)) %>%
    #recalculate cumulative segregated data based on corrected daily data
    dplyr::mutate(first_pcr_tests_cum_corrected = if_else(
      row_number() == 1,
      pcr_tests_cum,
      NA_real_
      )) %>%
    dplyr::mutate(first_rapid_test_cum_corrected = if_else(
      row_number() == 1,
      rapid_test_cum,
      NA_real_
    )) %>%
    tidyr::fill(first_pcr_tests_cum_corrected,
                .direction = "down") %>%
    tidyr::fill(first_rapid_test_cum_corrected,
                .direction = "down") %>%
    dplyr::mutate(first_pcr_tests_new_corrected = if_else(
      row_number() == 1,
      pcr_test_new_corrected,
      NA_real_
    )) %>%
    dplyr::mutate(first_rapid_test_new_corrected = if_else(
      row_number() == 1,
      rapid_test_new_corrected,
      NA_real_
    )) %>%
    tidyr::fill(first_pcr_tests_new_corrected,
                .direction = "down") %>%
    tidyr::fill(first_rapid_test_new_corrected,
                .direction = "down") %>%
    dplyr::mutate(pcr_tests_cum_corrected = if_else(
      row_number() == 1,
      first_pcr_tests_cum_corrected,
      first_pcr_tests_cum_corrected + cumsum(pcr_test_new_corrected) - first_pcr_tests_new_corrected
    )) %>%
    dplyr::mutate(rapid_test_cum_corrected = if_else(
      row_number() == 1,
      first_rapid_test_cum_corrected,
      first_rapid_test_cum_corrected + cumsum(rapid_test_new_corrected) - first_rapid_test_new_corrected
    )) %>%
    dplyr::select(-first_pcr_tests_new_corrected, -first_rapid_test_new_corrected,
                  -first_pcr_tests_cum_corrected, -first_rapid_test_cum_corrected)

  shiny_segregated <- combined_segregated %>%
    dplyr::arrange(country, date) %>%
    dplyr::group_by(country) %>%
    # dplyr::mutate(all_pcr_test_new = if_else(
    #   pcr_test_new_corrected == 0,
    #   NA_real_,
    #   pcr_test_new_corrected
    # )) %>%
    # dplyr::mutate(all_rapid_test_new = if_else(
    #   rapid_test_new_corrected == 0,
    #   NA_real_,
    #   rapid_test_new_corrected
    # )) %>%
    dplyr::mutate(all_new_tests = smooth_new_tests(new_tests_corrected,
                                                 tests_cumulative_corrected)) %>%
    dplyr::mutate(all_new_tests = round(robust_rollmean(all_new_tests))) %>%
    dplyr::mutate(all_pcr_test_cum = pcr_tests_cum_corrected) %>%
    dplyr::mutate(all_pcr_test_new = smooth_new_tests(pcr_test_new_corrected,
                                               pcr_tests_cum_corrected)) %>%
    dplyr::mutate(all_pcr_test_new = round(robust_rollmean(all_pcr_test_new))) %>%
    dplyr::mutate(all_rapid_test_cum = rapid_test_cum_corrected) %>%
    dplyr::mutate(all_rapid_test_new = smooth_new_tests(rapid_test_new_corrected,
                                                 rapid_test_cum_corrected)) %>%
    dplyr::mutate(all_rapid_test_new = round(robust_rollmean(all_rapid_test_new))) %>%
    dplyr::relocate(country,date,tests_cumulative,tests_cumulative_corrected,
                    new_tests_corrected,
                pcr_tests_cum,rapid_test_cum,pcr_test_new,rapid_test_new,
                pcr_tests_cum_corrected,rapid_test_cum_corrected,
                pcr_test_new_corrected,rapid_test_new_corrected,
                all_pcr_test_cum,all_rapid_test_cum,
                all_new_tests,
                all_pcr_test_new,all_rapid_test_new,source)


  if (write == TRUE) {
    readr::write_csv(combined_segregated, "segregated_tests.csv")
    readr::write_csv(shiny_segregated, "segregated-data.csv")
  }

  segregated_errors <- combined_segregated %>%
    dplyr::filter(pcr_test_new_corrected < 0 |
      rapid_test_new_corrected < 0) %>%
    dplyr::arrange(country, date)

  if (write == TRUE) {

    readr::write_csv(
      segregated_errors,
      "segregated-data-error.csv"
    )
  }

  return(combined_segregated)
}
