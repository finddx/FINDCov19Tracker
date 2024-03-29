#' Postprocess tests data for input into Shiny App
#'
#' Creates `coronavirus_cases.csv`, based on `coronavirus_cases_new.csv`.
#' `coronavirus_cases_new.csv` combines different sources (Selenium, fetch, and manual),
#' and might have `NA` or negative values for new_test_corrected.
#'
#' This file is created/updated by `process_jhu_data()` which needs to be run
#' before.
#'   **Input:** test data combined from different sources (Selenium, fetch, and manual)[`automated/coronavirus_tests_new.csv`](https://github.com/finddx/FINDCov19TrackerData/blob/master/automated/coronavirus_tests_new.csv).
#'
#'#'   **Output:**
#'   - `coronavirus_cases.csv`
#'
#'   which is then deployed by CI to the [`processed/`](https://github.com/finddx/FINDCov19TrackerData/tree/master/processed) directory in the `finddx/FINDCov19TrackerData` repo.
#'
#' @importFrom utils tail
#' @export
process_test_data <- function() {

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
    col_names = TRUE
  ) %>% # nolint
    dplyr::select(country, jhu_ID, source) %>%
    dplyr::rename(url = source)

  # read test data combining Scrape, fetch, and manual data
  cv_tests <- readr::read_csv(
  "https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/automated/coronavirus_tests_new.csv",
  cols(
    country = col_character(),
    tests_cumulative = col_double(),
    new_tests = col_double(),
    tests_cumulative_corrected = col_double(),
    new_tests_corrected = col_double(),
    date = col_date(format = ""),
    source = col_character()
  ),
  col_names = TRUE
) %>% # nolint
  dplyr::arrange(country, date) %>%
  dplyr::rename(jhu_ID = country) %>%
  dplyr::left_join(countries_all) %>%
  # Keeping the country names coronavirus_tests.csv had
  dplyr::mutate(jhu_ID = if_else(jhu_ID == "LaoPeoplesDemocraticRepublic",
    "LaoPeople'sDemocraticRepublic",
    jhu_ID
  )) %>%
  dplyr::mutate(jhu_ID = if_else(jhu_ID == "OccupiedPalestinianterritory",
    "occupiedPalestinianterritory",
    jhu_ID
  )) %>%
  # joining source(url) only since automated Selenium workflow implemented
  dplyr::mutate(source = if_else(date > as.Date("2021-02-18"),
    url,
    source)) %>%
  dplyr::select(-url)


  # prevent issues in DT with non ascii characters in URL
  cv_tests$source <- iconv(cv_tests$source, from = "ISO8859-1", to = "UTF-8")

  # be sure no NA in new_tests field
  cv_tests$new_tests <- ifelse(is.na(cv_tests$new_tests), 0, cv_tests$new_tests)
  cli::cli_alert("total count of new tests: {sum(cv_tests$new_tests)}.")

  # be sure no NA in tests_cumulative field
  cv_tests_clean <- cv_tests %>%
    dplyr::arrange(jhu_ID, date) %>%
    #  when tests_cumulative is NA filling with the last value
    dplyr::group_by(jhu_ID) %>%
    tidyr::fill(tests_cumulative, .direction = "down") %>%
    dplyr::ungroup() %>%
  # populating tests_cumulative_corrected and new_tests_corrected
    dplyr::mutate(
      new_tests_corrected = if_else(is.na(new_tests_corrected) &
                                      is.na(tests_cumulative_corrected),
                                    new_tests,
                                    new_tests_corrected
      )) %>%
    dplyr::mutate(tests_cumulative_corrected = if_else(
      is.na(tests_cumulative_corrected),
      tests_cumulative,
      tests_cumulative_corrected
    )) %>%
    # recalculating new_tests after filling tests_cumulative and after implementation of workflow
    dplyr::arrange(country, date) %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(new_tests = if_else(
      dplyr::row_number() != 1 & date > as.Date("2021-02-18"),
      tests_cumulative - dplyr::lag(tests_cumulative),
      new_tests
    )) %>%
    dplyr::mutate(new_tests_corrected = if_else(
      dplyr::row_number() != 1 & date > as.Date("2021-02-18"),
      tests_cumulative_corrected - dplyr::lag(tests_cumulative_corrected),
      new_tests_corrected
    )) %>%
    dplyr::ungroup() %>%
    # When there's negative values, taking into account date when was negative
    dplyr::arrange(jhu_ID, date) %>%
    dplyr::group_by(jhu_ID) %>%
    dplyr::mutate(date_change = if_else(
      new_tests_corrected < 0,
      date,
      max(date)
    )) %>%
    dplyr::mutate(date_negative = min(date_change)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(jhu_ID) %>%
    dplyr::mutate(tests_cumulative_corrected = if_else(
      (date >= date_negative & new_tests_corrected < 0) | date > date_negative,
      NA_real_,
      tests_cumulative_corrected
    )) %>%
    dplyr::mutate(new_tests_corrected = if_else(
      is.na(tests_cumulative_corrected),
      0,
      new_tests_corrected
    )) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(jhu_ID, date) %>%
    #  when tests_cumulative_corrected is NA filling with the last value
    dplyr::group_by(jhu_ID) %>%
    tidyr::fill(tests_cumulative_corrected, .direction = "down") %>%
    dplyr::arrange(jhu_ID, date) %>%
    dplyr::select(-date_change, -date_negative) %>%
    dplyr::relocate(country, date, new_tests, tests_cumulative,
      jhu_ID, source, new_tests_corrected, tests_cumulative_corrected)

  # coronavirus_cases.csv needs to be updated before coronavirus_test.csv
  if (!file.exists("processed/coronavirus_tests.csv") ||
      file.mtime("processed/coronavirus_cases.csv") > file.mtime("processed/coronavirus_tests.csv")
  ) {
    process_jhu_data()
  }
  cv_cases <- readr::read_csv("processed/coronavirus_cases.csv", col_types = readr::cols())
  countries <- suppressWarnings(readr::read_csv("https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/raw/countries_codes_and_coordinates.csv",
    col_types = readr::cols()
  ))

  # check consistency of country names across datasets
  countries_without_coordinates <- setdiff(
    unique(cv_tests_clean$country),
    unique(countries$country)
  )

  if (length(countries_without_coordinates) > 0) {
    cli::cli_alert_info(
      "{.fun process_test_data}: Error: mapping data lacking for the following countries:
        {countries_without_coordinates}",
      wrap = TRUE
    )
  }

  cv_test_new_neg <- subset(cv_tests_clean, new_tests_corrected < 0)

  if (nrow(cv_tests_clean[cv_tests_clean$new_tests_corrected < 0, ]) > 0) {
    readr::write_csv(cv_test_new_neg, "coronavirus_tests_new_negative.csv")
    cli::cli_alert_danger("Found negative test values.")
    print(cv_test_new_neg)
  } else {
    readr::write_csv(cv_tests_clean, "processed/coronavirus_tests.csv")
  }
  cli::cli_alert_success("{.file processed/coronavirus_tests.csv}: Up to date!")

}


#' Get tests from different sources (Selenium, fetch, and manual) and combine them.
#' Using the parameter `days`, all files in [`automated/merged/`](https://github.com/finddx/FINDCov19TrackerData/tree/master/automated/merged) are updated for the last dates given the input number.
#'
#' #' When manual countries have not been updated,
#' `coronavirus_cases_new.csv` has `NA` for tests_cumulative and new_tests.
#' When selenium or fetch process failed and those values have not been corrected manually,
#' `coronavirus_cases_new.csv` might have negative values or NA in new_tests and new_tests_corrected variables.
#'
#' Countries with negative values or `NA` are listed as well given the `days` input in the folder [`issues/`](https://github.com/finddx/FINDCov19TrackerData/tree/master/issues).
#'
#' Parameter days allows to update past dates using manual files in [manual/processed/](https://github.com/finddx/FINDCov19TrackerData/tree/master/manual/processed); except for dates on "2021-02-18" or before.
#' It allows to update from "2021-02-19" when the automatic workflow was implemented.
#' To update data on 2021-02-18 or previous days, changes should be made using the specific date in the folder [automated/merged/](https://github.com/finddx/FINDCov19TrackerData/tree/master/automated/merged).
#'
#' @description
#'   **Input:** Daily test data scraped via Selenium and "R fetch functions" from
#'   [`automated/fetch`](https://github.com/finddx/FINDCov19TrackerData/tree/master/automated/fetch) and [`automated/selenium`](https://github.com/finddx/FINDCov19TrackerData/tree/master/automated/selenium) directories.
#'
#'   **Output:**
#'   - `automated-tests.json`
#'   - `countries-error.csv`
#'
#'   which are then deployed by CI to the [`automated/merged/`](https://github.com/finddx/FINDCov19TrackerData/tree/master/automated/merged) directory in the `finddx/FINDCov19TrackerData` repo.
#' @param days days to combine. It should always be bigger than 0.
#' @param write if csv files should be written. Default TRUE.
#'
#' @examples
#' get_test_data(write = FALSE)
#'
#' @importFrom dplyr left_join mutate rename relocate select
#' @export
get_test_data <- function(days = 1, write = TRUE) {

  today <- format(Sys.time(), "%Y-%m-%d")

  # it includes the day before to retrieve this date and calculate tests for day 1
  first_date <- as.Date("2021-02-18")

  if (first_date <= as.Date("2021-02-18")) {
    # Due to the implementation of automated workflow
    warning("Changes on 2021-02-18 or before, should made directly in automated/merged")
    first_date <- as.Date("2021-02-18")
    window_update <- seq(first_date, as.Date(today), by = "days")
    # Given the implementation of the automated workflow on "2021-02-19"
    # The process should not take data from selenium or fetch on "2021-02-18"
    data_2021_02_18 <- readr::read_csv(
        "https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/automated/merged/2021-02-18-automated-tests.csv", # nolint
        cols(
          country = col_character(),
          tests_cumulative = col_double(),
          new_tests = col_double(),
          tests_cumulative_corrected = col_double(),
          new_tests_corrected = col_double(),
          date = col_date(format = ""),
          source = col_character()
        ),
        col_names = TRUE
      ) %>%
      dplyr::mutate(source = "manually")
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
    col_names = TRUE
  ) %>% # nolint
    dplyr::select(jhu_ID) %>%
    dplyr::rename(country = jhu_ID) %>%
    merge(window_update) %>%
    rename(date = y)

  selenium_list <- sprintf(
    "https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/automated/selenium/%s-tests-selenium.csv", # nolint
    window_update
  )
  selenium_tests <- rio::import_list(selenium_list, rbind = TRUE) %>%
    dplyr::mutate(source = "selenium") %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::select(-`_file`)
  selenium_tests_clean <- clean_selenium(selenium_tests)
  selenium_tests_daily <- selenium_tests_clean %>%
    dplyr::mutate(new_tests = NA_real_) %>%
    dplyr::mutate(tests_cumulative_corrected = NA_real_) %>%
    dplyr::mutate(new_tests_corrected = NA_real_) %>%
    dplyr::select(-pcr_tests_cum, -rapid_test_cum) %>%
    dplyr::relocate(
      country, tests_cumulative, new_tests,
      tests_cumulative_corrected, new_tests_corrected,
      date, source
    )

  fetch_list <- sprintf(
    "https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/automated/fetch/%s-tests-R.csv", # nolint
    seq(first_date, as.Date("2021-05-12"), by = "days")
  )
  fetch_tests_daily <- rio::import_list(fetch_list, rbind = TRUE) %>%
    dplyr::select(-`_file`) %>%
    dplyr::mutate(tests_cumulative = as.numeric(tests_cumulative)) %>%
    dplyr::mutate(new_tests = as.numeric(new_tests)) %>%
    dplyr::mutate(tests_cumulative_corrected = NA_real_) %>%
    dplyr::mutate(new_tests_corrected = NA_real_) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(source = "fetch") %>%
    dplyr::relocate(
      country, tests_cumulative, new_tests,
      tests_cumulative_corrected, new_tests_corrected,
      date, source
    )

  manual_list <- sprintf(
    "https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/manual/processed/%s-processed-manually.csv", # nolint
    window_update
  )

  manual_tests_daily <- tryCatch(
    {
      fl_gh <- gh::gh("GET /repos/:owner/:repo/git/trees/master?recursive=1",
                      owner = "finddx",
                      repo = "FINDCov19TrackerData",
                      branch = "selenium"
      )

      # takes the manual files uploaded between
      # the time frame to update (first_date and today)
      filelist_manual <- unlist(lapply(fl_gh$tree, "[", "path"), use.names = FALSE) %>%
        stringr::str_subset(., "manual/processed/.*processed-manually.csv$") %>%
        paste0("https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/", .)

      manual_files <- manual_list[which(manual_list %in% filelist_manual)]

      processed_manual <- rio::import_list(manual_files,
        rbind = TRUE,
        rbind_label = "file_name"
        ) %>%
        dplyr::select(-dplyr::any_of(c("file_name", "status", "url"))) %>%
        dplyr::mutate(tests_cumulative = as.numeric(tests_cumulative)) %>%
        dplyr::mutate(new_tests = as.numeric(new_tests)) %>%
        dplyr::mutate(tests_cumulative_corrected = as.numeric(tests_cumulative_corrected)) %>%
        dplyr::mutate(new_tests_corrected = (new_tests_corrected)) %>%
        dplyr::mutate(date = as.Date(date)) %>%
        dplyr::mutate(source = "manually") %>%
        dplyr::relocate(
          country, tests_cumulative, new_tests,
          tests_cumulative_corrected, new_tests_corrected,
          date, source
        )
    },
    error = function(cond) {
      cli::cli_alert_info("No file found in folder manual/processed for last {days} days. Ignoring
                          manual test files for these days.", wrap = TRUE)
      return(NULL)
    }
  )

  if (first_date == as.Date("2021-02-18")) {
    test_combined <- dplyr::bind_rows(
      selenium_tests_daily, fetch_tests_daily,
      manual_tests_daily, data_2021_02_18
    ) %>%
      # keeping only manual source when there is multiple
      dplyr::arrange(country, date) %>%
      dplyr::group_by(country, date) %>%
      dplyr::filter(n() == 1 | source == "manually") %>%
      dplyr::ungroup()
  } else {
    test_combined <- dplyr::bind_rows(
      selenium_tests_daily, fetch_tests_daily,
      manual_tests_daily
    ) %>%
      # keeping only manual source when there is multiple
      dplyr::arrange(country, date) %>%
      dplyr::group_by(country, date) %>%
      dplyr::filter(n() == 1 | source == "manually") %>%
      dplyr::ungroup()
  }


  test_combined_all_countries <- countries_all %>%
    dplyr::left_join(test_combined) %>%
    dplyr::arrange(country, date) %>%
    dplyr::group_by(country) %>%
    # calculating tests_cumulative when new_tests is available
    dplyr::mutate(tests_cumulative = calc_cumulative_t(
      tests_cumulative,
      new_tests)) %>%
    # calculating new_tests when tests_cumulative is available
    dplyr::mutate(new_tests = calc_new_t(tests_cumulative, new_tests)) %>%
    dplyr::ungroup() %>%
    # calculating new_tests
    dplyr::arrange(country, date) %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(new_tests = if_else(
      dplyr::row_number() != 1 ,
      tests_cumulative - dplyr::lag(tests_cumulative),
      new_tests
    )) %>%
    # calculating test_cumulative
    dplyr::arrange(country, date) %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(tests_cumulative = if_else(
      dplyr::row_number() != 1 ,
      dplyr::lag(tests_cumulative) + new_tests,
      tests_cumulative
    )) %>%
    dplyr::ungroup() %>%
    # populating corrected columns
    dplyr::mutate(new_tests_corrected = if_else(
      is.na(new_tests_corrected),
      new_tests,
      new_tests_corrected
    )) %>%
    dplyr::mutate(tests_cumulative_corrected = if_else(
      is.na(tests_cumulative_corrected),
      tests_cumulative,
      tests_cumulative_corrected
    )) %>%
    dplyr::arrange(country, date) %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(new_tests_corrected = if_else(
      dplyr::row_number() != 1 ,
      tests_cumulative_corrected - dplyr::lag(tests_cumulative_corrected),
      new_tests_corrected
    )) %>%
    dplyr::ungroup() %>%
    # First date is not updated is just used to calculate new_tests
    filter(date != first_date)

  # Splitting combined data frame in data frame per days
  # to write files in folder automated/merged
  test_combined_split <- test_combined_all_countries %>%
    dplyr::arrange(country, date) %>%
    dplyr::group_by(date, .add =TRUE) %>%
    dplyr::group_split()

  if (write == TRUE) {
    mapply(
      readr::write_csv,
      test_combined_split,
      paste0(window_update[-1], "-automated-tests.csv")
    )
  }

  # get countries with NA (before 2020-03-18)
  old_errors <- readr::read_csv(
    "https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/automated/coronavirus_tests_new.csv") %>%
    dplyr::arrange(country,date) %>%
    dplyr::group_by(country) %>%
    mutate(new_tests_calc=if_else(
      dplyr::row_number() != 1,
      tests_cumulative_corrected - dplyr::lag(tests_cumulative_corrected),
      new_tests)) %>%
    dplyr::filter(new_tests_calc<0) %>%
    dplyr::filter(date <= first_date) %>%
    dplyr::select(-new_tests_corrected) %>%
    dplyr::rename(new_tests_corrected = new_tests_calc) %>%
    dplyr::relocate(
      country, date, tests_cumulative, new_tests,
      tests_cumulative_corrected, new_tests_corrected,
      source
    )

  # get countries with NA (these errored during scraping)
  new_errors <- test_combined_all_countries %>%
    dplyr::filter(is.na(tests_cumulative_corrected) | new_tests_corrected < 0) %>%
    dplyr::arrange(country, date)

  countries_error <- dplyr::bind_rows(
    old_errors,
    new_errors
  ) %>%
    dplyr::arrange(country, date)

  if (write == TRUE) {

    readr::write_csv(
      countries_error,
      "all-countries-error.csv"
    )
  }

  return(list(test_combined = test_combined_all_countries, countries_error = countries_error))
}

#' Combine test daily data from all countries across all dates
#' @description This function reads all clean input files from the [automated/merged](https://github.com/finddx/FINDCov19TrackerData/tree/master/automated/merged) directory and row-binds them. The output is written to a file called
#' `coronavirus_tests_new.csv` and uploaded to `automated/coronavirus_tests_new.csv`.
#' @importFrom stringr str_subset
#' @importFrom gh gh
#' @importFrom purrr map_dfr
#' @importFrom readr write_csv
#' @importFrom dplyr mutate arrange desc
#' @importFrom rio import_list
#' @export
combine_all_tests <- function() {

  fl_gh <- gh::gh("GET /repos/:owner/:repo/git/trees/master?recursive=1",
    owner = "finddx",
    repo = "FINDCov19TrackerData",
    branch = "selenium"
  )

  filelist <- unlist(lapply(fl_gh$tree, "[", "path"), use.names = FALSE) %>%
    stringr::str_subset(., "automated/merged/.*tests.csv$") %>%
    paste0("https://raw.githubusercontent.com/finddx/FINDCov19TrackerData/master/", .)

  files_df <- rio::import_list(filelist, rbind = TRUE) %>%
    dplyr::arrange(country,date) %>%
    dplyr::relocate(country, tests_cumulative, new_tests) %>%
    dplyr::select(country, tests_cumulative, new_tests,
     tests_cumulative_corrected, new_tests_corrected, date, source)

  # checking if due to manual files there are duplicates
  files_df_duplicated <- files_df %>%
    dplyr::group_by(country, date) %>%
    dplyr::tally() %>%
    dplyr::filter(n > 1)

  readr::write_csv(files_df, "coronavirus_tests_new.csv")
  readr::write_csv(files_df_duplicated, "duplicated_tests.csv")

}
