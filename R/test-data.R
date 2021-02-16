#' Postprocess tests data for input into Shiny App
#'
#' During the process, `coronavirus_cases.csv` (case data) is read and used.
#' This file is created/updated by `process_jhu_data()` which needs to be run
#' before.
#'
#' @return Writes `coronavirus_tests.csv`
#'
#' @importFrom mailR send.mail
#' @importFrom utils tail
#' @export
process_test_data <- function() {
  fl_gh <- gh::gh("GET /repos/:owner/:repo/git/trees/master?recursive=1",
    owner = "dsbbfinddx",
    repo = "FINDCov19TrackerData",
    access_token = gh::gh_token()
  )
  filelist <- unlist(lapply(fl_gh$tree, "[", "path"), use.names = FALSE) %>%
    stringr::str_subset(., "coronavirus_tests_[0-9]{8}_sources_SO.csv") %>%
    stringr::str_remove(., "data/")

  most_recent <- utils::tail(filelist, 1)

  today <- format(Sys.time(), format = "%Y%m%d")

  # always read the file with the latest date - approaches using the latest
  # modification timestamp caused troubles in the past
  cv_tests <- suppressWarnings(
    readr::read_delim(sprintf(
      "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/%s",
      most_recent
    ),
    col_types = readr::cols(),
    delim = ";"
    )
  )

  cli::cli_alert_info("{.fun process_test_data}: Processing information for {.field {most_recent}}.")

  # remove empty "ind" and "X" columns
  cv_tests %<>%
    dplyr::select(-ind, -X)

  # tests file is ok, go on with the update
  cv_tests$date <- as.Date(cv_tests$date,
    tryFormats = c("%d.%m.%y", "%d/%m/%Y", "%Y/%m/%d", "%Y-%m-%d")
  )

  # be sure no NA in new_tests field
  cv_tests$new_tests <- ifelse(is.na(cv_tests$new_tests), 0, cv_tests$new_tests)
  cli::cli_alert("total count of new tests: {sum(cv_tests$new_tests)}.")

  # prevent issues in DT with non ascii characters in URL
  cv_tests$source <- iconv(cv_tests$source, from = "ISO8859-1", to = "UTF-8")

  # import data
  # coronavirus_cases.csv is created by process_jhu_data()

  # coronavirus_cases.csv needs to be updated before coronavirus_test.csv
  if (!file.exists("processed/coronavirus_tests.csv") ||
    file.mtime("processed/coronavirus_cases.csv") > file.mtime("processed/coronavirus_tests.csv")
  ) {
    process_jhu_data()
  }
  cv_cases <- readr::read_csv("processed/coronavirus_cases.csv", col_types = readr::cols(), quoted_na = FALSE)
  countries <- suppressWarnings(readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/raw/countries_codes_and_coordinates.csv",
    col_types = readr::cols(), quoted_na = FALSE
  ))

  # check consistency of country names across datasets
  countries_without_coordinates <- setdiff(
    unique(cv_tests$country),
    unique(countries$country)
  )

  if (length(countries_without_coordinates) > 0) {
    cli::cli_alert_info(
      "{.fun process_test_data}: Error: mapping data lacking for the following countries:
        {countries_without_coordinates}",
      wrap = TRUE
    )
  }

  cv_cases_max_date <- max(cv_cases$date)
  cv_tests_max_date <- max(cv_tests$date)
  cv_max_date <- max(c(cv_cases_max_date, cv_tests_max_date))

  cv_tests_sum <-  cv_tests %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(max_date = max(date)) %>%
    dplyr::filter(date == max_date) %>%
    plyr::mutate(max_date = date) %>%
    dplyr::mutate(last_tests_cum = tests_cumulative) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, country, last_tests_cum, jhu_ID)

  cv_tests_summ_missing <- subset(cv_tests_sum, date < cv_max_date)

  if (length(cv_tests_summ_missing$country >= 1)) {
    cli::cli_alert_info("{.fun process_test_data}: Adding new tests data information to
        {.file coronavirus_tests.csv} for countries:", wrap = TRUE)
    cli::cli_ul(cv_tests_summ_missing$country)
  }
  cv_tests_added <- purrr::map(cv_tests_summ_missing$country, ~ {
    df <- subset(cv_tests_sum, country == .x)
    dates <- seq(lubridate::ymd(df$date) + 1,
      lubridate::ymd(cv_max_date),
      by = "day"
    )
    df_add <- data.frame(
      ind = "", country = df$country, date = dates,
      new_tests = rep(0, length(dates)),
      tests_cumulative = rep(df$last_tests_cum, length(dates)),
      jhu_ID = df$jhu_ID
    )
  })

  cv_tests_added <- dplyr::bind_rows(cv_tests_added)

  cv_tests_new <- cv_tests %>%
    dplyr::bind_rows(cv_tests_added)

  cv_test_new_neg <- subset(cv_tests_new, new_tests_corrected < 0)

  if (nrow(cv_test_new_neg) > 0) {
    readr::write_csv(cv_test_new_neg, "issues/coronavirus_tests_new_negative.csv")
    cli::cli_alert_danger("Found negative test values.")
    print(cv_test_new_neg)
    # mailR::send.mail(
    #   from = "anna.mantsoki@finddx.org",
    #   to = c("anna.mantsoki@finddx.org", "Imane.ElIdrissi@finddx.org"),
    #   subject = "Negative values on new tests",
    #   body = paste0(
    #     "There are ", nrow(cv_test_new_neg),`
    #     "new tests values in the coronavirus_tests.csv file"
    #   ),
    #   smtp = list(host.name = "aspmx.l.google.com", port = 25),
    #   authenticate = FALSE,
    #   send = TRUE
    # )
  } else {
    readr::write_csv(cv_tests_new, "processed/coronavirus_tests.csv")
  }
  cli::cli_alert_success("{.file processed/coronavirus_tests.csv}: Up to date!")
}

#' Get and combine test data from different sources
#' @description
#'   **Input:** Daily test data scraped via Selenium and "R fetch functions" from
#'   [`automated/fetch`](https://github.com/dsbbfinddx/FINDCov19TrackerData/tree/master/automated/fetch) and [`automated/selenium`](https://github.com/dsbbfinddx/FINDCov19TrackerData/tree/master/automated/selenium) directories.
#'
#'   **Output:**
#'   - `automated-tests.json`
#'   - `countries-error.csv`
#'
#'   which are then deployed by CI to the [`automated/merged/`](https://github.com/dsbbfinddx/FINDCov19TrackerData/tree/master/automated/merged) directory in the `dsbbfinddx/FINDCov19TrackerData` repo.
#' @param days days to combine. It should always be bigger than 0.
#' @param write if csv files should be written. Default TRUE.
#' @export
get_test_data <- function(days = 1, write = TRUE) {

  today <- format(Sys.time(), "%Y-%m-%d")

  # it includes the day before to retrieve this date and calculate tests for day 1
  first_date <- as.Date(today) - days

  window_update <- seq(first_date, as.Date(today), by = "days")

  selenium_list <- sprintf(
    "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/automated/selenium/%s-tests-selenium.csv", # nolint
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
    dplyr::relocate(
      country, tests_cumulative, new_tests,
      tests_cumulative_corrected, new_tests_corrected,
      date, source
    )

  fetch_list <- sprintf(
    "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/automated/fetch/%s-tests-R.csv", # nolint
    seq(first_date, as.Date(today), by = "days")
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
    "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/manual/processed/%s-processed-manually.csv", # nolint
    window_update
  )

  manual_tests_daily <- tryCatch(
    {
      fl_gh <- gh::gh("GET /repos/:owner/:repo/git/trees/master?recursive=1",
        owner = "dsbbfinddx",
        repo = "FINDCov19TrackerData",
        branch = "selenium"
      )

      filelist_manual <- unlist(lapply(fl_gh$tree, "[", "path"), use.names = FALSE) %>%
        stringr::str_subset(., "manual/processed/.*processed-manually.csv$") %>%
        paste0("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/", .)

      manual_files <- manual_list[which(manual_list %in% filelist_manual)]

      processed_manual <- rio::import_list(manual_files, rbind = TRUE) %>%
        dplyr::select(-`_file`, -status, -url) %>%
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
      cli::cli_alert_info("No file with manual test countries found for
                today. Ignoring input.", wrap = TRUE)
      return(NULL)
    }
  )

  test_combined <- dplyr::bind_rows(
    selenium_tests_daily, fetch_tests_daily,
    manual_tests_daily
  ) %>%
    # keeping only manual source when there is multiple
    dplyr::arrange(country, date) %>%
    dplyr::group_by(country, date) %>%
    dplyr::filter(n() == 1 | source == "manually") %>%
    dplyr::ungroup() %>%
    # calculating tests_cumulative when new_tests is available
    dplyr::arrange(country, date) %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(tests_cumulative = if_else(dplyr::row_number() != 1 &
      is.na(tests_cumulative) &
      !is.na(new_tests),
      dplyr::lag(tests_cumulative) + new_tests,
    tests_cumulative
    )) %>%
    # calculating new_tests
    dplyr::arrange(country, date) %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(new_tests = if_else(dplyr::row_number() != 1,
      tests_cumulative - dplyr::lag(tests_cumulative),
      new_tests
    )) %>%
    dplyr::ungroup() %>%
    # populating corrected columns
    dplyr::mutate(new_tests_corrected = if_else(is.na(new_tests_corrected),
      new_tests,
      new_tests_corrected
    )) %>%
    dplyr::mutate(tests_cumulative_corrected = if_else(is.na(tests_cumulative_corrected),
      tests_cumulative,
      tests_cumulative_corrected
    )) %>%
    dplyr::arrange(date, country)

  test_combined_split <- test_combined %>%
    dplyr::group_split(date)
  # The firs date didn't change
  test_combined_split <- test_combined_split[-1]

  if (write == TRUE) {
    mapply(
      readr::write_csv,
      test_combined_split,
      paste0(window_update[-1], "-automated-tests.csv")
    )
  }

  # get countries with NA (these errored during scraping)
  countries_error <- test_combined %>%
    dplyr::filter(is.na(tests_cumulative_corrected) | new_tests_corrected < 0) %>%
    dplyr::select(country, date, source)

  countries_error_split <- countries_error %>%
    dplyr::group_split(date)
  # The firs date didn't change
  countries_error_split <- countries_error_split[-1]
  countries_error_date <- as.Date(unique(countries_error$date))
  countries_error_date <- countries_error_date[which(as.Date(unique(countries_error$date)) != first_date)]

  if (write == TRUE) {
    mapply(
      readr::write_csv,
      countries_error_split,
      paste0(countries_error_date, "-countries-error.csv")
    )
  }

  return(invisible(test_combined))
}

#' Combine test daily data from all countries across all dates
#' @description This function reads all clean input files from the [automated/merged](https://github.com/dsbbfinddx/FINDCov19TrackerData/tree/master/automated/merged) directory and row-binds them. The output is written to a file called
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
    owner = "dsbbfinddx",
    repo = "FINDCov19TrackerData",
    branch = "selenium"
  )

  filelist <- unlist(lapply(fl_gh$tree, "[", "path"), use.names = FALSE) %>%
    stringr::str_subset(., "automated/merged/.*tests.csv$") %>%
    paste0("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/", .)

  files_df <- rio::import_list(filelist, rbind = TRUE) %>%
    dplyr::arrange(dplyr::desc(date, country)) %>%
    dplyr::relocate(country, tests_cumulative, new_tests) %>%
    dplyr::select(country, tests_cumulative, new_tests,
     tests_cumulative_corrected, new_tests_corrected, date, source)

  readr::write_csv(files_df, "coronavirus_tests_new.csv")
}
