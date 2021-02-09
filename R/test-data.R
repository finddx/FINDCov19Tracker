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
#' @export
get_daily_test_data <- function() {

  today <- format(Sys.time(), "%Y-%m-%d")

  selenium_tests <- readr::read_csv(sprintf("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/automated/selenium/%s-tests-selenium.csv", today), # nolint
    col_types = cols(
      country = col_character(),
      date = col_date(format = ""),
      tests_cumulative = col_character()
    ),
    quoted_na = FALSE
  ) %>% # nolint
    mutate(source = "selenium") %>%
    mutate(date = as.Date(date))
  selenium_tests_clean <- clean_selenium(selenium_tests)
  selenium_tests_daily <- calculate_daily_tests_selenium(selenium_tests_clean) %>%
    mutate(tests_cumulative_corrected = NA_real_) %>%
    mutate(new_tests_corrected = NA_real_)

  fetch_funs_tests <- readr::read_csv(sprintf("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/automated/fetch/%s-tests-R.csv", today), # nolint
    col_types = cols(
      country = col_character(),
      date = col_date(format = ""),
      new_tests = col_double(),
      tests_cumulative = col_double()
    ),
    quoted_na = FALSE
  ) %>%
    mutate(tests_cumulative = as.numeric(tests_cumulative)) %>%
    mutate(new_tests = as.numeric(new_tests)) %>%
    mutate(tests_cumulative_corrected = NA_real_) %>%
    mutate(new_tests_corrected = NA_real_) %>%
    mutate(date = as.Date(date)) %>%
    mutate(source = "fetch")

  manual_tests <- tryCatch(
    {
  processed_manual <- readr::read_csv(sprintf("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/manual/processed/%s-processed-manually.csv", today), # nolint
    col_types = cols(
      country = col_character(),
      tests_cumulative = col_double(),
      new_tests = col_double(),
      tests_cumulative_corrected = col_double(),
      new_tests_corrected = col_double(),
      date = col_date(format = ""),
      source = col_character()
    ),
    quoted_na = FALSE
  ) # nolint
      calculate_tests_manual_file(processed_manual)
    },
    error = function(cond) {
      cli::cli_alert_info("No file with manual test countries found for
          today. Ignoring input.", wrap = TRUE)
      return(NULL)
    }
  )

  test_combined <- dplyr::bind_rows(
    selenium_tests_daily, fetch_funs_tests,
    manual_tests
  ) %>%
    dplyr::group_by(country) %>%
    dplyr::filter(n() == 1 | source == "manually") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(new_tests_corrected = if_else(is.na(new_tests_corrected),
      new_tests,
      new_tests_corrected
    )) %>%
    dplyr::mutate(tests_cumulative_corrected = if_else(is.na(tests_cumulative_corrected),
      tests_cumulative,
      tests_cumulative_corrected
    )) %>%
    dplyr::arrange(date, country) %>%
    dplyr::relocate(country, tests_cumulative, new_tests, date, source) %>%
    dplyr::select(
      country, tests_cumulative, new_tests,
      tests_cumulative_corrected, new_tests_corrected, date, source
    )
  readr::write_csv(test_combined, "automated-tests.csv")

  # get countries with NA (these errored during scraping)
  countries_error <- test_combined %>%
    dplyr::filter(is.na(tests_cumulative) | new_tests < 0) %>%
    dplyr::select(country, source)
  readr::write_csv(countries_error, "countries-error.csv")

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
