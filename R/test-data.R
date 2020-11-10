#' Update covid data
#'
#' During the process, `coronavirus_cases.csv` (case data) is read and used.
#' This file is created/updated by `process_jhu_data()` which should be run
#' before.
#'
#' @return Writes `coronavirus_tests.csv`
#'
#' @importFrom mailR send.mail
#' @export
process_test_data <- function() {
  fl_gh <- gh::gh("GET /repos/:owner/:repo/git/trees/master?recursive=1",
    owner = "dsbbfinddx",
    repo = "data",
    access_token = gh::gh_token()
  )
  filelist <- unlist(lapply(fl_gh$tree, "[", "path"), use.names = FALSE) %>%
    stringr::str_subset(., "coronavirus_tests_[0-9]{8}_sources_SO.csv") %>%
    stringr::str_remove(., "data/")

  most_recent <- tail(filelist, 1)

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
  cv_cases <- readr::read_csv("processed/coronavirus_cases.csv", col_types = readr::cols())
  countries <- suppressWarnings(readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/data/master/raw/countries_codes_and_coordinates.csv",
    col_types = readr::cols()
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

  cv_tests_sum <- cv_tests %>%
    dplyr::filter(date == cv_tests_max_date) %>%
    dplyr::mutate(max_date = date) %>%
    dplyr::group_by(country) %>%
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
    # mailR::send.mail(
    #   from = "anna.mantsoki@finddx.org",
    #   to = c("anna.mantsoki@finddx.org", "Imane.ElIdrissi@finddx.org"),
    #   subject = "Negative values on new tests",
    #   body = paste0(
    #     "There are ", nrow(cv_test_new_neg),
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
