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

  countries_error <- readr::read_csv(sprintf("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/issues/%s-countries-error.csv", as.character(Sys.Date(), format = "%Y-%m-%d")), # nolint
    col_types = cols(
      country = col_character(),
      date = col_date(format = ""),
      source = col_character()
    ),
    quoted_na = FALSE
  ) %>% # nolint
    dplyr::pull(country)

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
