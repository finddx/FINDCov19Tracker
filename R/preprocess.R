#' Scrape test data from countries via R
#'
#' @description
#' Rowwise processing of countries test data reports with support for various
#'  file type (csv, pdf, xlsx, etc.).
#'  Used for all countries which cannot be scraped via Selenium.
#'
#' @export
fetch_test_data <- function() {

  info <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/resources/countries-urls.csv") # nolint
  # info <- readr::read_csv("/Users/pjs/git/clients/find-1/FINDCov19TrackerData/resources/countries-urls.csv") # nolint
  # info <- info[-c(44, 127), ]
  # info <- info[c(47), ]
  info <- info[c(1,6), ]

  # info <- info %>%
  #   dplyr::filter(!is.na(type), type != "Selenium")

  # select only one country for testing purposes
  # info <- info[c(29), ]

  res <- purrr::pmap(info, process_countries_rowwise)

  # rowbind results
  res <- as.data.frame(do.call(rbind, res))

  # order data.frame columns
  res_ordered <- res %>%
    relocate(country, date)

  return(res_ordered)
}

process_countries_rowwise <- function(...) {
  dots <- list(...)

  if (!is.na(dots$data_url)) {
    cli::cli_alert_success("{.strong {dots$country}}: Type: {.code {dots$type}}, URL: {.url {dots$data_url}}.") # nolint
  } else if (!is.na(dots$source)) {
    cli::cli_alert_success("{.strong {dots$country}}: Type: {.code {dots$type}}, URL: {.url {dots$source}}.") # nolint
  } else {
    cli::cli_alert_danger("{.strong {dots$country}}: 'URL' field: 'NA' -> {.emph Skipping}.") # nolint
    res <- rep(NA, 2)
    res <- append(res, c(dots$country, as.character(Sys.Date())))
    res <- purrr::set_names(res, c(
      "new_tests", "tests_cumulative",
      "country", "date"
    ))
  }

  res <- switch(dots$type,
    # xlsx = fetch_from_xlsx(dots),
    # csv = fetch_from_csv(dots),
    # json = fetch_from_json(dots),
    # html = fetch_from_html(dots),
    # zip = fetch_from_zip(dots),
    pdf = fetch_from_pdf(dots),
    pdf_list = fetch_from_pdf_list(dots),
    # html_list = fetch_from_html_list(dots),
    # html2 = fetch_from_html2(dots),
    rep(NA, 2) # all other types
  )

  print(res)
  # in case of problematic returns
  if (length(res) == 0) {
    res <- c(NA, NA)
  }

  # each country should return both new_tests and cumulative_tests
  checkmate::assert_double(res, len = 2)

  # append country name
  res <- append(res, c(dots$country, as.character(Sys.Date())))

  res <- purrr::set_names(res, c(
    "new_tests", "tests_cumulative",
    "country", "date"
  ))

  return(res)
}
