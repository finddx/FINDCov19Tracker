#' Scrape test data from all countries
#'
#' @description
#' Rowwise processing of countries test data reports with support for any file
#' type (csv, pdf, xlsx, etc.)
#'
#' @details
#' Countries processed via Selenium are not yet integrated into this mechanism.
#' For now, all countries until "Denmark" were checked and asserted  with two
#' numeric returns (2020-08-25).
#' @export
fetch_test_data <- function() {

  # FIXME: Philippines (tableau data)
  # FIXME: Denmark (Arc GIS dashboard)

  info <- read_urls("https://github.com/dsbbfinddx/FINDCov19TrackerData/raw/master/manual/tests_urls_patrick.xlsx")
  info <- info[-c(44, 127), ]

  info <- info %>%
    dplyr::filter(!is.na(type), type != "Selenium")

  # select only one country for testing purposes
  #info <- info[c(29), ]

  # info %<>%
  #   dplyr::mutate(xpath_new = dplyr::case_when(
  #     country == "Uruguay" ~ "(?<=a cabo)(.*)(?=análisis)",
  #     TRUE ~ xpath_new
  #   )) %>%


  # write data.frame in FINDCov19TrackerData repo when manual changes were made
  # xlsx::write.xlsx(as.data.frame(info), fs::path_expand("~/git/cynkra/find/FINDCov19TrackerData/manual/tests_urls_patrick.xlsx"),
  #   row.names = FALSE
  # )
  res <- purrr::pmap(info, process_countries_rowwise)

  res <- as.data.frame(do.call(rbind, res))

  return(res)
}

process_countries_rowwise <- function(...) {
  dots <- list(...)

  if (!is.na(dots$data_url)) {
    cli::cli_alert_success("{.strong {dots$country}}: Type: {.code {dots$type}}, URL: {.url {dots$data_url}}.")
  } else if (!is.na(dots$source)) {
    cli::cli_alert_success("{.strong {dots$country}}: Type: {.code {dots$type}}, URL: {.url {dots$source}}.")
  } else {
    cli::cli_alert_danger("{.strong {dots$country}}: 'URL' field: 'NA' -> {.emph Skipping}.")
    res = rep(NA, 2)
    res <- append(res, c(dots$country, as.character(Sys.Date())))
    res <- purrr::set_names(res, c("new_tests", "tests_cumulative", "country", "date"))
    return(res)
  }

  res <- switch(dots$type,
    xlsx = fetch_from_xlsx(dots),
    csv = fetch_from_csv(dots),
    json = fetch_from_json(dots),
    html = fetch_from_html(dots),
    zip = fetch_from_zip(dots),
    pdf = fetch_from_pdf(dots),
    pdf_list = fetch_from_pdf_list(dots),
    html_list = fetch_from_html_list(dots),
    html2 = fetch_from_html2(dots),
    rep(NA, 2) # all other types
  )

  # each country should return both new_tests and cumul_tests
  checkmate::assert_double(res, len = 2, any.missing = FALSE)

  # append country name
  res <- append(res, c(dots$country, as.character(Sys.Date())))

  res <- purrr::set_names(res, c("new_tests", "tests_cumulative", "country", "date"))

  return(res)
}
