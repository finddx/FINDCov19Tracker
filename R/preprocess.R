#' Scrape test data from all countries
#'
#' @description
#' Rowwise processing of countries test data reports with support for any file
#' type (csv, pdf, xlsx, etc.)
#' @export
preprocess_test_data <- function() {

  info <- read_urls("https://github.com/dsbbfinddx/FINDCov19TrackerData/raw/master/manual/coronavirus_tests_countries_urls_CH_v7.xlsx")
  info %<>%
    dplyr::mutate(type = dplyr::case_when(
      country == "Afghanistan" ~ "html",
      TRUE ~ type
    )) %>%
    dplyr::mutate(xpath_new = dplyr::case_when(
      country == "Romania" ~ "acestea ([.0-9]+) au fost",
      TRUE ~ xpath_new
    )) %>%
    dplyr::mutate(xpath_cumul = dplyr::case_when(
      country == "Uruguay" ~ "(?<=procesado)(.*)(?=tests)",
      TRUE ~ xpath_cumul
    )) %>%
    dplyr::mutate(xpath_new = dplyr::case_when(
      country == "Uruguay" ~ "(?<=a cabo)(.*)(?=análisis)",
      TRUE ~ xpath_new
    ))
  #   dplyr::filter(country == "Afghanistan") %>%
  #   dplyr::mutate(type = "html") %>%
  #   dplyr::mutate(data_url = "https://www.humanitarianresponse.info/en/operations/afghanistan/document/afghanistan-flash-update-covid-19-strategic-situation-report-no-67") %>%
  #   dplyr::mutate(xpath_cumul = "/html/body/div[1]/div[4]/div/div[2]/div/div/div/div/div/div/div/div[1]/div/div[5]/div/div/div/div/p[2]/strong") %>%
  #   dplyr::mutate(pattern_cumul = "tested. \\d{1,4}[\\,\\.]{1}\\d{1,3}")

  res <- purrr::pmap(info, process_countries_rowwise)
  return(res)
}

process_countries_rowwise <- function(...) {
  dots <- list(...)

  if (!is.na(dots$data_url)) {
    cli::cli_alert_success("{.strong {dots$country}}: Type: {.code {dots$type}}, URL: {.url {dots$data_url}}.")
  } else {
    cli::cli_alert_danger("{.strong {dots$country}}: 'URL' field: 'NA' -> {.emph Skipping}.")
    res <- rep(NA, 2)
    purrr::set_names(res, c("new_tests", "tests_cumulative"))
    return(res)
  }

  res <- switch(dots$type,
    # xlsx = fetch_from_csv_xlsx(dots),
    # csv = fetch_from_csv_xlsx(dots),
    # json = fetch_from_json(dots),
    # html = fetch_from_html(dots),
    # zip = fetch_from_zip(dots),
    # pdf = fetch_from_pdf(dots),
    # pdf_list = fetch_from_pdf_list(dots),
    html_list = fetch_from_html_list(dots),
    rep(NA, 2) # all other cases
    # more fetch functions here
  )

  if (length(res) != 2) {
    browser()
  }

  purrr::set_names(res, c("new_tests", "tests_cumulative"))
  return(res)
}
