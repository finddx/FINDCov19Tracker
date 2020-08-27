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

  info <- read_urls("https://github.com/dsbbfinddx/FINDCov19TrackerData/raw/master/manual/tests_urls.xlsx")

  # select only one country for testing purposes
  # info <- info[42, ]

  # info %<>%
    # dplyr::mutate(data_url = dplyr::case_when(
    #   country == "Denmark" ~ "https://www.ssi.dk/sygdomme-beredskab-og-forskning/sygdomsovervaagning/c/covid19-overvaagning",
    #   TRUE ~ data_url
    # )) %>%
    # dplyr::mutate(xpath_new = dplyr::case_when(
    #   country == "Uruguay" ~ "(?<=a cabo)(.*)(?=análisis)",
    #   TRUE ~ xpath_new
    # )) %>%
    # dplyr::mutate(xpath_cumul = dplyr::case_when(
    #   country == "Afghanistan" ~ "(?<=healthcare workers\\)\\. )(.*)(?= people out of)",
    #   TRUE ~ xpath_cumul
    # ))
    # dplyr::mutate(xpath_new = dplyr::case_when(
    #   country == "Czech Republic" ~ "/html/body/main/div[3]/div[1]/div[1]/div[1]/div/p[2]/span[2]",
    #   TRUE ~ xpath_new
    # ))
  # dplyr::mutate(xpath_cumul = dplyr::case_when(
  #   country == "Scotland" ~ "(?<=A total of )(.*)(?= people in Scotland)",
  #   TRUE ~ xpath_cumul
  # )) %>%
  # dplyr::mutate(xpath_new = dplyr::case_when(
  #   country == "Denmark" ~ "//*[@id=\"top\"]/div[2]/section[6]/div[1]/table/tbody/tr[3]/td[3]",
  #   TRUE ~ xpath_new
  # ))

  # write data.frame in FINDCov19TrackerData repo when manual changes were made
  # xlsx::write.xlsx(as.data.frame(info), fs::path_expand("~/git/cynkra/find/FINDCov19TrackerData/manual/tests_urls.xlsx"),
  #   row.names = FALSE
  # )
  res <- purrr::pmap(info, process_countries_rowwise)

  # each country should return both new_tests and cumul_tests
  checkmate::assert_integer(res, len = 2)

  return(res)
}

process_countries_rowwise <- function(...) {
  dots <- list(...)

  if (!is.na(dots$data_url) || !is.na(dots$source)) {
    cli::cli_alert_success("{.strong {dots$country}}: Type: {.code {dots$type}}, URL: {.url {dots$data_url}}.")
  } else {
    cli::cli_alert_danger("{.strong {dots$country}}: 'URL' field: 'NA' -> {.emph Skipping}.")
    res <- rep(NA, 2)
    purrr::set_names(res, c("new_tests", "tests_cumulative"))
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

  purrr::set_names(res, c("new_tests", "tests_cumulative"))
  return(res)
}
