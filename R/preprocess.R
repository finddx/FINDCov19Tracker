#' Scrape test data from all countries
#'
#' @description
#' Rowwise processing of countries test data reports with support for any file
#' type (csv, pdf, xlsx, etc.)
#' @export
fetch_test_data <- function() {

  info <- read_urls("https://github.com/dsbbfinddx/FINDCov19TrackerData/raw/master/manual/tests_urls.xlsx")

  #info = info[3:nrow(info), ]
  info = info[25, ]

  info %<>%
  # dplyr::mutate(xpath_new = dplyr::case_when(
  #   country == "Romania" ~ "acestea ([.0-9]+) au fost",
  #   TRUE ~ xpath_new
  # )) %>%
    # dplyr::mutate(xpath_cumul = dplyr::case_when(
    #   country == "Uruguay" ~ "(?<=procesado)(.*)(?=tests)",
    #   TRUE ~ xpath_cumul
    # )) %>%
  dplyr::mutate(data_url = dplyr::case_when(
    country == "Bulgaria" ~ "https://coronavirus.bg/",
    TRUE ~ data_url
  ))
    # dplyr::mutate(xpath_new = dplyr::case_when(
    #   country == "Uruguay" ~ "(?<=a cabo)(.*)(?=análisis)",
    #   TRUE ~ xpath_new
    # )) %>%
    # dplyr::mutate(xpath_cumul = dplyr::case_when(
    #   country == "Croatia" ~ "(?<=ukupno testirana)(.*)(?=osoba)",
    #   TRUE ~ xpath_cumul
    # )) %>%
    # dplyr::mutate(xpath_new = dplyr::case_when(
    #   country == "Croatia" ~ "(?<=osoba, od toga)(.*)(?=u protekla 24 sata)",
    #   TRUE ~ xpath_new
    # )) %>%
    # dplyr::mutate(xpath_cumul = dplyr::case_when(
    #   country == "Scotland" ~ "(?<=A total of )(.*)(?= people in Scotland)",
    #   TRUE ~ xpath_cumul
    # )) %>%
    # dplyr::mutate(xpath_new = dplyr::case_when(
    #   country == "Scotland" ~ "(?<=hospital with confirmed COVID-19\\. )(.*)(?= new tests)",
    #   TRUE ~ xpath_new
    # ))

  xlsx::write.xlsx(as.data.frame(info), fs::path_expand("~/git/cynkra/find/FINDCov19TrackerData/manual/tests_urls.xlsx"),
                   row.names = FALSE)
  res <- purrr::pmap(info, process_countries_rowwise)
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
    rep(NA, 2) # all other cases
    # more fetch functions here
  )

  purrr::set_names(res, c("new_tests", "tests_cumulative"))
  return(res)
}
