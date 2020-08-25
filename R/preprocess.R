#' Scrape test data from all countries
#'
#' @description
#' Rowwise processing of countries test data reports with support for any file
#' type (csv, pdf, xlsx, etc.)
#' @export
preprocess_test_data <- function() {

  info <- read_urls("https://github.com/dsbbfinddx/FINDCov19TrackerData/raw/master/manual/tests_urls.xlsx")

  #info = info[3:nrow(info), ]
  info = info[7, ]

  info %<>%
    # dplyr::mutate(type = dplyr::case_when(
    #   country == "Afghanistan" ~ "pdf_list",
    #   TRUE ~ type
    # )) %>%
    # dplyr::mutate(data_url = dplyr::case_when(
    #   country == "Afghanistan" ~ "https://www.humanitarianresponse.info/sites/www.humanitarianresponse.info/files/documents/files/",
    #   TRUE ~ data_url
    # )) %>%
    # dplyr::mutate(xpath_cumul = dplyr::case_when(
    #   country ==  "Afghanistan" ~ "(?<=tested: )(.*)(?= Key concerns)",
    #   TRUE ~ xpath_cumul
    # )) %>%
    # # dplyr::mutate(xpath_new = dplyr::case_when(
    # #   country == "Albania" ~ "getData_json[['tabs']][['teste_gjithesej_dje']]",
    # #   TRUE ~ xpath_new
    # # )) %>%
    # dplyr::mutate(type = dplyr::case_when(
    #   country == "Albania" ~ "html_list",
    #   TRUE ~ type
    # )) %>%
    # dplyr::mutate(data_url = dplyr::case_when(
    #   country == "Albania" ~ "https://new.shendetesia.gov.al/covid-19",
    #   TRUE ~ data_url
    # )) %>%
    # dplyr::mutate(xpath_cumul = dplyr::case_when(
    #   country == "Albania" ~ "(?<=Testime totale)(.*)(?=Raste pozitive)",
    #   TRUE ~ xpath_cumul
    # )) %>%
    # dplyr::mutate(xpath_new = dplyr::case_when(
    #   country == "Albania" ~ "(?<=janë kryer)(.*)(?=testime\\.)",
    #   TRUE ~ xpath_new
    # )) #%>%
    # dplyr::mutate(type = dplyr::case_when(
    #   country == "Argentina" ~ "pdf",
    #   TRUE ~ type
    # )) %>%
    # dplyr::mutate(data_url = dplyr::case_when(
    #   country == "Argentina" ~ "https://www.argentina.gob.ar/sites/default/files/DATE-reporte-matutino-covid-19.pdf",
    #   TRUE ~ data_url
    # )) %>%
    # dplyr::mutate(xpath_cumul = dplyr::case_when(
    #   country == "Argentina" ~ "(?<=se realizaron)(.*)(?=pruebas diagn)",
    #   TRUE ~ xpath_cumul
    # )) %>%
    # dplyr::mutate(xpath_new = dplyr::case_when(
    #   country == "Argentina" ~ "(?<=fueron realizadas)(.*)(?=nuevas muestras)",
    #   TRUE ~ xpath_new
    # ))
    # dplyr::mutate(date_format = dplyr::case_when(
    #   country == "Argentina" ~ "%d-%m-%Y",
    #   TRUE ~ date_format
    # )) %>%
    # dplyr::mutate(xpath_new = dplyr::case_when(
    #   country == "Romania" ~ "acestea ([.0-9]+) au fost",
    #   TRUE ~ xpath_new
    # )) %>%
    # dplyr::mutate(xpath_cumul = dplyr::case_when(
    #   country == "Uruguay" ~ "(?<=procesado)(.*)(?=tests)",
    #   TRUE ~ xpath_cumul
    # )) %>%
    # dplyr::mutate(xpath_new = dplyr::case_when(
    #   country == "Uruguay" ~ "(?<=a cabo)(.*)(?=análisis)",
    #   TRUE ~ xpath_new
    # )) %>%
    # dplyr::mutate(xpath_cumul = dplyr::case_when(
    #   country == "Armenia" ~ '(?<=Ընդհանուր թեստեր - )(.*?)(?=\\\")',
    #   TRUE ~ xpath_cumul
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

  # xlsx::write.xlsx(as.data.frame(info), fs::path_expand("~/git/cynkra/find/FINDCov19TrackerData/manual/tests_urls.xlsx"),
  #                  row.names = FALSE)
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

  browser()

  res <- switch(dots$type,
    xlsx = fetch_from_csv_xlsx(dots),
    csv = fetch_from_csv_xlsx(dots),
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

  if (length(res) != 2) {
    browser()
  }

  purrr::set_names(res, c("new_tests", "tests_cumulative"))
  return(res)
}
