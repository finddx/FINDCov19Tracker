get_status <- function(url) {
  return(data.frame(url = url, response_code = rvest::html_session(url) %>%
    httr::status_code()))
}

read_urls <- function(path) {
  tf <- tempfile(fileext = ".xlsx")
  curl::curl_download(path, tf)
  file <- readxl::read_xlsx(tf, sheet = 1)
  return(file)
}
