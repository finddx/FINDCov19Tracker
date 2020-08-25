# collect_all <- function(r) {
#   type <- info[[r, "type"]]
#   if (!is.na(type)) {
#     country <- info[[r, "country"]]
#     message(country)
#     if (type %in% c("csv", "xlsx")) {
#       data <- fetch_from_csv_xlsx(type, info[[r, "data_url"]], info[[r, "date_format"]], info[[r, "xpath_cumul"]], info[[r, "xpath_new"]], info[[r, "backlog"]])
#     } else if (type == "html") {
#       data <- fetch_from_html(info[[r, "source"]], info[[r, "xpath_cumul"]], info[[r, "xpath_new"]])
#     } else if (type == "json") {
#       data <- fetch_from_json(info[[r, "data_url"]], info[[r, "xpath_cumul"]], info[[r, "xpath_new"]])
#     } else if (type == "jsonstat") {
#       data <- fetch_from_jsonstat(info[[r, "data_url"]], info[[r, "xpath_cumul"]], info[[r, "xpath_new"]])
#     } else if (type == "pdf") {
#       data <- fetch_from_pdf(country, info[[r, "data_url"]], info[[r, "date_format"]], info[[r, "xpath_cumul"]])
#     } else if (type == "pdf_list") {
#       data <- fetch_from_pdf_list(info[[r, "source"]], info[[r, "data_url"]], info[[r, "xpath_cumul"]])
#     } else if (type == "html2") {
#       data <- fetch_from_html2(info[[r, "data_url"]], info[[r, "date_format"]], info[[r, "xpath_cumul"]])
#     } else if (type == "html_list") {
#       data <- fetch_from_html_list(info[[r, "source"]], info[[r, "data_url"]], info[[r, "xpath_cumul"]])
#     } else if (type == "zip") {
#       data <- fetch_from_zip(info[[r, "data_url"]], info[[r, "xpath_cumul"]])
#     } else if (type == "Selenium") {
#       data <- fetch_from_selenium(info[[r, "country"]], info[[r, "xpath_cumul"]])
#     }
#     return(data)
#   }
#   return(NA)
# }
#
fetch_from_csv <- function(dots) {
  if (dots$country == "USA") {
  }
  if (!is.na(dots$date_format)) { # for now only Costa Rica, updated day before
    yesterday_char <- as.character(Sys.Date() - 1, dots$date_format)
    dots$data_url <- gsub("DATE", yesterday_char, dots$data_url)
  }
  data <- rio::import(dots$data_url, format = "csv")
  # remove missing data
  # this removes rows which only have NA or "" in all columns
  data <- data %>%
    janitor::remove_empty(which = c("rows", "cols"))

  if (!is.na(dots$xpath_cumul)) {
    seps <- c(
      stringr::str_extract(dots$xpath_cumul, "[,;]"),
      stringr::str_extract(dots$xpath_new, "[,;]")
    )
    sep <- seps[which(!is.na(seps))]
    idx <- unlist(stringr::str_split(dots$xpath_cumul, sep))

    # account for some special cases
    # - totalTestResultsIncrease is dropped in favor of totalTestResults, which
    # would lead to clashes down the road
    data <- data %>%
      dplyr::select_if(!(names(.) %in% c("totalTestResultsIncrease")))

    cols <- grep(idx[[2]], names(data))
    data[, cols] <- sapply(data[, cols], as.numeric)
    type <- idx[[1]]
    if (type == "last") {
      tests_cumulative <- data[nrow(data), cols]
    } else if (type == "sum") {
      tests_cumulative <- sum(na.omit(data[, cols]))
    } else {
      tests_cumulative <- data[as.integer(type), cols]
    }
    tests_cumulative <- as.numeric(tests_cumulative)
    tests_cumulative <- tests_cumulative + proc_backlog
    checkmate::assert_numeric(tests_cumulative, max.len = 1)
  }

  if (!is.na(dots$xpath_new)) {
    seps <- c(
      stringr::str_extract(dots$xpath_cumul, "[,;]"),
      stringr::str_extract(dots$xpath_new, "[,;]")
    )
    sep <- seps[which(!is.na(seps))]
    idx <- unlist(stringr::str_split(dots$xpath_new, sep))
    cols <- grep(idx[[2]], names(data))
    data[, cols] <- sapply(data[, cols], as.numeric)
    type <- idx[[1]]
    if (type == "last") {
      new_tests <- data[nrow(data), cols]
    } else if (type == "sum") {
      new_tests <- sum(na.omit(data[, cols]))
    } else {
      new_tests <- data[as.integer(type), cols]
    }
    new_tests <- as.numeric(new_tests)
    checkmate::assert_numeric(new_tests, max.len = 1)
  }

  if (is.na(dots$xpath_new)) {
    new_tests <- calculate_new_tests(dots, tests_cumulative)
  }

  check_country(dots, tests_cumulative = tests_cumulative, new_tests = new_tests)

  return(c(new_tests, tests_cumulative))
}

fetch_from_xlsx <- function(dots) {

  tests_cumulative <- NA
  new_tests <- NA
  proc_backlog <- ifelse(is.na(dots$backlog), 0, as.numeric(dots$backlog))

  tmpfile <- tempfile("country_data", fileext = ".xlsx")

  if (!is.na(dots$date_format)) {
    today_char <- as.character(Sys.Date(), dots$date_format)
    yesterday_char <- as.character(Sys.Date() - 1, dots$date_format)

    tryCatch(
      {
        download.file(gsub("DATE", today_char, dots$data_url),
          destfile = tmpfile, quiet = TRUE, mode =
            "wb"
        )
      },
      silent = FALSE,
      condition = function(err) { }
    )
    if (file.size(tmpfile) == 0) {
      tryCatch(
        {
          download.file(gsub("DATE", yesterday_char, dots$data_url),
            destfile = tmpfile, quiet = FALSE, mode =
              "wb"
          )
        },
        silent = FALSE,
        condition = function(err) { }
      )
      if (file.size(tmpfile) == 0) {
        return(c(new_tests, tests_cumulative))
      }
    }
  } else {
    tryCatch(
      {
        download.file(url,
          destfile = tmpfile, quiet = FALSE, mode =
            "wb"
        )
      },
      silent = FALSE,
      condition = function(err) { }
    )
    if (file.size(tmpfile) == 0) {
      return(c(new_tests, tests_cumulative))
    }
  }

  data <- readxl::read_excel(tmpfile, sheet = 1, progress = FALSE)
  sep <- ","

  if (!is.na(dots$xpath_cumul)) {
    seps <- c(
      stringr::str_extract(dots$xpath_cumul, "[,;]"),
      stringr::str_extract(dots$xpath_new, "[,;]")
    )
    sep <- seps[which(!is.na(seps))]
    idx <- unlist(stringr::str_split(dots$xpath_cumul, sep))

    # account for some special cases
    # - totalTestResultsIncrease is dropped in favor of totalTestResults, which
    # would lead to clashes down the road
    data <- data %>%
      dplyr::select_if(!(names(.) %in% c("totalTestResultsIncrease")))

    cols <- grep(idx[[2]], names(data))
    data[, cols] <- sapply(data[, cols], as.numeric)
    type <- idx[[1]]
    if (type == "last") {
      tests_cumulative <- data[nrow(data), cols]
    } else if (type == "sum") {
      tests_cumulative <- sum(na.omit(data[, cols]))
    } else {
      tests_cumulative <- data[as.integer(type), cols]
    }
    tests_cumulative <- as.numeric(tests_cumulative)
    tests_cumulative <- tests_cumulative + proc_backlog
    checkmate::assert_numeric(tests_cumulative, max.len = 1)
  }

  if (!is.na(dots$xpath_new)) {
    seps <- c(
      stringr::str_extract(dots$xpath_cumul, "[,;]"),
      stringr::str_extract(dots$xpath_new, "[,;]")
    )
    sep <- seps[which(!is.na(seps))]
    idx <- unlist(stringr::str_split(dots$xpath_new, sep))
    cols <- grep(idx[[2]], names(data))
    data[, cols] <- sapply(data[, cols], as.numeric)
    type <- idx[[1]]
    if (type == "last") {
      new_tests <- data[nrow(data), cols]
    } else if (type == "sum") {
      new_tests <- sum(na.omit(data[, cols]))
    } else {
      new_tests <- data[as.integer(type), cols]
    }
    new_tests <- as.numeric(new_tests)
    checkmate::assert_numeric(new_tests, max.len = 1)
  }

  if (is.na(dots$xpath_new)) {
    new_tests <- calculate_new_tests(dots, tests_cumulative)
  }

  check_country(dots, tests_cumulative = tests_cumulative, new_tests = new_tests)

  return(c(new_tests, tests_cumulative))
}

fetch_from_json <- function(dots) {

  tests_cumulative <- NA
  new_tests <- NA

  getData <- httr::GET(dots$data_url)
  getData_text <- httr::content(getData, "text", encoding = "UTF-8")
  getData_json <- jsonlite::fromJSON(getData_text, flatten = TRUE)

  if (!is.na(dots$xpath_cumul)) {
    # FIXME: prefix namespace of last so we do not need to import all of dplyr
    # because of "dplyr::last()" embedded in xpath_cumul
    library(dplyr)
    tests_cumulative <- as.numeric(eval(parse(text = dots$xpath_cumul)))
  }
  if (!is.na(dots$xpath_new)) {
    new_tests <- as.numeric(eval(parse(text = dots$xpath_new)))
  }

  if (is.na(dots$xpath_new)) {
    new_tests <- calculate_new_tests(dots, tests_cumulative)
  }

  check_country(dots, new_tests = new_tests, tests_cumulative = tests_cumulative)
  return(c(new_tests, tests_cumulative))
}

is.error <- function(x) inherits(x, "try-error")

fetch_from_html <- function(dots) {

  tests_cumulative <- NA
  new_tests <- NA

  page <- try(xml2::read_html(dots$data_url), silent = TRUE)
  if (is.error(page)) {
    page <- try(xml2::read_html(url(dots$data_url)), silent = TRUE)
    if (is.error(page)) {
      return(c(new_tests, tests_cumulative))
    }
  }
  if (!is.na(dots$xpath_cumul)) {
    text <- page %>%
      rvest::html_node(xpath = dots$xpath_cumul) %>%
      rvest::html_text()
    tests_cumulative <- stringr::str_extract(text, dots$pattern_cumul)

    if (dots$country == "Afghanistan") {
      tests_cumulative <- as.numeric(
        gsub(",",
          replacement = "",
          stringr::str_extract(tests_cumulative, "[0-9].*,.*")
        )
      )
      checkmate::assert_int(tests_cumulative)
    }
  }
  if (!is.na(dots$xpath_new)) {
    text <- page %>%
      rvest::html_node(xpath = dots$xpath_new) %>%
      rvest::html_text()
    new_tests <- as.numeric(gsub("[^0-9]", "", dots$pattern_new))
  }
  return(c(new_tests, tests_cumulative))
}

fetch_from_pdf <- function(dots) {

  # if (!dots$country == "Switzerland") {
  #   stop(sprintf(
  #     "Country %s is not supported yet for PDF extraction.",
  #     dots$country
  #   ))
  # }

  tests_cumulative <- NA
  new_tests <- NA

  tmpfile <- tempfile("country_data", fileext = ".pdf")

  if (is.na(dots$date_format)) {
    tryCatch(
      {
        download.file(dots$data_url,
          destfile = tmpfile, quiet = TRUE,
        )
      },
      silent = FALSE,
      condition = function(err) { }
    )
    if (file.size(tmpfile) == 0) {
      return(c(new_tests, tests_cumulative))
    }
  } else {
    # today_char <-  as.character(Sys.Date(), dots$date_format)
    yesterday_char <- as.character(Sys.Date() - 1, dots$date_format)

    # FIXME: only use pdf_text() instead of tabulizer?
    # tryCatch(
    #   {
    #     download.file(gsub("DATE", today_char, dots$data_url),
    #       destfile = tmpfile, quiet = TRUE
    #     )
    #   },
    #   silent = FALSE,
    #   condition = function(err) { }
    # )
    # if (file.size(tmpfile) == 0) {
    #   tryCatch(
    #     {
    #       download.file(gsub("DATE", yesterday_char, dots$data_url),
    #         destfile = tmpfile, quiet = FALSE, mode =
    #           "wb"
    #       )
    #     },
    #     silent = FALSE,
    #     condition = function(err) { }
    #   )
    #   if (file.size(tmpfile) == 0) {
    #     return(c(new_tests, tests_cumulative))
    #   }
    # }
  }
  # if (dots$country == "Switzerland") {
  #   table <- tabulizer::extract_tables(tmpfile,
  #     guess = FALSE,
  #     area = list(c(379, 201, 389, 326)),
  #     output = "character"
  #   )[[1]] %>%
  #     stringr::str_split(., pattern = "\t") %>%
  #     magrittr::extract2(1) %>%
  #     stringr::str_replace(., pattern = " ", replacement = "") %>%
  #     stringr::str_remove(., pattern = "\\+")
  #
  #   new_tests <- table[2]
  #   tests_cumulative <- table[1]
  # }

  content <- pdftools::pdf_text(gsub("DATE", yesterday_char, dots$data_url))

  tests_cumulative <- na.omit(
    as.numeric(
      stringr::str_replace_all(
        stringr::str_extract(stringr::str_squish(
          content
        ), dots$xpath_cumul),
        "[.]|[,]", ""
      )
    )
  )

  new_tests <- na.omit(
    as.numeric(
      stringr::str_replace_all(
        stringr::str_extract(stringr::str_squish(
          content
        ), dots$xpath_new),
        "[.]|[,]", ""
      )
    )
  )

  # in case something goes wrong, we fall back to the manual calculation of new_tests
  if (length(new_tests) == 0) {
    dots$xpath_new <- NA
  }

  if (is.na(dots$xpath_new)) {
    new_tests <- calculate_new_tests(dots, tests_cumulative)
  }

  check_country(dots, tests_cumulative = tests_cumulative, new_tests = new_tests)

  return(c(new_tests, tests_cumulative))
}

fetch_from_pdf_list <- function(dots) {
  tests_cumulative <- NA
  new_tests <- NA

  page <- xml2::read_html(dots$source)
  hrefs <- rvest::html_attr(rvest::html_nodes(page, "a"), "href")

  pdfs <- grep(dots$data_url, hrefs, ignore.case = TRUE, value = TRUE)

  pdf <- pdfs[1]

  content <- pdftools::pdf_text(pdf)

  tests_cumulative <- na.omit(
    as.numeric(
      stringr::str_replace_all(
        stringr::str_extract(stringr::str_squish(
          content
        ), dots$xpath_cumul),
        "[.]|[,]", ""
      )
    )
  )

  new_tests <- na.omit(
    as.numeric(
      stringr::str_replace_all(
        stringr::str_extract(stringr::str_squish(
          content
        ), dots$xpath_new),
        "[.]|[,]", ""
      )
    )
  )

  if (is.na(dots$xpath_new)) {
    new_tests <- calculate_new_tests(dots, tests_cumulative)
  }

  check_country(dots, pdfs = pdfs, tests_cumulative = tests_cumulative, new_tests = new_tests)

  return(c(new_tests, tests_cumulative))
}

fetch_from_html_list <- function(dots) {
  tests_cumulative <- NA
  new_tests <- NA

  # FIXME
  if (dots$country == "Greece") {
    return(c(new_tests, tests_cumulative))
  }

  if (dots$country == "Uruguay") {
    browser()
  }

  page <- xml2::read_html(dots$source)
  hrefs <- rvest::html_attr(rvest::html_nodes(page, "a"), "href")

  urls <- grep(dots$data_url, hrefs, ignore.case = TRUE, value = TRUE)

  url <- urls[1]

  content <- xml2::read_html(url) %>%
    rvest::html_text()
  tests_cumulative <- as.numeric(
    stringr::str_squish(na.omit(stringr::str_extract(content, dots$xpath_cumul)))
  )

  new_tests <- as.numeric(
    stringr::str_squish(na.omit(stringr::str_extract(content, dots$xpath_new)))
  )

  check_country(dots, new_tests = new_tests, tests_cumulative = tests_cumulative)

  return(c(new_tests, tests_cumulative))
}

fetch_from_html2 <- function(dots) {

  tests_cumulative <- NA
  new_tests <- NA

  browser()

  if (!is.na(dots$date_format)) {
    today_char <- as.character(Sys.Date(), dots$date_format)
    yesterday_char <- as.character(Sys.Date() - 1, dots$date_format)

    page <- try(xml2::read_html(gsub("DATE", today_char, dots$data_url)), silent = TRUE)
    if (is.error(page)) {
      page <- try(xml2::read_html(gsub("DATE", yesterday_char, dots$data_url)), silent = TRUE)
      if (is.error(page)) {
        return(c(new_tests, tests_cumulative))
      }
    }

  } else {
    page <- try(xml2::read_html(dots$data_url), silent = TRUE)
    if (is.error(page)) {
      page <- try(xml2::read_html(url(dots$data_url)), silent = TRUE)
      if (is.error(page)) {
        return(c(new_tests, tests_cumulative))
      }
    }
  }
  content <- page %>%
    rvest::html_text()
  tests_cumulative <- as.numeric(
    stringr::str_replace_all(
      stringr::str_extract(stringr::str_squish(
        content
      ), dots$xpath_cumul),
      "[.]|[,]", ""
    )
  )

  new_tests <- as.numeric(
    stringr::str_replace_all(
      stringr::str_extract(stringr::str_squish(
        content
      ), dots$xpath_new),
      "[.]|[,]", ""
    )
  )

  if (is.na(dots$xpath_new)) {
    new_tests <- calculate_new_tests(dots, tests_cumulative)
  }

  check_country(dots, new_tests = new_tests, tests_cumulative = tests_cumulative)

  return(c(new_tests, tests_cumulative))
}

fetch_from_zip <- function(dots) {

  tests_cumulative <- NA
  new_tests <- NA

  tmpfile <- tempfile("country_data", fileext = ".zip")
  tryCatch(
    {
      download.file(dots$data_url,
        destfile = tmpfile, quiet = TRUE,
      )
    },
    silent = FALSE,
    condition = function(err) { }
  )

  if (file.size(tmpfile) > 0) {
    file <- unzip(tmpfile)
    data <- rio::import(file)
    unlink(file)

    if (dots$xpath_cumul == "nrow") {
      tests_cumulative <- nrow(data)
    }
  }

  return(c(new_tests, tests_cumulative))
}

# fetch_from_selenium <- function(country, pattern) {
#   tests_cumulative <- NA
#   new_tests <- NA

#   today <- Sys.Date()
#   today_str <- as.character(today, format = "%Y%m%d")

#   country_to_grep <- country
#   if (country == "Cape Verde") {
#     country_to_grep <- "Cabo Verde"
#   } else if (country == "The Gambia") {
#     country_to_grep <- "Gambia"
#   } else if (country == "United Republic of Tanzania") {
#     country_to_grep <- "Tanzania"
#   }
#   line <- grep(paste0("echo: ", country_to_grep, ";"), readLines(paste0("output_selenium_", today_str, ".txt"), encoding = "UTF-8"), value = T)
#   message(line)
#   if (length(line) > 0) {
#     content <- gsub(paste0("echo: ", country_to_grep, ";"), "", line)
#     tests_cumulative <- as.numeric(gsub("[, .]", "", gsub(pattern, "\\1", str_extract(content, pattern))))
#   }

#   return(c(new_tests, tests_cumulative))
# }
