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

fetch_from_csv_xlsx <- function(dots) {

  tests_cumulative <- NA
  new_tests <- NA
  proc_backlog <- ifelse(is.na(dots$backlog), 0, as.numeric(dots$backlog))

  if (dots$type == "xlsx") {

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

  } else if (dots$type == "csv") {
    if (dots$country == "USA") {
    }
    if (!is.na(dots$date_format)) { # for now only Costa Rica, updated day before
      yesterday_char <- as.character(Sys.Date() - 1, dots$date_format)
      dots$data_url <- gsub("DATE", yesterday_char, dots$data_url)
    }
    #cli::cli_alert_info("URL: {dots$data_url}.")
    # seps <- c(stringr::str_extract(dots$xpath_cumul, "[,;]"), stringr::str_extract(dots$xpath_new, "[,;]"))
    # sep <- seps[which(!is.na(seps))]
    # data <- readr::read_csv2(file = dots$data_url)
    # data = readr::read_table(dots$data_url)
    data <- rio::import(dots$data_url, format = "csv")
    # remove missing data
    # this removes rows which only have NA or "" in all columns
    data <- data %>%
      janitor::remove_empty(which = c("rows", "cols"))
    # data = data[rowSums(is.na(data)|data=='') != ncol(data),]
    # data = data[complete.cases(data), ]

  } else {
    return(c(new_tests, tests_cumulative))
  }

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
    data = data %>%
      dplyr::select_if(!(names(.) %in% c('totalTestResultsIncrease')))

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

  return(c(new_tests, tests_cumulative))
}

# fetch_from_json <- function(url, cumul, new) {
#   message(url)
#   tests_cumulative <- NA
#   new_tests <- NA

#   getData <- GET(url)
#   getData_text <- content(getData, "text")
#   getData_json <- fromJSON(getData_text, flatten = TRUE)

#   if (!is.na(cumul)) {
#     tests_cumulative <- as.numeric(eval(parse(text = cumul)))
#   }
#   if (!is.na(new)) {
#     new_tests <- as.numeric(eval(parse(text = new)))
#   }
#   return(c(new_tests, tests_cumulative))
# }


# is.error <- function(x) inherits(x, "try-error")

# fetch_from_html <- function(url, cumul, new) {
#   message(url)
#   tests_cumulative <- NA
#   new_tests <- NA

#   page <- try(read_html(url), silent = TRUE)
#   if (is.error(page)) {
#     page <- try(read_html(url(url)), silent = TRUE)
#     if (is.error(page)) {
#       return(c(new_tests, tests_cumulative))
#     }
#   }
#   if (!is.na(cumul)) {
#     text <- page %>%
#       html_node(xpath = cumul) %>%
#       html_text()
#     tests_cumulative <- as.numeric(gsub("[^0-9]", "", text))
#   }
#   if (!is.na(new)) {
#     text <- page %>%
#       html_node(xpath = new) %>%
#       html_text()
#     new_tests <- as.numeric(gsub("[^0-9]", "", text))
#   }
#   return(c(new_tests, tests_cumulative))
# }

# fetch_from_pdf <- function(country, url, date_format, pattern) {
#   message(url)
#   tests_cumulative <- NA
#   new_tests <- NA

#   tmpfile <- tempfile("country_data", fileext = ".pdf")

#   if (is.na(date_format)) {
#     tryCatch(
#       {
#         download.file(url,
#           destfile = tmpfile, quiet = FALSE, mode =
#             "wb"
#         )
#       },
#       silent = FALSE,
#       condition = function(err) { }
#     )
#     if (file.size(tmpfile) == 0) {
#       return(c(new_tests, tests_cumulative))
#     }

#   } else {
#     today_char <- ifelse(grepl("%B", date_format) & country == "Afghanistan", str_to_lower(as.character(Sys.Date(), date_format)),
#       as.character(Sys.Date(), date_format)
#     )
#     yesterday_char <- ifelse(grepl("%B", date_format) & country == "Afghanistan", str_to_lower(as.character(Sys.Date() - 1, date_format)),
#       as.character(Sys.Date() - 1, date_format)
#     )

#     tryCatch(
#       {
#         download.file(gsub("DATE", today_char, url),
#           destfile = tmpfile, quiet = FALSE, mode =
#             "wb"
#         )
#       },
#       silent = FALSE,
#       condition = function(err) { }
#     )
#     if (file.size(tmpfile) == 0) {
#       tryCatch(
#         {
#           download.file(gsub("DATE", yesterday_char, url),
#             destfile = tmpfile, quiet = FALSE, mode =
#               "wb"
#           )
#         },
#         silent = FALSE,
#         condition = function(err) { }
#       )
#       if (file.size(tmpfile) == 0) {
#         return(c(new_tests, tests_cumulative))
#       }
#     }
#   }
#   content <- pdf_text(tmpfile)
#   tests_cumulative <- as.numeric(gsub("[, .]", "", unique(gsub(pattern, "\\1", na.omit(str_extract(content, pattern))))))

#   return(c(new_tests, tests_cumulative))
# }

# fetch_from_pdf_list <- function(url, pattern_url, pattern_content) {
#   message(url)
#   tests_cumulative <- NA
#   new_tests <- NA

#   page <- read_html(url)
#   hrefs <- html_attr(html_nodes(page, "a"), "href")

#   pdfs <- grep(pattern_url, hrefs, ignore.case = T, value = T)

#   pdf <- pdfs[1]

#   content <- pdf_text(pdf)
#   tests_cumulative <- as.numeric(gsub("[, .]", "", unique(gsub(pattern_content, "\\1", na.omit(str_extract(content, pattern_content))))))

#   return(c(new_tests, tests_cumulative))
# }

# fetch_from_html_list <- function(url_list, pattern_url, pattern_content) {
#   message(url_list)
#   tests_cumulative <- NA
#   new_tests <- NA

#   page <- read_html(url_list)
#   hrefs <- html_attr(html_nodes(page, "a"), "href")

#   urls <- grep(pattern_url, hrefs, ignore.case = T, value = T)

#   url <- urls[1]

#   content <- read_html(url) %>% html_text()
#   tests_cumulative <- as.numeric(gsub("[, .]", "", unique(gsub(pattern_content, "\\1", na.omit(str_extract(content, pattern_content))))))

#   return(c(new_tests, tests_cumulative))
# }

# fetch_from_html2 <- function(url, date_format, pattern) {
#   message(url)
#   tests_cumulative <- NA
#   new_tests <- NA

#   if (!is.na(date_format)) {
#     today_char <- as.character(Sys.Date(), date_format)
#     yesterday_char <- as.character(Sys.Date() - 1, date_format)

#     page <- try(read_html(gsub("DATE", today_char, url)), silent = TRUE)
#     if (is.error(page)) {
#       page <- try(read_html(gsub("DATE", yesterday_char, url)), silent = TRUE)
#       if (is.error(page)) {
#         return(c(new_tests, tests_cumulative))
#       }
#     }

#   } else {
#     page <- try(read_html(url), silent = TRUE)
#     if (is.error(page)) {
#       page <- try(read_html(url(url)), silent = TRUE)
#       if (is.error(page)) {
#         return(c(new_tests, tests_cumulative))
#       }
#     }
#   }
#   content <- page %>% html_text()
#   tests_cumulative <- as.numeric(gsub("[, .]", "", unique(gsub(pattern, "\\1", na.omit(str_extract(content, pattern))))))

#   return(c(new_tests, tests_cumulative))
# }

# fetch_from_zip <- function(url, cumul) {
#   message(url)
#   tests_cumulative <- NA
#   new_tests <- NA

#   tmpfile <- tempfile("country_data", fileext = ".zip")
#   tryCatch(
#     {
#       download.file(url,
#         destfile = tmpfile, quiet = FALSE, mode =
#           "wb"
#       )
#     },
#     silent = FALSE,
#     condition = function(err) { }
#   )

#   if (file.size(tmpfile) > 0) {
#     file <- unzip(tmpfile)
#     data <- read.csv(file)
#     unlink(file)

#     if (cumul == "nrow") {
#       tests_cumulative <- nrow(data)
#     }
#   }

#   return(c(new_tests, tests_cumulative))
# }

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
