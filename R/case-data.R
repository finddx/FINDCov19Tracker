#' @title Update John Hopkins University data
#' @description Updates the JHU cases data and saves them in coronavirus_cases.csv
#'   file in the processed folder
#'
#' Edward Parker, London School of Hygiene & Tropical Medicine, March 2019.
#' Data extracted from Johns Hopkins data obtained from following Github
#'  repository https://github.com/CSSEGISandData/COVID-19
#' @return Writes `processed/coronavirus_cases.csv` and `processed/jhu_data.csv`.
#' @export
process_jhu_data <- function() {

  # read latest Covid-2019 data: confirmed cases--------------------------------
  jhu_cases_ori <- download_jhu_data("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  # jhu_cases_ori_us <- download_jhu_data("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

  jhu_cases <- preprocess_jhu_data(jhu_cases_ori)
  # jhu_cases_us <- preprocess_jhu_data_us(jhu_cases_ori_us)
  check_jhu_data(jhu_cases_ori, jhu_cases)

  # read latest Covid-2019 data: deaths ----------------------------------------
  jhu_deaths_ori <- download_jhu_data("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  # jhu_deaths_ori_us <- download_jhu_data("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

  jhu_deaths <- preprocess_jhu_data(jhu_deaths_ori)
  # jhu_deaths_us <- preprocess_jhu_data_us(jhu_deaths_ori_us)
  check_jhu_data(jhu_deaths_ori, jhu_deaths)

  # merge dataframes -----------------------------------------------------------

  jhu_merge <- jhu_cases %>%
    dplyr::left_join(jhu_deaths, by = c("country", "date")) %>%
    # dplyr::bind_rows(jhu_cases_us) %>%
    # dplyr::bind_rows(jhu_deaths_us) %>%
    dplyr::rename(cases = count.x, deaths = count.y) %>%
    dplyr::arrange(date)

  # remove Timor Leste as this duplicates East Timor data
  readr::write_csv(jhu_merge, "processed/jhu_data.csv")

  # load country data
  # suppress warning for the first column being unnamed
  countries <- suppressWarnings(readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/data/master/raw/countries_codes_and_coordinates.csv",
    col_types = readr::cols()
  ))

  # check all jhu country names have corresponding country data
  "%ni%" <- Negate("%in%")
  countries_without_coordinates <- unique(as.character(jhu_merge[which(as.character(
    levels(jhu_merge$country)
  ) %ni%
    countries$jhu_ID), "country"]$country))

  if (length(countries_without_coordinates) > 0) {
    cli::cli_alert_info("{.fun process_jhu_data}: Mapping data lacking for the following
          countries: {countries_without_coordinates}.", wrap = TRUE)
  }

  # drop countries without coordinates
  jhu_merge_no_map <- jhu_merge %>%
    dplyr::mutate(country = as.character(country)) %>%
    dplyr::filter(!country %in% countries_without_coordinates)

  jhu_merge_no_map_relative <-
    jhu_merge_no_map %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(new_cases = cases - dplyr::lag(cases)) %>%
    dplyr::mutate(new_deaths = deaths - dplyr::lag(deaths)) %>%
    dplyr::mutate(last_update = Sys.time()) %>%
    dplyr::ungroup()

  readr::write_csv(jhu_merge_no_map_relative, "processed/coronavirus_cases.csv")

  cli::cli_alert_success("{.file processed/coronavirus_cases.csv}: Up to date!")
}

#' @title Preprocess JHU data
#' Processes JHU input data
#' @export
preprocess_jhu_data <- function(input_df) {
  input_df <- input_df %>%
    dplyr::rename(province = "Province/State") %>%
    dplyr::rename(country = "Country/Region")

  input_df_clean <- input_df %>%
    dplyr::mutate(province = dplyr::recode(.data$province,
      Macao = "Macau"
    )) %>%
    dplyr::mutate(country = dplyr::recode(.data$country,
      "Korea, South" = "RepublicofKorea",
      "Congo (Brazzaville)" = "Congo",
      "Congo (Kinshasa)" = "DemocraticRepublicoftheCongo",
      "Cote d'Ivoire" = "CotedIvoire",
      "Cruise Ship" = "DiamondPrincessCruiseShip",
      "Diamond Princess" = "DiamondPrincessCruiseShip",
      "Dominican Republic" = "DominicanRepublic",
      "El Salvador" = "ElSalvador",
      "Equatorial Guinea" = "EquatorialGuinea",
      "Holy See" = "HolySee",
      "MS Zaandam" = "MSZaandam",
      "New Zealand" = "NewZealand",
      "North Macedonia" = "NorthMacedonia",
      "Papua New Guinea" = "PapuaNewGuinea",
      "Saint Kitts and Nevis" = "SaintKittsandNevis",
      "Saint Lucia" = "SaintLucia",
      "Saint Vincent and the Grenadines" = "SaintVincentandtheGrenadines",
      "San Marino" = "SanMarino",
      "Sao Tome and Principe" = "SaoTomeandPrincipe",
      "Saudi Arabia" = "SaudiArabia",
      "Sierra Leone" = "SierraLeone",
      "South Sudan" = "SouthSudan",
      "Sri Lanka" = "SriLanka",
      "Taiwan*" = "Taiwan",
      "Trinidad and Tobago" = "TrinidadandTobago",
      "United Arab Emirates" = "UnitedArabEmirates",
      "United Kingdom" = "UnitedKingdom",
      "Western Sahara" = "WesternSahara",
      "Gambia" = "TheGambia",
      "Antigua and Barbuda" = "AntiguaandBarbuda",
      "Bosnia and Herzegovina" = "BosniaandHerzegovina",
      "Burkina Faso" = "BurkinaFaso",
      "Central African Republic" = "CentralAfricanRepublic",
      "Costa Rica" = "CostaRica",
      "Bahamas" = "TheBahamas",
      "Cabo Verde" = "CapeVerde",
      "Syria" = "SyrianArabRepublic",
      "Timor-Leste" = "EastTimor",
      "South Africa" = "SouthAfrica",
      "East Timor" = "EastTimor",
      "West Bank and Gaza" = "occupiedPalestinianterritory",
      "Guinea-Bissau" = "GuineaBissau",
      "Tanzania, United Republic of" = "UnitedRepublicofTanzania",
      "Moldova, Republic of" = "RepublicofMoldova",
      " " = "NA"
    ))

  input_df_sel <- input_df_clean %>%
    dplyr::select(-province, -Lat, -Long) %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(), sum))

  input_df_sel_tidy <- input_df_sel %>%
    tidyr::pivot_longer(-country, values_to = "count", names_to = "date") %>%
    dplyr::mutate(date = as.Date(date, "%m/%d/%y")) %>%
    dplyr::mutate(count = as.integer(count)) %>%
    dplyr::mutate(country = as.factor(country))

  return(input_df_sel_tidy)
}

# US data is already in confirmed_global.csv
# preprocess_jhu_data_us <- function(input_df) {
#   input_df_proc <- input_df %>%
#     dplyr::rename(country = Country_Region) %>%
#     dplyr::select(tidyselect::matches("/|country")) %>%
#     dplyr::mutate(country = dplyr::recode(.data$country, US = "USA"))
#
#   input_df_long <- input_df_proc %>%
#     tidyr::pivot_longer(-country, values_to = "count", names_to = "date") %>%
#     dplyr::mutate(date = as.Date(date, "%m/%d/%y")) %>%
#     dplyr::mutate(count = as.integer(count)) %>%
#     dplyr::mutate(country = as.factor(country))
#
#   return(input_df_long)
# }

#' Download JHU data
#' @description Download JHU data.
download_jhu_data <- function(url) {
  jhu_cases_ori <- readr::read_csv(url, col_types = readr::cols())
  return(jhu_cases_ori)
}

#' Perform Consistency Checks
#'
#' @description Compares original and preprocessed JHU datasets to ensure
#'   consistency.
check_jhu_data <- function(original, modified) {
  # do some consistency checks
  # get the number of cases from the most recent day
  last_day <- colnames(original[, ncol(original)])
  total_cases_ori <- sum(original[, last_day])

  case_count_tidy <- modified %>%
    dplyr::filter(date == as.Date(last_day, "%m/%d/%y")) %>%
    dplyr::summarise(sum(count)) %>%
    dplyr::pull()
  if (total_cases_ori != case_count_tidy) {
    stop(paste0("Error: incorrect processing - total counts do not match."))
  }
}
