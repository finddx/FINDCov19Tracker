#' @title Create Input Data for Shiny Application
#' @description Reads coronavirus_cases.csv (cases) and coronavirus_tests.csv (tests)
#'   files from `processed/` directory.
#'   Writes `processed/data_shiny.csv`.
#' @export
create_shiny_data <- function() {
  cv_cases <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/data/master/processed/coronavirus_cases.csv",
    col_types = readr::cols()
  )

  cv_tests <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/data/master/processed/coronavirus_tests.csv",
    col_types = readr::cols()
  )

  pop <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/data/master/raw/UN_populations_2020.csv",
    col_types = readr::cols()
  ) %>%
    dplyr::mutate(country = dplyr::recode(.data$country,
      "Republic of Korea" = "RepublicofKorea",
      "Congo (Brazzaville)" = "Congo",
      # "Republic of the Congo" = Congo
      "Democratic Republic of the Congo" = "DemocraticRepublicoftheCongo",
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
      " " = "NA",
      "United States of America" = "US",
      "Bolivia (Plurinational State of)" = "Bolivia",
      "Brunei Darussalam" = "Brunei",
      "Côte d'Ivoire" = "CotedIvoire",
      "Iran (Islamic Republic of)" = "Iran",
      "Lao People's Democratic Republic" = "Laos",
      "Republic of Moldova" = "Moldova",
      "State of Palestine" = "occupiedPalestinianterritory",
      "Russian Federation" = "Russia",
      "Syrian Arab Republic" = "SyrianArabRepublic",
      "China, Taiwan Province of China" = "Taiwan",
      "United Republic of Tanzania" = "Tanzania",
      "Venezuela (Bolivarian Republic of)" = "Venezuela",
      "Viet Nam" = "Vietnam"
    ))

  iso_country <-
    countrycode::codelist %>%
    tibble::as_tibble() %>%
    dplyr::select(
      regex = country.name.en.regex, country = iso2c
    )

  # check all jhu country names have corresponding country data
  "%ni%" <- Negate("%in%")
  countries_without_population <- unique(cv_cases[which(cv_cases$country %ni%
    unique(pop$country)), "country"]$country)

  if (length(countries_without_population) > 0) {
    cli::cli_alert_info("{.fun process_jhu_data}: Population data lacking for the following
          countries: {countries_without_population}.", wrap = TRUE)
  }

  data <- cv_cases %>%
    dplyr::left_join(cv_tests, by = c("country" = "jhu_ID", "date" = "date")) %>%
    dplyr::left_join(pop) %>%
    dplyr::mutate(pop_100k = population / 100000) %>%
    dplyr::select(
      name = country,
      date,
      cases,
      new_cases,
      deaths,
      new_deaths,
      tests = tests_cumulative,
      new_tests,
      pop_100k
    )

  data_shiny <-
    data %>%
    fuzzyjoin::regex_left_join(iso_country, by = c("name" = "regex"), ignore_case = TRUE) %>%
    dplyr::mutate(country_iso = dplyr::case_when(
      name == "Kosovo" ~ "XK",
      name == "SouthAfrica" ~ "ZA",
      name == "CentralAfricanRepublic" ~ "CF",
      name == "DominicanRepublic" ~ "DO",
      name == "SaintLucia" ~ "LC",
      name == "WesternSahara" ~ "EH",
      TRUE ~ country
    )) %>%
    # drop
    # 1 Diamond Princess Cruise Ship
    # 2 MS Zaandam
    dplyr::select(-regex, -country) %>%
    dplyr::rename(name_full = name) %>%
    dplyr::select(name_full, country_iso, dplyr::everything())

  # filter data which no iso_code match
  data_shiny_filtered <- data_shiny %>%
    dplyr::filter(!is.na(country_iso))

  cli::cli_alert_danger("The following countries lack an iso code and are dropped:
                        {setdiff(data_shiny$name, data_shiny_filtered$name)}.")

  readr::write_csv(data_shiny_filtered, "processed/data_shiny.csv")
}
