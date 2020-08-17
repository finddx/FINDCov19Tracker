#' @title Create Input Data for Shiny Application
#' @description Reads coronavirus_cases.csv (cases) and coronavirus_tests.csv (tests)
#'   files from `processed/` directory.
#'   Writes `processed/data_shiny.csv`.
#' @export
#' @import dplyr tibble
create_shiny_data <- function() {

  # library(tidyverse)

  # read and cobine data -------------------------------------------------------

  cv_cases <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/data/master/processed/coronavirus_cases.csv",
    col_types = readr::cols()
  )

  cv_tests <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/data/master/processed/coronavirus_tests.csv",
    col_types = readr::cols()
  )

  pop <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/data/master/raw/UN_populations_2020.csv",
    col_types = readr::cols()
  ) %>%
    mutate(country = recode(.data$country,
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
    as_tibble() %>%
    select(
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

  data_combined_raw <-
    cv_cases %>%
    left_join(cv_tests, by = c("country" = "jhu_ID", "date" = "date")) %>%
    left_join(pop) %>%
    mutate(pop_100k = population / 100000) %>%
    select(
      name = country,
      date,
      cases,
      new_cases,
      deaths,
      new_deaths,
      tests = tests_cumulative,
      new_tests,
      pop_100k
    ) %>%
    fuzzyjoin::regex_left_join(iso_country, by = c("name" = "regex"), ignore_case = TRUE) %>%
    mutate(country = case_when(
      name == "Kosovo" ~ "XK",
      name == "SouthAfrica" ~ "ZA",
      name == "CentralAfricanRepublic" ~ "CF",
      name == "DominicanRepublic" ~ "DO",
      name == "SaintLucia" ~ "LC",
      name == "WesternSahara" ~ "EH",
      name == "UnitedRepublicofTanzania" ~ "TZ",
      name == "RepublicofKorea" ~ "KR",
      TRUE ~ country
    )) %>%
    # drop ships
    filter(!(name %in% c("DiamondPrincessCruiseShip", "MSZaandam"))) %>%
    # 1 Diamond Princess Cruise Ship
    # 2 MS Zaandam
    select(-regex) %>%
    select(name, country, everything())

  # filter data which no iso_code match
  data_combined <- data_combined_raw %>%
    filter(!is.na(country))

  cli::cli_alert_danger("The following countries lack an iso code and are dropped:
                        {setdiff(data_combined_raw$name, data_combined$name)}.")


  # process data for shiny app -------------------------------------------------

  # do we still want this?
  # readr::write_csv(data_combined, "processed/data_combined.csv")

  country_name <-
    countrycode::codelist %>%
    as_tibble() %>%
    select(
      name = country.name.en, country = iso2c
    ) %>%
    dplyr::mutate(country = dplyr::case_when(
      name == "Kosovo" ~ "XK",
      TRUE ~ country
    ))  %>%
    filter(!is.na(country))

  # we could write it, or read it from here
  # readr::write_csv(country_name, "../FINDCov19TrackerData/raw/country_name.csv")

  country_info <-
    readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/data/master/raw/country_info.csv", col_types = readr::cols()) %>%
    select(-name_not_used) %>%
    filter(!is.na(country_iso))

  data_country <-
    data_combined %>%
    select(-name) %>%
    # prefix cummulative vars
    rename(cum_cases = cases, cum_deaths = deaths, cum_tests = tests, time = date) %>%
    # keep orginal data in separate columns
    mutate(across(starts_with("new"), function(e) e, .names = "{col}_orig")) %>%
    # rolling averages of 7 for new vars
    arrange(country, time) %>%
    group_by(country) %>%
    mutate(across(starts_with("new"), function(e) data.table::frollmean(e, 7, na.rm = TRUE))) %>%
    ungroup() %>%
    # per capita
    mutate(
      across(
        c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests),
        function(e) e / pop_100k,
        .names = "cap_{col}"
      ),
      across(
        c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests),
        function(e) e,
        .names = "all_{col}"
      )
    ) %>%
    # positivity rate
    mutate(pos = na_if(all_new_cases / all_new_tests, Inf)) %>%
    add_column(set = "country", .before = 1) %>%
    rename(unit = country)

  # aggregate to regions, income groups

  # if ratios are aggregated, only use observations that have data for nominator and denominator
  sum_ratio <- function(nominator, denominator) {
    denominator[denominator == 0] <- NA
    in_use <- !is.na(nominator) & !is.na(denominator)
    sum(nominator[in_use]) / sum(denominator[in_use])
  }

  sum_basic <- function(x) {
    sum(x, na.rm = TRUE)
  }

  data_region <-
    data_country %>%
    left_join(select(country_info, unit = country_iso, region = continent, income), by = "unit") %>%
    group_by(unit = region, time) %>%
    summarize(
      across(
        c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests),
        function(e) sum_ratio(e, pop_100k),
        .names = "cap_{col}"
      ),
      across(
        c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests),
        function(e) sum_basic(e),
        .names = "all_{col}"
      ),
      pos = sum_ratio(all_new_cases, all_new_tests)
    ) %>%
    ungroup() %>%
    add_column(set = "region", .before = 1)

  data_income <-
    data_country %>%
    left_join(select(country_info, unit = country_iso, region = continent, income), by = "unit") %>%
    group_by(unit = income, time) %>%
    summarize(
      across(
        c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests),
        function(e) sum_ratio(e, pop_100k),
        .names = "cap_{col}"
      ),
      across(
        c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests),
        function(e) sum_basic(e),
        .names = "all_{col}"
      ),
      pos = sum_ratio(all_new_cases, all_new_tests)
    ) %>%
    ungroup() %>%
    add_column(set = "income", .before = 1)

  data_all <-
    bind_rows(data_country, data_region, data_income) %>%
    filter(!is.na(unit)) %>%
    mutate(across(where(is.numeric), function(e) {e[is.na(e)] <- NA; e})) %>%
    select(-c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests)) %>%
    arrange(time, set, unit)

  latest_test_date <-
    data_all %>%
    filter(set == "country") %>%
    select(unit, time, value = new_tests_orig) %>%
    filter(!is.na(value)) %>%
    filter(value > 0) %>%
    arrange(desc(time)) %>%
    group_by(unit) %>%
    summarize(latest_test_date = time[1]) %>%
    ungroup()

  unit_info <-
    data_all %>%
    filter(time == max(data_all$time)) %>%
    select(
      set, unit,
      cases = cap_new_cases,
      deaths = cap_new_deaths,
      pos = pos,
      tests = cap_new_tests
    ) %>%
    left_join(country_info, by = c("unit" = "country_iso")) %>%
    left_join(latest_test_date, by = "unit")

  readr::write_csv(unit_info, "processed/unit_info.csv")
  readr::write_csv(data_all, "processed/data_all.csv")

  writeLines(jsonlite::toJSON(data_all), "processed/data_all.json")


}
