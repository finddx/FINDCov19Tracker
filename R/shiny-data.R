#' @title Create Input Data for Shiny Application
#' @description Reads coronavirus_cases.csv (cases) and coronavirus_tests.csv (tests)
#'   files from `processed/` directory.
#'   Writes `processed/data_shiny.csv`.
#' @export
#' @import dplyr tibble
create_shiny_data <- function() {

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
      TRUE ~ country
    )) %>%
    # drop
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
    filter(!is.na(country))

  # bring data to long structure
  country <-
    data_combined %>%
    select(-name) %>%
    rename(
      cum_cases = cases,
      cum_deaths = deaths,
      cum_tests = tests,
      pop = pop_100k,
      time = date
    ) %>%
    select(country, time, pop, everything()) %>%
    mutate(
      cum_pos = 100 * cum_cases / cum_tests,
      new_pos = 100 * new_cases / new_tests
    ) %>%
    tidyr::pivot_longer(
      cum_cases:new_pos,
      names_to = c("diff", "var"),
      names_pattern = "(.+)_(.+)",
      values_to = c("value_all")
    )

  region <-
    country %>%
    left_join(select(country_info, country, region), by = "country") %>%
    group_by(time, unit = region, diff, var) %>%
    summarize_at(vars(pop, value_all), sum, na.rm = TRUE) %>%
    ungroup() %>%
    add_column(set = "region", .before = 1)

  income <-
    country %>%
    left_join(select(country_info, country, income), by = "country") %>%
    group_by(time, unit = income, diff, var) %>%
    summarize_at(vars(pop, value_all), sum, na.rm = TRUE) %>%
    ungroup() %>%
    add_column(set = "income", .before = 1)

  all <-
    country %>%
    rename(unit = country) %>%
    add_column(set = "country", .before = 1) %>%

    # add_column(set = "country", .before = 1) %>%
    bind_rows(region, income) %>%
    # posivity rate is the same if displyed per capita
    mutate(value_cap = if_else(var == "pos", value_all, value_all / pop)) %>%
    select(-pop) %>%
    tidyr::pivot_longer(
      value_all:value_cap,
      names_to = "ref",
      names_pattern = "value_(.+)",
      values_to = c("value")
    ) %>%
    filter(!is.na(value)) %>%
    filter(!is.na(unit))


  # 7 day moving averages
  shiny_data <-
    all %>%
    filter(diff == "new") %>%
    group_by(unit, var, ref) %>%
    mutate(value = data.table::frollmean(value, 7, na.rm = T)) %>%
    ungroup() %>%
    bind_rows(filter(all, diff == "cum")) %>%
    filter(!is.na(value))

  all_latest_wide <-
    shiny_data %>%
    filter(diff == "new") %>%
    filter(ref == "cap") %>%
    arrange(unit, var, time) %>%
    group_by(set, unit, diff, var, ref) %>%
    summarize(value = value[n()]) %>%
    ungroup() %>%
    select(-diff, -ref) %>%
    tidyr::pivot_wider(names_from = "var")

  unit_info <-
    all_latest_wide %>%
    left_join(country_info, by = c("unit" = "country"))

  readr::write_csv(unit_info, "processed/unit_info.csv")
  readr::write_csv(shiny_data, "processed/shiny_data.csv")


}
