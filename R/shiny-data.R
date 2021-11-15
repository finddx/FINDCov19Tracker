#' @title Create Input Data for Shiny Application
#' @description Reads coronavirus_cases.csv (cases) and coronavirus_tests.csv
#'   (tests) files from `processed/` directory.
#'   Writes `processed/data_shiny.csv`.
#' @export
#' @import dplyr
#' @importFrom gert git_status
#' @importFrom tibble add_column
create_shiny_data <- function() {

  process_jhu_data()
  process_test_data()

  # in case some countries have negative data, we exit early
  git_mod <- gert::git_status()
  git_mod <- git_mod[git_mod$status == "modified", ]
  if (any("issues/coronavirus_tests_new_negative.csv" %in% git_mod$file)) {
    return(invisible())
  }
  # country reference data -----------------------------------------------------

  country_name <-
    countrycode::codelist %>%
    as_tibble() %>%
    select(
      name = country.name.en, country = iso3c
    ) %>%
    dplyr::mutate(country = dplyr::case_when(
      name == "Kosovo" ~ "XK",
      TRUE ~ country
    )) %>%
    filter(!is.na(country))

  # we could write it, or read it from here
  # readr::write_csv(country_name, "../FINDCov19TrackerData/raw/country_name.csv")

  # read data ------------------------------------------------------------------

  # regex matching table
  iso_country <-
    countrycode::codelist %>%
    as_tibble() %>%
    select(
      regex = country.name.en.regex, country = iso3c
    )

  # cv_cases_raw <- readr::read_csv("processed/coronavirus_cases.csv",
  #   col_types = readr::cols()
  # )

  #cv_cases_raw <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/coronavirus_cases.csv", col_types = readr::cols(), quoted_na = FALSE) # nolint

  cv_cases_raw <- readr::read_csv("processed/coronavirus_cases.csv", col_types = readr::cols(), quoted_na = FALSE)

  #cv_tests_raw <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/coronavirus_tests.csv", col_types = readr::cols(), quoted_na = FALSE) # nolint

  cv_tests_raw <- readr::read_csv("processed/coronavirus_tests.csv", col_types = readr::cols(), quoted_na = FALSE)

  pop_raw <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/raw/UN_populations_2020.csv", col_types = readr::cols(), quoted_na = FALSE) # nolint

  country_info <-
    readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/raw/country_info.csv", col_types = readr::cols(), quoted_na = FALSE) %>% # nolint
    select(-name) %>%
    filter(!is.na(alpha3)) %>%
    mutate(pop = population / 1000)


  # use clean identifier (iso2c) -----------------------------------------------

  cv_cases <-
    cv_cases_raw %>%
    rename(name = country) %>%
    fuzzyjoin::regex_left_join(iso_country, by = c("name" = "regex"),
     ignore_case = TRUE) %>%
    mutate(country = case_when(
      name == "Kosovo" ~ "XKX",
      name == "SouthAfrica" ~ "ZAF",
      name == "CentralAfricanRepublic" ~ "CAF",
      name == "DominicanRepublic" ~ "DOM",
      name == "SaintLucia" ~ "LCA",
      name == "WesternSahara" ~ "ESH",
      # name == "UnitedRepublicofTanzania" ~ "TZ",
      name == "RepublicofKorea" ~ "KOR",
      # name == "LaoPeople'sDemocraticRepublic" ~ "LA",
      TRUE ~ country
    )) %>%
    # drop ships
    filter(!(name %in% c("DiamondPrincessCruiseShip", "MSZaandam"))) %>%
    select(-regex) %>%
    relocate(country) %>%
    # drop negative cases and deaths
    mutate(across(
      c(cases, deaths, new_cases, new_deaths),
      function(e) if_else(e < 0, NA_real_, e)
    ))

  um <- unique(filter(cv_cases, is.na(country))$name)
  if (length(um) > 0) {
    cli::cli_alert_warning("Unmatched countries in 'cv_cases': {um}")
  }

  cv_tests <-
    cv_tests_raw %>%
    rename(name = country) %>%
    fuzzyjoin::regex_left_join(iso_country,
      by = c("name" = "regex"),
      ignore_case = TRUE
    ) %>%
    mutate(country = case_when(
      name == "Kosovo" ~ "XK",
      TRUE ~ country
    )) %>%
    # drop non countries
    filter(!(name %in% c("Scotland"))) %>%
    select(-regex) %>%
    relocate(country) %>%
    # drop 0 or negative testing values
    mutate(across(
      c(new_tests_corrected, tests_cumulative_corrected),
      function(e) if_else(e <= 0, NA_real_, e)
    ))

  um <- unique(filter(cv_tests, is.na(country))$name)
  if (length(um) > 0) {
    cli::cli_alert_warning("Unmatched countries in 'cv_tests': {um}")
  }

  pop <-
    pop_raw %>%
    rename(name = country) %>%
    fuzzyjoin::regex_left_join(iso_country,
      by = c("name" = "regex"), ignore_case = TRUE
    ) %>%
    mutate(country = case_when(
      name == "Kosovo" ~ "XK",
      TRUE ~ country
    )) %>%
    # drop non countries
    filter(!(name %in% c("Channel Islands"))) %>%
    select(-regex) %>%
    relocate(country)

  um <- unique(filter(pop, is.na(country))$name)
  if (length(um) > 0) {
    cli::cli_alert_warning("Unmatched countries in 'pop': {um}")
  }

  # check all jhu country names have corresponding country data
  "%ni%" <- Negate("%in%")
  countries_without_population <- unique(cv_cases[which(cv_cases$country %ni%
    unique(pop$country)), "country"]$country)

  if (length(countries_without_population) > 0) {
    cli::cli_alert_info("{.fun process_jhu_data}: Population data lacking for
      the following countries: {countries_without_population}.", wrap = TRUE)
  }

  # combining data -------------------------------------------------------------

  data_combined <-
    select(cv_cases, -name) %>%
    full_join(select(cv_tests, -name), by = c("country", "date")) %>%
    left_join(pop, by = "country") %>%
    mutate(pop_100k = population / 100000) %>%
    mutate(pop = population / 1000) %>%
    select(
      country,
      date,
      cases,
      new_cases,
      deaths,
      new_deaths,
      tests_orig = tests_cumulative,
      new_tests_orig = new_tests,
      tests = tests_cumulative_corrected,
      new_tests = new_tests_corrected,
      pop_100k,
      pop
    )

  um <- unique(filter(cv_tests, is.na(country))$name)
  if (length(um) > 0) {
    cli::cli_alert_danger("Some missing countries in 'data_combined'")
  }


  # calculations ---------------------------------------------------------------

  data_country <-
    data_combined %>%
    # prefix cummulative vars
    rename(
      cum_cases = cases, cum_deaths = deaths, cum_tests_orig = tests_orig,
      time = date
    ) %>%
    # keep original data in separate columns
    mutate(across(c(new_cases, new_deaths),
      function(e) e,
      .names = "{col}_orig"
    )) %>%
    # rolling averages of 7 for new vars
    arrange(country, time) %>%
    group_by(country) %>%
    mutate(new_tests = smooth_new_tests(new_tests, tests)) %>%
    mutate(cum_tests = cumsum(coalesce(new_tests, 0))) %>%
    mutate(across(c(new_cases, new_deaths, new_tests), robust_rollmean)) %>%
    mutate(across(c(new_cases, new_deaths, new_tests), round)) %>%
    ungroup() %>%
    # per capita
    mutate(
      across(
        c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests),
        ~ .x / pop,
        .names = "cap_{col}"
      ),
      across(
        c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests),
        ~ .x,
        .names = "all_{col}"
      )
    ) %>%
    # positivity rate
    mutate(pos = na_if(all_new_cases / all_new_tests, Inf)) %>%
    tibble::add_column(set = "country", .before = 1) %>%
    rename(unit = country)

  # aggregate to regions, income groups

  # if ratios are aggregated, only use observations that have data for
  # nominator and denominator
  sum_ratio <- function(nominator, denominator) {
    denominator[denominator == 0] <- NA
    in_use <- !is.na(nominator) & !is.na(denominator)
    sum(nominator[in_use]) / sum(denominator[in_use])
  }

  data_region <-
    data_country %>%
    left_join(select(country_info,
      unit = alpha3, region = continent, who_region,
      income
    ), by = "unit") %>%
    group_by(unit = region, time) %>%
    summarize(.groups="keep",
      across(
        c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests),
        ~ sum_ratio(.x, pop),
        .names = "cap_{col}"
      ),
      across(
        c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests),
        ~ sum_basic(.x),
        .names = "all_{col}"
      ),
      pos = sum_ratio(all_new_cases, all_new_tests)
    ) %>%
    ungroup() %>%
    tibble::add_column(set = "region", .before = 1)

  data_who_region <-
    data_country %>%
    left_join(select(country_info,
                     unit = alpha3, region = continent, who_region,
                     income
    ), by = "unit") %>%
    group_by(unit = who_region, time) %>%
    summarize(
      .groups = "keep",
      across(
        c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests),
        ~ sum_ratio(.x, pop),
        .names = "cap_{col}"
      ),
      across(
        c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests),
        ~ sum_basic(.x),
        .names = "all_{col}"
      ),
      pos = sum_ratio(all_new_cases, all_new_tests)
    ) %>%
    ungroup() %>%
    tibble::add_column(set = "who_region", .before = 1)

  data_income <-
    data_country %>%
    left_join(select(country_info,
      unit = alpha3, region = continent, who_region,
      income
    ), by = "unit") %>%
    group_by(unit = income, time) %>%
    summarize(.groups="keep",
      across(
        c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests),
        ~ sum_ratio(.x, pop),
        .names = "cap_{col}"
      ),
      across(
        c(cum_cases, new_cases, cum_deaths, new_deaths, cum_tests, new_tests),
        ~ sum_basic(.x),
        .names = "all_{col}"
      ),
      pos = sum_ratio(all_new_cases, all_new_tests)
    ) %>%
    ungroup() %>%
    tibble::add_column(set = "income", .before = 1)

  data_all <-
    bind_rows(data_country, data_region, data_who_region, data_income) %>%
    filter(!is.na(unit)) %>%
    mutate(across(where(is.numeric), function(e) {
      e[is.na(e)] <- NA
      e
    })) %>%
    select(-c(
      cum_cases, new_cases, cum_deaths, new_deaths, tests,
      cum_tests, new_tests
    )) %>%
    arrange(time, set, unit) %>%
    left_join(country_name, by = c("unit" = "country")) %>%
    relocate(name, .before = unit)


  # summary table --------------------------------------------------------------

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


  # date_in_table <- max(data_all$time) - 1
  date_in_table <- max(data_all$time)

  unit_info <-
    data_all %>%
    filter(time == !!date_in_table) %>%
    select(
      set, unit,
      cases = cap_new_cases,
      deaths = cap_new_deaths,
      pos = pos,
      tests = cap_new_tests
    ) %>%
    left_join(country_info, by = c("unit" = "alpha3")) %>%
    left_join(latest_test_date, by = "unit")


  # writing data ---------------------------------------------------------------

  readr::write_csv(unit_info, "processed/unit_info.csv")
  readr::write_csv(data_all, "processed/data_all.csv")

}

