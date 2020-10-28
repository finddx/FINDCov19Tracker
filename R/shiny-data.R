#' @title Create Input Data for Shiny Application
#' @description Reads coronavirus_cases.csv (cases) and coronavirus_tests.csv (tests)
#'   files from `processed/` directory.
#'   Writes `processed/data_shiny.csv`.
#' @export
#' @import dplyr tibble

library(data.table)
create_shiny_data <- function() {

  process_jhu_data()
  process_test_data()

  # country reference data -----------------------------------------------------

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

  # read data ------------------------------------------------------------------

  # regex matching table
  iso_country <-
    countrycode::codelist %>%
    as_tibble() %>%
    select(
      regex = country.name.en.regex, country = iso2c
    )

  # cv_cases_raw <- readr::read_csv("processed/coronavirus_cases.csv",
  #   col_types = readr::cols()
  # )

  cv_cases_raw <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/coronavirus_cases.csv",
                                  col_types = readr::cols()
  )

  # cv_tests_raw <- readr::read_csv("processed/coronavirus_tests.csv",
  #   col_types = readr::cols()
  # )


  cv_tests_raw <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/coronavirus_tests.csv",
                                  col_types = readr::cols()
  )

  pop_raw <- readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/raw/UN_populations_2020.csv",
    col_types = readr::cols()
  )

  country_info <-
    readr::read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/raw/country_info.csv", col_types = readr::cols()) %>%
    select(-name_not_used) %>%
    filter(!is.na(country_iso))

  # use clean identifier (iso2c) -----------------------------------------------

  cv_cases <-
    cv_cases_raw %>%
    rename(name = country) %>%
    fuzzyjoin::regex_left_join(iso_country, by = c("name" = "regex"), ignore_case = TRUE) %>%
    mutate(country = case_when(
      name == "Kosovo" ~ "XK",
      name == "SouthAfrica" ~ "ZA",
      name == "CentralAfricanRepublic" ~ "CF",
      name == "DominicanRepublic" ~ "DO",
      name == "SaintLucia" ~ "LC",
      name == "WesternSahara" ~ "EH",
      # name == "UnitedRepublicofTanzania" ~ "TZ",
      name == "RepublicofKorea" ~ "KR",
      # name == "LaoPeople'sDemocraticRepublic" ~ "LA",
      TRUE ~ country
    )) %>%
    # drop ships
    filter(!(name %in% c("DiamondPrincessCruiseShip", "MSZaandam"))) %>%
    select(-regex) %>%
    relocate(country) %>%
    # drop negative cases and deaths
    mutate(across(c(cases, deaths, new_cases, new_deaths), function(e) if_else(e < 0, NA_real_, e)))

  um <- unique(filter(cv_cases, is.na(country))$name)
  if (length(um) > 0) cli::cli_alert_warning("Unmatched countries in 'cv_cases': {um}")

  cv_tests <-
    cv_tests_raw %>%
    rename(name = country) %>%
    fuzzyjoin::regex_left_join(iso_country, by = c("name" = "regex"), ignore_case = TRUE) %>%
    mutate(country = case_when(
      name == "Kosovo" ~ "XK",
      TRUE ~ country
    )) %>%
    # drop non countries
    filter(!(name %in% c("Scotland"))) %>%
    select(-regex) %>%
    relocate(country) %>%
    # drop 0 or negative testing values
    mutate(across(c(new_tests, tests_cumulative), function(e) if_else(e <= 0, NA_real_, e)))

  um <- unique(filter(cv_tests, is.na(country))$name)
  if (length(um) > 0) cli::cli_alert_warning("Unmatched countries in 'cv_tests': {um}")

  pop <-
    pop_raw %>%
    rename(name = country) %>%
    fuzzyjoin::regex_left_join(iso_country, by = c("name" = "regex"), ignore_case = TRUE) %>%
    mutate(country = case_when(
      name == "Kosovo" ~ "XK",
      TRUE ~ country
    )) %>%
    # drop non countries
    filter(!(name %in% c("Channel Islands"))) %>%
    select(-regex) %>%
    relocate(country)

  um <- unique(filter(pop, is.na(country))$name)
  if (length(um) > 0) cli::cli_alert_warning("Unmatched countries in 'pop': {um}")

  # check all jhu country names have corresponding country data
  "%ni%" <- Negate("%in%")
  countries_without_population <- unique(cv_cases[which(cv_cases$country %ni%
    unique(pop$country)), "country"]$country)

  if (length(countries_without_population) > 0) {
    cli::cli_alert_info("{.fun process_jhu_data}: Population data lacking for the following
          countries: {countries_without_population}.", wrap = TRUE)
  }

  # combining data -------------------------------------------------------------

  data_combined <-
    select(cv_cases, -name) %>%
    full_join(select(cv_tests, -name), by = c("country", "date")) %>%
    left_join(pop, by = "country") %>%
    mutate(pop_100k = population / 100000) %>%
    select(
      country,
      date,
      cases,
      new_cases,
      deaths,
      new_deaths,
      tests = tests_cumulative,
      new_tests,
      pop_100k
    )

  um <- unique(filter(cv_tests, is.na(country))$name)
  if (length(um) > 0) cli::cli_alert_danger("Some missing countries in 'data_combined'")


  # calculations ---------------------------------------------------------------

  # x <- c(2, 2, 2, 2, 2, 2, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2, 2, 2)
  # robust_rollmean(x)

  smooth_new_tests <- function(x, y){
    # rle of NAs
    m <- rle(is.na(x))
    # if there is an NA add the number of times it shows up, if there is a value add 0
    no_of_NAs <- rep(ifelse(m$values,m$lengths,0),times = m$lengths)

    #create a data table with variable, number of NAs and keep only the entries for values, with their original index in the data.frame
    dat <- data.table(x, y, no_of_NAs) %>%
      mutate(ind = as.numeric(rownames(.)))%>%
      filter(no_of_NAs == 0)
    # if there are value in the data.table for the variable
    if(nrow(dat) > 0){

      dat_NA <- data.frame(index = 1:length(x), new_tests_smooth = NA)
      dat_ <- lapply(1:nrow(dat), function(i){
        # for the first entry of dat, check if the original data frame has a value not in the first row, create a df with NA values up to the first value
        if (i == 1 & dat[i, ind] > 1){
          ind_ <- dat[i, ind]
          rbind(data.frame(index = 1:(ind_ - 1), new_tests_smooth = NA),
                data.frame(index = ind_, new_tests_smooth = dat[i, x]))
          # for the first entry of dat, check if the original data frame has a value in the first row
        }else if (i == 1 & dat[i, ind] == 1) {
          ind_ <- dat[i, ind]
          data.frame(index = ind_, new_tests_smooth = dat[i, x])
        }else{
          # for the second entry and later check if there are values and if they come up in gaps or consecutively and
          #create the inbetween values using the diff betweeb the cumulative values reported
          ind_1 <- dat[i - 1, ind]
          ind_2 <- dat[i, ind]
          diff_ind <- ind_2 - ind_1
          if(diff_ind > 1){
            cum_test <- dat[i - 1, y] + round(c(dat[i, x] * c(1:diff_ind)/diff_ind))
            smooth_test <- c(cum_test[1] - dat[i - 1, y], diff(cum_test))
            data.frame(index = (ind_1 + 1):ind_2, new_tests_smooth = smooth_test)
          }else{
            smooth_test <- dat[i, x]
            data.frame(index = ind_2, new_tests_smooth = smooth_test)
          }
        }
      })

      dat_ <- rbindlist(dat_) %>%
        full_join(dat_NA, by = 'index') %>%
        select(index, new_tests_smooth.x) %>%
        rename(new_tests_smooth = new_tests_smooth.x)

    }else{
      dat_ <- data.frame(index = 1:length(x), new_tests_smooth = NA)
    }


    return(dat_$new_tests_smooth)

}



  robust_rollmean <- function(x) {
    ans <- data.table::frollmean(x, 7, na.rm = TRUE)
    no_of_obs <- data.table::frollsum(!is.na(x), 7, na.rm = T, fill = 0)
    ans[no_of_obs <= 3] <- NA
    ans
  }


  data_country <-
    data_combined %>%
    # prefix cummulative vars
    rename(cum_cases = cases, cum_deaths = deaths, cum_tests_orig = tests, time = date) %>%
    # keep original data in separate columns
    mutate(across(starts_with("new"), function(e) e, .names = "{col}_orig")) %>%
    # rolling averages of 7 for new vars
    arrange(country, time) %>%
    group_by(country) %>%
    mutate(new_tests = smooth_new_tests(new_tests, cum_tests_orig)) %>%
    mutate(cum_tests = cumsum(coalesce(new_tests, 0))) %>%
    mutate(across(c(new_cases, new_deaths, new_tests), robust_rollmean)) %>%
    mutate(across(c(new_cases, new_deaths, new_tests), round)) %>%
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


  date_in_table <- max(data_all$time) - 1

  unit_info <-
    data_all %>%
    filter(time == !! date_in_table) %>%
    select(
      set, unit,
      cases = cap_new_cases,
      deaths = cap_new_deaths,
      pos = pos,
      tests = cap_new_tests
    ) %>%
    left_join(country_info, by = c("unit" = "country_iso")) %>%
    left_join(latest_test_date, by = "unit")


  # writing data ---------------------------------------------------------------

  readr::write_csv(unit_info, "processed/unit_info.csv")
  readr::write_csv(data_all, "processed/data_all.csv")

  # readr::write_csv(unit_info, "/Users/Anna/FIND_Onedrive/OneDrive - Foundation for Innovative New Diagnostics FIND/BB_Projects/Shinyapps_projects/FINDCov19TrackerData/processed/unit_info.csv")
  # readr::write_csv(data_all, "/Users/Anna/FIND_Onedrive/OneDrive - Foundation for Innovative New Diagnostics FIND/BB_Projects/Shinyapps_projects/FINDCov19TrackerData/processed/data_all.csv")

  #jsonlite::stream_out(data_all, file("processed/data_all.json"))

}
