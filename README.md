# FINDCov19Tracker

- [FINDCov19Tracker](#findcov19tracker)
  - [Data Sources](#data-sources)
  - [Workflow Description](#workflow-description)
    - [COVID-19 Cases](#covid-19-cases)
    - [COVID-19 Tests](#covid-19-tests)
    - [Shiny App](#shiny-app)

Several countries and entities including the World Bank are publishing aggregate estimates on the total number of tests performed.
These reports are published across individual websites, statistical reports and press releases – often in multiple languages and updated with different periodicity.
Although there is currently no centralized database and many countries do not publish official reports on volumes of tests performed, we are working to build a global picture of how many people are being tested for COVID-19.

The FIND team publishes its results in a visual application which can be found at

https://www.finddx.org/covid-19/test-tracker

## Data Sources

**Test data** : Collated everyday by the FIND team, from information found online.
A large portion is automated via Python and R (see below in section [Tests](#tests)).
A minor portion is gathered by manual visits to the respective country websites.
_Usually the official government websites of each country are consulted._

**Case data** : Downloaded daily from the COVID19 [John Hopkins University (JHU) repository](https://github.com/CSSEGISandData/COVID-19).

## Workflow Description

The querying and processing of the cases and test data is done by R package {FINDCovTracker19} found in this repo.

### COVID-19 Cases

- [`FINDCov19Tracker::process_jhu_data()`](https://finddx.github.io/FINDCov19Tracker/reference/process_jhu_data.html): main function which starts all John Hopkins University (JHU) data processing.
  The functions calls [`FINDCov19Tracker::preprocess_jhu_data()`](https://finddx.github.io/FINDCov19Tracker/reference/preprocess_jhu_data.html) and [`FINDCov19Tracker::check_jhu_data()`](https://finddx.github.io/FINDCov19Tracker/reference/check_jhu_data.html) and writes `processed/coronavirus_cases.csv`.

### COVID-19 Tests

Test data scraping is mainly automated and run in a GitHub Actions workflow in the [finddx/FINDCov19TrackerData](https://github.com/finddx/FINDCov19TrackerData) repository.
See the repos [README](https://github.com/finddx/FINDCov19TrackerData) for a high-level and detailed workflow description.

### Shiny App

[`FINDCov19Tracker::create_shiny_data()`](https://finddx.github.io/FINDCov19Tracker/reference/create_shiny_data.html): makes use of [`coronavirus_cases.csv`](https://github.com/finddx/FINDCov19TrackerData/blob/master/processed/coronavirus_cases.csv) and [`coronavirus_tests.csv`](https://github.com/finddx/FINDCov19TrackerData/blob/master/processed/coronavirus_tests.csv).
The function writes [`processed/data_all.csv`](https://github.com/finddx/FINDCov19TrackerData/blob/master/processed/data_all.csv) which is being used as the input for the Shiny app.
