# FINDCov19Tracker

- [FINDCov19Tracker](#findcov19tracker)
  - [Data sources](#data-sources)
  - [Workflow Description](#workflow-description)
  - [Cases](#cases)
  - [Tests](#tests)

Several countries and entities including the World Bank are publishing aggregate estimates on the total number of tests performed.
These reports are published across individual websites, statistical reports and press releases – often in multiple languages and updated with different periodicity.
Although there is currently no centralized database and many countries do not publish official reports on volumes of tests performed, we are working to build a global picture of how many people are being tested for COVID-19.

The FIND team publishes its results in a visual application which can be found at

https://www.finddx.org/covid-19/test-tracker

## Data sources

**Test data** : Collated everyday by the FIND team, from information found online.
A large portion is automated via Python and R (see below in section [Tests](#tests)).
A minor portion is gathered by manual visits to the respective country websites.
_Usually the official government websites of each country are consulted._

**Case data** : Downloaded daily from the COVID19 [John Hopkins University (JHU) repository](https://github.com/CSSEGISandData/COVID-19).

## Workflow Description

The processing is done by R package {FINDCovTracker19} found in this repo.
A GitHub Actions workflow in [dsbbfinddx/FindCov19TrackerData](https://github.com/dsbbfinddx/FindCov19TrackerData) executes the processing.

## Cases

- [`FINDCov19Tracker::process_jhu_data()`](https://dsbbfinddx.github.io/FINDCov19Tracker/reference/process_jhu_data.html): main function which starts all John Hopkins University (JHU) data processing.
  Calls [`FINDCov19Tracker::preprocess_jhu_data()`](https://dsbbfinddx.github.io/FINDCov19Tracker/reference/preprocess_jhu_data.html) and [`FINDCov19Tracker::check_jhu_data()`](https://dsbbfinddx.github.io/FINDCov19Tracker/reference/check_jhu_data.html).
  Writes `processed/coronavirus_cases.csv`.

## Tests

Tests are mainly retrieved via an automated Selenium-based approach, run through CI via Python.
Countries which cannot be retrieved via Selenium (e.g. because they provide their data as PDFs) are covered by R-based "fetch-functions".
Even though the main language used here is R, Selenium does not support R - hence Python was chosen to automate the Selenium approach.

1. [`FINDCov19Tracker::fetch_test_data()`](https://dsbbfinddx.github.io/FINDCov19Tracker/reference/fetch_test_data.html): retrieves the cumulative and new test statistics from official resources of the individual countries.
   Returns a `data.frame` with the information about "new tests" and "cumulative tests" for each country.

1. [`run.py`](https://github.com/dsbbfinddx/FINDCov19TrackerData/blob/selenium/selenium/run.py) and [`test.py`](https://github.com/dsbbfinddx/FINDCov19TrackerData/blob/selenium/selenium/test.py) execute the Selenium processing.

1. [`FINDCov19Tracker::get_daily_test_data()`](https://dsbbfinddx.github.io/FINDCov19Tracker/reference/get_daily_test_data.html) combines the test data from the Selenium and R approaches and writes a combined JSON file to FIXME containing all test

1. [`FINDCov19Tracker::process_test_data()`](https://dsbbfinddx.github.io/FINDCov19Tracker/reference/process_test_data.html): aligns cases and tests data.
   Writes `processed/coronavirus_tests.csv`.

1. [`FINDCov19Tracker::create_shiny_data()`](https://dsbbfinddx.github.io/FINDCov19Tracker/reference/create_shiny_data.html): makes use of `coronavirus_cases.csv` and `coronavirus_tests.csv`.
   Writes `processed/data_shiny.csv` which is being used by the shiny app.
