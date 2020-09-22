# FINDCov19Tracker
Several countries and entities including the World Bank are publishing aggregate estimates on the total number of tests performed.
These reports are published across individual websites, statistical reports and press releases – often in multiple languages and updated with different periodicity.
Although there is currently no centralized database and many countries do not publish official reports on volumes of tests performed, we are working to build a global picture of how many people are being tested for COVID-19.

[-> Map](https://www.finddx.org/covid-19/test-tracker/)

### Data sources 

**Test data** : Collated everyday by the FIND team, from information found online. 
  Usually the official government websites of each country are consulted. 

**Case data** : Downloaded daily from the COVID19 JHU repository: https://github.com/CSSEGISandData/COVID-19

### R Package Workflow Description

We make use of an R package structure to process the downloaded data.
The workflow is automated via GitHub Actions in [dsbbfinddx/FindCov19TrackerData](https://github.com/dsbbfinddx/FindCov19TrackerData).

- `process_jhu_data()`: main function which starts all JHU data processing.
  Calls `download_jhu_data()`, `preprocess_jhu_data()` and `check_jhu_data()`.
  Writes `processed/coronavirus_cases.csv`.

- `fetch_test_data()`: retrieves the cumulative and new test statistics from official resources of the individual countries. It returns a list with the information about "new tests" and "cumulative tests" for each country.

- `process_test_data()`: "test" data processing to align cases and tests data.
  Calls `create_covid_data()`.
  Writes `processed/coronavirus_tests.csv`.
  
- `create_shiny_data()`: makes use of `coronavirus_cases.csv` and `coronavirus_tests.csv`.
  Writes `processed/data_shiny.csv` which is being used by the shiny app.

