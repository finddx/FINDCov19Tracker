
### Data sources 

**Test data** : Collated everyday by the FIND team, from information found online. [x] countries are updated automatically running the [x] script found in the main folder. The source test file is then saved in the dropbox folder under the name coronavirus_tests_YYYYMMDD_sources_SO.csv

**Case data** : Downloaded daily from the COVID19 JHU repository: https://github.com/CSSEGISandData/COVID-19

### R Package Workflow Description

We make use of an R package structure to process the downloaded data.
The workflow is automated via GitHub Actions in [dsbbfinddx/data](https://github.com/dsbbfinddx/data).

- `process_jhu_data()`: Main function which starts all JHU data processing.
  Calls `download_jhu_data()`, `preprocess_jhu_data()` and `check_jhu_data()`.
  Writes `processed/coronavirus_cases.csv`.
  
- `process_test_data()`: Main function which starts all "test" data processing.
  Calls `create_covid_data()`.
  Writes `processed/coronavirus_tests.csv`.
  
- `create_shiny_data()`: Makes use of `coronavirus_cases.csv` and `coronavirus_tests.csv`.
  Writes `processed/data_shiny.csv` which is being used by the shiny app.

