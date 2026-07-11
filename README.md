# Mimic Analysis Workflow

This repository contains an R-based analysis workflow for preparing the MMICS dataset, generating descriptive tables, exploring missingness, and fitting ICU admission models with and without multiple imputation.

## Project Structure

- `data/original_data/`: source data files used by the analysis scripts.
- `output/tables/`: exported summary tables.
- `output/figures/imputation/`: missingness and imputation figures.
- `src/`: analysis scripts and helper functions.
- `renv/`, `renv.lock`: project-local package management.

## Requirements

- R 4.5.x
- The source workbook `data/original_data/MMICS data 13-05-26 (for analysis).xlsx`
- Internet access the first time packages are restored with `renv`

## Package Management

This project uses `renv` to lock package versions.

To restore the project library on a new machine:

```r
renv::restore()
```

To check whether the project library and lockfile are synchronized:

```r
renv::status()
```

If you add or update packages intentionally, refresh the lockfile with:

```r
renv::snapshot()
```

## Analysis Workflow

The scripts are written to be run from the repository root.

1. `src/1_data_cleaning.r`
   Loads the Excel workbook, coerces selected variables to numeric, derives admission and outcome fields, and creates `model_data` for downstream modelling.
2. `src/2_tables.r`
   Builds descriptive tables stratified by ICU admission and death, then writes `output/tables/demographics_tables.xlsx`.
3. `src/3_missingness.r`
   Explores missing data patterns and fits an imputation model, writing a figure to `output/figures/imputation/missing_data_pattern.png`.
4. `src/4_univariable_models.r`
   Runs univariable logistic regression for ICU admission and identifies candidate predictors for multivariable modelling.
5. `src/5_mi_models.r`
   Fits multiple-imputation logistic models across predefined predictor groups and evaluates pooled model performance.

Shared helper logic lives in `src/0_functions.r`, including `mark_mi_selection()`, which wraps `psfmi` model selection and pooled performance evaluation.

## Running the Scripts

From an R session opened in the repository root, run scripts in sequence as needed:

```r
source("src/1_data_cleaning.r")
source("src/2_tables.r")
source("src/3_missingness.r")
source("src/4_univariable_models.r")
source("src/5_mi_models.r")
```

You can also run individual scripts from the shell, for example:

```sh
Rscript src/2_tables.r
```

## Outputs

- `output/tables/demographics_tables.xlsx`: descriptive summary tables.
- `output/figures/imputation/missing_data_pattern.png`: missing data visualization.
- In-memory R objects created by the modelling scripts, including pooled model objects and performance summaries.

## Notes

- The scripts assume relative paths from the repository root.
- Some modelling objects are assigned into the global environment by design.
- The repository currently contains data-specific assumptions tied to the MMICS workbook schema.