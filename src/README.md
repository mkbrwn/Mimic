# src

This directory contains the R analysis scripts and shared helper functions.

Key files:

- `0_functions.r`: shared helper functions for model selection and pooled performance.
- `1_data_cleaning.r`: imports and cleans source data, then constructs `model_data`.
- `2_tables.r`: creates descriptive summary tables.
- `3_missingness.r`: explores missingness and fits the imputation workflow.
- `4_univariable_models.r`: runs univariable screening for ICU admission models.
- `5_mi_models.r`: fits multiple-imputation models and pooled performance summaries.

Notes:

- Scripts assume they are run from the repository root using relative paths.
- The typical workflow starts with `1_data_cleaning.r` and progresses numerically.