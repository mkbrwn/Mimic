# original_data

This directory stores the raw source data used by the R analysis workflow.

Current expected file:

- `MMICS data 13-05-26 (for analysis).xlsx`

Notes:

- Scripts such as `src/1_data_cleaning.r` read from this location using relative paths.
- Treat files here as source inputs and avoid editing them in place.
- Data files in this directory are ignored by Git.