# renv

This directory contains the project-local `renv` bootstrap and settings files used for reproducible package management.

Key files:

- `activate.R`: activates the project library when the repository is opened in R.
- `settings.json`: stores project-level `renv` configuration.
- `library/`: project-local package library populated by `renv::restore()`.

Notes:

- Treat this directory as tool-managed.
- Update package state through `renv` commands rather than editing files here manually.