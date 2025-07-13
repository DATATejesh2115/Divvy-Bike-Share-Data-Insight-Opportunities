# packages.R

required_packages <- c(
  "tidyverse",
  "data.table",
  "lubridate",
  "readxl",
  "janitor",
  "stringr",
  "dplyr"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Install missing packages
invisible(lapply(required_packages, install_if_missing))

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))
