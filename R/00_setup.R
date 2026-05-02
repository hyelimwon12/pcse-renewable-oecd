# 00_setup.R
# Run this once on a new machine to install every package the analysis needs.
# Re-running is safe: install.packages() will just upgrade or skip.

required_pkgs <- c(
  "readxl",    # read .xlsx files
  "dplyr",     # data wrangling (mutate, group_by, lag, ...)
  "tidyr",     # reshape helpers
  "plm",       # panel-data econometrics (xtreg fe equivalent)
  "lmtest",    # coeftest() for hypothesis tests
  "sandwich",  # robust / clustered standard errors
  "prais",     # Prais-Winsten regression
  "pcse"       # panel-corrected standard errors
)

missing <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                 logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_pkgs, library, character.only = TRUE))
cat("All packages loaded.\n")
