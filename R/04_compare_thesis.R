# 04_compare_thesis.R
# Print a side-by-side comparison of R-replicated coefficients (from
# 03_pcse_regression.R) against the thesis Table E values.
#
# Notes on expected differences:
#  - Thesis used Stata `xtpcse, correlation(psar1)` which applies
#    Prais-Winsten AR(1) transformation BEFORE PCSE. R has no
#    package on CRAN that does this for unbalanced panels, so our
#    coefficients are pooled-OLS magnitudes — typically larger than
#    the AR(1)-filtered Stata coefficients.
#  - `land_km` (R) = `land_area / 1e6`. Thesis reports raw land_area
#    coef — multiply R land_km coef by 1e-6 to compare.

suppressPackageStartupMessages({
  library(dplyr)
  library(lmtest)
})

models <- readRDS("data/processed/pcse_models.rds")

# Thesis Table E — typed in by hand from the document.
thesis_table_e <- tibble::tribble(
  ~variable,             ~lag1,      ~lag2,      ~lag3,
  "Length of FIT",       -0.00973,   -0.00227,    0.00408,
  "Price of FIT",         0.0878,     0.0122,     0.0252,
  "ETS coverage",         0.00643,    0.00378,    0.00364,
  "Dummy FIT",            0.0632,     0.0601,     0.0655,
  "Dummy RPS",            0.0761,     0.101,      0.0978,
  "Dummy ETS",           -0.229,     -0.153,     -0.0825,
  "Carbon Intensity",    -0.177,     -0.152,     -0.145,
  "Interest Rate",       -0.00946,   -0.0103,    -0.0137,
  "Land area (raw)",      4.98e-7,    4.58e-7,    4.61e-7,
  "Logged GDP",           1.795,      1.933,      1.817,
  "Constant",            -7.936,     -9.523,     -8.403
)

# Pull R coefficients into the same shape, keyed by lag suffix.
r_coef <- function(suffix) {
  ct <- models[[suffix]]$ct
  est <- ct[, "Estimate"]
  # Thesis reports land_area on raw scale; we estimated land_km = land/1e6.
  # Convert by multiplying our estimate by 1e-6.
  if ("land_km" %in% names(est)) {
    est["land_km"] <- est["land_km"] * 1e-6
  }
  est
}

r1 <- r_coef("L1"); r2 <- r_coef("L2"); r3 <- r_coef("L3")

map <- c(
  "Length of FIT"    = "CLENGTH",
  "Price of FIT"     = "FIT_USD",
  "ETS coverage"     = "ETS_cover",
  "Dummy FIT"        = "dummy_FIT",
  "Dummy RPS"        = "dummy_RPS",
  "Dummy ETS"        = "dummy_ETS",
  "Carbon Intensity" = "wdi_co2",
  "Interest Rate"    = "InterestRate",
  "Land area (raw)"  = "land_km",     # already rescaled to raw above
  "Logged GDP"       = "lngdp",
  "Constant"         = "(Intercept)"
)

get_r <- function(varname, lag, est_vec) {
  rn <- map[[varname]]
  if (rn == "(Intercept)") return(unname(est_vec[rn]))
  if (rn %in% c("wdi_co2", "InterestRate", "land_km", "lngdp")) {
    return(unname(est_vec[rn]))
  }
  unname(est_vec[paste0(rn, "_L", lag)])
}

cmp <- thesis_table_e %>%
  rowwise() %>%
  mutate(
    R_lag1 = get_r(variable, 1, r1),
    R_lag2 = get_r(variable, 2, r2),
    R_lag3 = get_r(variable, 3, r3)
  ) %>%
  ungroup() %>%
  select(variable,
         thesis_L1 = lag1, R_L1 = R_lag1,
         thesis_L2 = lag2, R_L2 = R_lag2,
         thesis_L3 = lag3, R_L3 = R_lag3)

cat("\n=== Thesis Table E vs R replication ===\n")
cat("(R uses pooled OLS + Beck-Katz PCSE; AR(1) correction not applied)\n\n")
print(cmp, n = 20, width = Inf)

# Show N obs match
cat("\nN obs (thesis : R):\n")
cat(" 1-yr lag   595 :", models$L1$n_obs, "\n")
cat(" 2-yr lag   567 :", models$L2$n_obs, "\n")
cat(" 3-yr lag   539 :", models$L3$n_obs, "\n")
cat("\nN groups (thesis : R):\n")
cat(" all models  30 :", models$L3$n_groups, "\n")

# Summarize sign agreement
cat("\nSign agreement by lag (thesis vs R, ignoring intercept):\n")
sign_agree <- function(t, r) sum(sign(t) == sign(r), na.rm = TRUE)
n_vars <- nrow(thesis_table_e) - 1  # drop intercept from count
keep <- thesis_table_e$variable != "Constant"
cat(" L1:", sign_agree(thesis_table_e$lag1[keep], cmp$R_L1[keep]), "/", n_vars, "\n")
cat(" L2:", sign_agree(thesis_table_e$lag2[keep], cmp$R_L2[keep]), "/", n_vars, "\n")
cat(" L3:", sign_agree(thesis_table_e$lag3[keep], cmp$R_L3[keep]), "/", n_vars, "\n")
