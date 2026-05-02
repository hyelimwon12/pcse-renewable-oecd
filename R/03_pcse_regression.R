# 03_pcse_regression.R
# Replicates Table E of the thesis: PCSE regression of lnrenewable on
# policy + control variables, run separately at 1-, 2-, 3-year policy lags.
# Sample: 30 OECD countries (Mexico + 7 others excluded in 02_build_variables.R).
#
# CAVEAT: Stata's `xtpcse , correlation(psar1)` first applies a
# panel-specific Prais-Winsten AR(1) transformation, *then* computes
# panel-corrected SEs. R has no current CRAN package that does both for
# unbalanced panels (panelAR is archived). Here we run pooled OLS and
# apply Beck-Katz PCSE only — coefficients are pooled-OLS, SEs are
# panel-corrected. We expect coefficients to be in the same ballpark as
# Table E but not identical, because the AR(1) layer is missing.

suppressPackageStartupMessages({
  library(dplyr)
  library(plm)
  library(lmtest)
})

panel <- readRDS("data/processed/panel_lagged.rds") %>%
  arrange(ccode, year)

pdat <- pdata.frame(panel, index = c("ccode", "year"))

# Helper: fit pooled OLS at a given lag suffix (L1/L2/L3) and return
# a coeftest with Beck-Katz panel-corrected SEs.
fit_lag <- function(suffix) {
  rhs <- paste0(c("CLENGTH", "FIT_USD", "ETS_cover",
                  "dummy_FIT", "dummy_RPS", "dummy_ETS"), "_", suffix)
  # land_km = land_area / 1e6, used for numerical stability. Coefficient
  # is therefore in (lnrenewable per million km^2) instead of (per km^2).
  rhs_full <- c(rhs, "wdi_co2", "InterestRate", "land_km", "lngdp")
  f <- as.formula(paste("lnrenewable ~", paste(rhs_full, collapse = " + ")))

  fit <- plm(f, data = pdat, model = "pooling")
  vc  <- vcovBK(fit, type = "HC1", cluster = "time")
  list(fit = fit, vcov = vc, ct = coeftest(fit, vc),
       n_obs = nobs(fit),
       n_groups = length(unique(index(fit)[, 1])))
}

m1 <- fit_lag("L1")
m2 <- fit_lag("L2")
m3 <- fit_lag("L3")

print_block <- function(m, label) {
  cat("\n========== ", label, " ==========\n", sep = "")
  cat("N obs: ", m$n_obs, "  | N groups: ", m$n_groups, "\n\n", sep = "")
  print(m$ct)
}

print_block(m1, "PCSE — One-year lag")
print_block(m2, "PCSE — Two-year lag")
print_block(m3, "PCSE — Three-year lag")

saveRDS(list(L1 = m1, L2 = m2, L3 = m3),
        "data/processed/pcse_models.rds")
cat("\nModels saved: data/processed/pcse_models.rds\n")
