# 02_build_variables.R
# Build the derived variables used in the thesis (Table E):
#   *_L1, *_L2, *_L3 : 1/2/3-year lags of policy variables
#   lngdp            : log(gdp_oecd)
#   land_km          : land_area / 1e6 (rescale for numerical stability)
#
# Sample: drop the 8 OECD countries excluded in the thesis (less than 10 yrs
# of data). Final panel = 30 countries.

library(dplyr)

panel <- readRDS("data/processed/panel.rds")

excluded <- c(
  "Czechia",                                       # Czech Republic in thesis
  "Colombia",
  "Costa Rica",
  "Estonia",
  "Mexico",
  "Netherlands (the)",
  "Slovakia",
  "Turkey"                                         # Türkiye in thesis
)

panel_v <- panel %>%
  filter(!cname %in% excluded) %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(
    # 1-year lags
    CLENGTH_L1   = dplyr::lag(CLENGTH, 1),
    FIT_USD_L1   = dplyr::lag(FIT_USD, 1),
    ETS_cover_L1 = dplyr::lag(ETS_cover, 1),
    dummy_FIT_L1 = dplyr::lag(dummy_FIT, 1),
    dummy_RPS_L1 = dplyr::lag(dummy_RPS, 1),
    dummy_ETS_L1 = dplyr::lag(dummy_ETS, 1),
    # 2-year lags
    CLENGTH_L2   = dplyr::lag(CLENGTH, 2),
    FIT_USD_L2   = dplyr::lag(FIT_USD, 2),
    ETS_cover_L2 = dplyr::lag(ETS_cover, 2),
    dummy_FIT_L2 = dplyr::lag(dummy_FIT, 2),
    dummy_RPS_L2 = dplyr::lag(dummy_RPS, 2),
    dummy_ETS_L2 = dplyr::lag(dummy_ETS, 2),
    # 3-year lags
    CLENGTH_L3   = dplyr::lag(CLENGTH, 3),
    FIT_USD_L3   = dplyr::lag(FIT_USD, 3),
    ETS_cover_L3 = dplyr::lag(ETS_cover, 3),
    dummy_FIT_L3 = dplyr::lag(dummy_FIT, 3),
    dummy_RPS_L3 = dplyr::lag(dummy_RPS, 3),
    dummy_ETS_L3 = dplyr::lag(dummy_ETS, 3),
    # Controls
    lngdp        = log(gdp_oecd),
    land_km      = land_area / 1e6  # rescale to keep matrix well-conditioned
  ) %>%
  ungroup()

saveRDS(panel_v, "data/processed/panel_lagged.rds")

cat("Countries kept:", length(unique(panel_v$cname)), "\n")
cat("Total rows:", nrow(panel_v), "\n")
cat("Saved: data/processed/panel_lagged.rds\n")
