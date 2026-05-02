# 01_load_data.R
# Load the raw Excel and write a clean RDS (R's native binary format) to
# data/processed/. Later scripts read the RDS — fast and unambiguous.

library(readxl)
library(dplyr)

raw_path <- "data/raw/filtered_data_2000_2021.xlsx"
out_path <- "data/processed/panel.rds"

raw <- read_excel(raw_path)

panel <- raw %>%
  rename(
    InterestRate = `Interest Rate`,  # backticks because of the space
    renewable    = pg_regtoreen      # match Stata's DV name
  ) %>%
  mutate(
    lnrenewable  = log(renewable),
    pct_renewable = renewable / gdp_oecd * 100  # rough placeholder; see note
  ) %>%
  arrange(ccode, year)

saveRDS(panel, out_path)

cat("Rows:", nrow(panel), " Cols:", ncol(panel), "\n")
cat("Countries:", length(unique(panel$cname)),
    " Years:", paste(range(panel$year), collapse = "-"), "\n")
cat("Saved:", out_path, "\n")
