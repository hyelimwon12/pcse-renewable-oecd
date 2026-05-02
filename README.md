# Renewable Energy Policy & Capacity (R replication)

Replicating in R the panel-data analysis from Hyelim Won's master's thesis,
originally run in Stata (2000–2021, 27 OECD countries).

## Research question
Do renewable-energy policy instruments (FIT, RPS, ETS) drive renewable
electricity capacity in OECD countries?

## Models
- **Case 1**: country fixed-effects (`xtreg ... fe`) with 3-year lagged policies
  and carbon intensity (`wdi_co2`) as a control.
- **Case 2**: same FE specification, dropping `wdi_co2`; runs on three DVs —
  `renewable`, `lnrenewable`, `pct_renewable`.
- **Case 3**: Prais-Winsten with panel-corrected standard errors
  (`xtpcse ... correlation(psar1)`).

Mexico is excluded (`if cname != "Mexico"`).

## Project layout
```
R/                  analysis scripts (run in numeric order)
data/raw/           original Excel — never edit
data/processed/     cleaned/transformed data written by scripts
output/             tables, figures (not tracked in git)
```

## How to reproduce
```r
source("R/01_load_data.R")
source("R/02_build_variables.R")
source("R/03_fe_regressions.R")
source("R/04_pcse_regression.R")
```
