# 05_visualizations.R
# Four visualizations of the thesis data + results.
# Plots are written to output/ as PNGs (gitignored — regenerate from code).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

panel <- readRDS("data/processed/panel_lagged.rds")
models <- readRDS("data/processed/pcse_models.rds")

# Short country names for cleaner plots.
panel <- panel %>%
  mutate(country = recode(cname,
    "Korea (the Republic of)"                                    = "South Korea",
    "United Kingdom of Great Britain and Northern Ireland (the)" = "UK",
    "United States of America (the)"                             = "USA"
  ))

theme_set(theme_minimal(base_size = 11))

# ============================================================
# Plot 1 — Renewable energy growth over time, by country
# ------------------------------------------------------------
# A "spaghetti" plot: every country in grey + the top-5 producers
# highlighted in colour. Log-y axis because renewables span ~5
# orders of magnitude across countries (Iceland small, US huge).
# ============================================================
top5 <- panel %>%
  filter(year == 2021) %>%
  arrange(desc(renewable)) %>%
  slice_head(n = 5) %>%
  pull(country)

p1 <- ggplot(panel, aes(year, renewable, group = country)) +
  geom_line(colour = "grey75", linewidth = 0.4) +
  geom_line(data = panel %>% filter(country %in% top5),
            aes(colour = country), linewidth = 1) +
  scale_y_log10(labels = label_comma()) +
  labs(
    title = "Renewable energy generation, 2000–2021",
    subtitle = "Top-5 producers in 2021 highlighted; all 30 OECD countries shown",
    x = NULL, y = "Renewable generation (log scale)",
    colour = NULL
  ) +
  theme(legend.position = "bottom")

ggsave("output/01_renewable_growth.png", p1, width = 8, height = 5, dpi = 150)
cat("Saved output/01_renewable_growth.png\n")

# ============================================================
# Plot 2 — Policy adoption heatmap (replaces Table A, animated over time)
# ------------------------------------------------------------
# For each country × year, a tile coloured by which of FIT/RPS/ETS
# is active. We pivot the three dummies to long form and facet
# by policy. Country order is by total policy-years (most active up top).
# ============================================================
policy_long <- panel %>%
  select(country, year, FIT = dummy_FIT, RPS = dummy_RPS, ETS = dummy_ETS) %>%
  pivot_longer(c(FIT, RPS, ETS), names_to = "policy", values_to = "active")

country_order <- policy_long %>%
  group_by(country) %>%
  summarise(total = sum(active, na.rm = TRUE), .groups = "drop") %>%
  arrange(total) %>%
  pull(country)

p2 <- policy_long %>%
  mutate(country = factor(country, levels = country_order),
         active  = factor(ifelse(active == 1, "Yes", "No"),
                          levels = c("No", "Yes"))) %>%
  ggplot(aes(year, country, fill = active)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  facet_wrap(~ policy) +
  scale_fill_manual(values = c(Yes = "#2c7fb8", No = "grey92")) +
  labs(
    title = "Policy adoption by country and year",
    subtitle = "Blue tile = policy in force that year",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme(legend.position = "bottom",
        panel.grid = element_blank())

ggsave("output/02_policy_heatmap.png", p2, width = 10, height = 7, dpi = 150)
cat("Saved output/02_policy_heatmap.png\n")

# ============================================================
# Plot 4 — Distribution of key variables (replaces Table C)
# ------------------------------------------------------------
# Densities of the four most interesting variables. Helps you eyeball
# scale, skew, and outliers — things a Table-of-statistics hides.
# ============================================================
desc <- panel %>%
  transmute(
    `log(renewable)`   = log(renewable),
    `Carbon intensity` = wdi_co2,
    `Interest rate`    = InterestRate,
    `ETS coverage (%)` = ETS_cover
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))

p4 <- ggplot(desc, aes(value)) +
  geom_density(fill = "#2c7fb8", colour = NA, alpha = 0.7) +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  labs(
    title = "Distribution of key variables (2000–2021)",
    subtitle = "30 OECD countries, all years pooled",
    x = NULL, y = "Density"
  )

ggsave("output/04_distributions.png", p4, width = 8, height = 5, dpi = 150)
cat("Saved output/04_distributions.png\n")

cat("\nAll plots written to output/\n")
