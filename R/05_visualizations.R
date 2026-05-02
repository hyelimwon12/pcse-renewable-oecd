# 05_visualizations.R
# Four visualizations of the thesis data + results.
# Plots are written to output/ as PNGs (gitignored — regenerate from code).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(forcats)
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
# Plot 3 — Coefficient plot (replaces Table E, 3-year lag model)
# ------------------------------------------------------------
# Dot-and-whisker: coefficient ± 1.96*SE, colour by significance.
# A coefficient plot is almost always more readable than a table —
# you see effect direction, magnitude, and uncertainty at once.
# ============================================================
ct <- models$L3$ct  # the coeftest object

coefs <- tibble::tibble(
    term = rownames(ct),
    estimate = ct[, "Estimate"],
    se       = ct[, "Std. Error"],
    p        = ct[, "Pr(>|t|)"]
  ) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se,
    sig   = case_when(
      p < 0.01  ~ "p < 0.01",
      p < 0.05  ~ "p < 0.05",
      p < 0.10  ~ "p < 0.10",
      TRUE      ~ "n.s."
    ),
    sig = factor(sig, levels = c("p < 0.01", "p < 0.05", "p < 0.10", "n.s."))
  )

# Re-order so largest-magnitude effects sit at top of plot
coefs <- coefs %>% mutate(term = fct_reorder(term, abs(estimate)))

p3 <- ggplot(coefs, aes(estimate, term, colour = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y") +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c(
    "p < 0.01" = "#2c7fb8", "p < 0.05" = "#41b6c4",
    "p < 0.10" = "#7fcdbb", "n.s."     = "grey60"
  )) +
  labs(
    title = "PCSE regression coefficients on log(renewable)",
    subtitle = "Three-year lag model, R replication of thesis Table E",
    x = "Coefficient (95% CI)", y = NULL, colour = NULL
  ) +
  theme(legend.position = "bottom")

ggsave("output/03_coefficient_plot.png", p3, width = 8, height = 5, dpi = 150)
cat("Saved output/03_coefficient_plot.png\n")

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

# ============================================================
# Plot 5 — Faceted coefficient plot across all three lag models
# ------------------------------------------------------------
# Direct visual replacement for thesis Table E: every column of the
# table becomes a panel in one figure. Easier to spot which effects
# are robust across lag specifications and which only show up in one.
# ============================================================
extract_coefs <- function(model_obj, lag_label) {
  ct <- model_obj$ct
  tibble::tibble(
    term     = rownames(ct),
    estimate = ct[, "Estimate"],
    se       = ct[, "Std. Error"],
    p        = ct[, "Pr(>|t|)"]
  ) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      lag = lag_label,
      # Strip trailing "_L1" / "_L2" / "_L3" so the same variable lines up
      # across panels on the y-axis.
      term_clean = sub("_L[123]$", "", term)
    )
}

all_coefs <- bind_rows(
  extract_coefs(models$L1, "1-year lag"),
  extract_coefs(models$L2, "2-year lag"),
  extract_coefs(models$L3, "3-year lag")
) %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se,
    sig   = case_when(
      p < 0.01 ~ "p < 0.01",
      p < 0.05 ~ "p < 0.05",
      p < 0.10 ~ "p < 0.10",
      TRUE     ~ "n.s."
    ),
    sig = factor(sig, levels = c("p < 0.01", "p < 0.05", "p < 0.10", "n.s."))
  )

# Order variables: policy variables on top, controls below, by L3 magnitude
order_terms <- all_coefs %>%
  filter(lag == "3-year lag") %>%
  arrange(abs(estimate)) %>%
  pull(term_clean)

all_coefs <- all_coefs %>%
  mutate(term_clean = factor(term_clean, levels = order_terms))

p5 <- ggplot(all_coefs, aes(estimate, term_clean, colour = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y") +
  geom_point(size = 2) +
  facet_wrap(~ lag) +
  scale_colour_manual(values = c(
    "p < 0.01" = "#2c7fb8", "p < 0.05" = "#41b6c4",
    "p < 0.10" = "#7fcdbb", "n.s."     = "grey60"
  )) +
  labs(
    title = "PCSE regression coefficients across lag specifications",
    subtitle = "Visual replacement for thesis Table E — three lag models side by side",
    x = "Coefficient (95% CI)", y = NULL, colour = NULL
  ) +
  theme(legend.position = "bottom")

ggsave("output/05_table_e_faceted.png", p5, width = 11, height = 5, dpi = 150)
cat("Saved output/05_table_e_faceted.png\n")

cat("\nAll plots written to output/\n")
