# 06_policy_effect_plots.R
# Visualizations focused on the relationship between policies (FIT/RPS/ETS)
# and renewable energy growth.
#
# Four plots:
#   06 - Event study: RE around year of first policy adoption
#   07 - Country case studies: 6 illustrative trajectories with policy timeline
#   08 - Treated vs never-treated: mean RE trajectory by group, by policy
#   09 - Growth-rate heatmap with policy-state overlay

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(forcats)
})

panel <- readRDS("data/processed/panel_lagged.rds") %>%
  mutate(country = recode(cname,
    "Korea (the Republic of)"                                    = "South Korea",
    "United Kingdom of Great Britain and Northern Ireland (the)" = "UK",
    "United States of America (the)"                             = "USA"
  ))

theme_set(theme_minimal(base_size = 11))

# ============================================================
# Plot 6 — Event study around first policy adoption
# ------------------------------------------------------------
# For each country that ever adopted policy P, define
#   t = 0  := first year with dummy_P == 1
#   event_time = year - t
# Center each country's lnrenewable on its value at t=0, then average
# across countries by event_time. Result: "average country's path
# before & after adopting that policy."
# ============================================================
build_event_study <- function(policy_dummy_col, policy_label) {
  adoption <- panel %>%
    filter(.data[[policy_dummy_col]] == 1) %>%
    group_by(country) %>%
    summarise(year_adopt = min(year), .groups = "drop")

  panel %>%
    inner_join(adoption, by = "country") %>%
    group_by(country) %>%
    mutate(
      event_time   = year - year_adopt,
      ln_at_zero   = lnrenewable[year == year_adopt][1],
      ln_centered  = lnrenewable - ln_at_zero
    ) %>%
    ungroup() %>%
    filter(event_time >= -5, event_time <= 10, !is.na(ln_centered)) %>%
    group_by(event_time) %>%
    summarise(
      mean_ln = mean(ln_centered, na.rm = TRUE),
      se_ln   = sd(ln_centered, na.rm = TRUE) / sqrt(n()),
      n       = n(),
      .groups = "drop"
    ) %>%
    mutate(policy = policy_label)
}

event_data <- bind_rows(
  build_event_study("dummy_FIT", "FIT"),
  build_event_study("dummy_RPS", "RPS"),
  build_event_study("dummy_ETS", "ETS")
)

p6 <- ggplot(event_data, aes(event_time, mean_ln)) +
  geom_ribbon(aes(ymin = mean_ln - 1.96 * se_ln,
                  ymax = mean_ln + 1.96 * se_ln),
              fill = "#2c7fb8", alpha = 0.2) +
  geom_line(colour = "#2c7fb8", linewidth = 1) +
  geom_point(colour = "#2c7fb8", size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
  facet_wrap(~ policy) +
  labs(
    title = "Event study: renewable energy around first policy adoption",
    subtitle = "Mean change in log(renewable) relative to adoption year (=0). 95% CI band.",
    x = "Years since policy adoption", y = "Δ log(renewable)"
  )

ggsave("output/06_event_study.png", p6, width = 11, height = 4.5, dpi = 150)
cat("Saved output/06_event_study.png\n")

# ============================================================
# Plot 7 — Country case studies
# ------------------------------------------------------------
# Six countries with distinct policy stories. Each panel: line = renewable
# generation over time, vertical ribbons mark when each policy was active.
# ============================================================
case_countries <- c("Germany", "USA", "South Korea", "Spain", "UK", "Iceland")

case_data <- panel %>% filter(country %in% case_countries)

# For each (country, policy) get the YEAR FIRST ADOPTED — drawn as a
# coloured dashed vertical line. Cleaner than translucent fill bands
# (which collide with log-scale axes and end up invisible).
policy_starts <- case_data %>%
  select(country, year, FIT = dummy_FIT, RPS = dummy_RPS, ETS = dummy_ETS) %>%
  pivot_longer(c(FIT, RPS, ETS), names_to = "policy", values_to = "active") %>%
  filter(active == 1) %>%
  group_by(country, policy) %>%
  summarise(adopt_year = min(year), .groups = "drop") %>%
  mutate(country = factor(country, levels = case_countries))

p7 <- ggplot(case_data %>%
               mutate(country = factor(country, levels = case_countries)),
             aes(year, renewable)) +
  geom_vline(data = policy_starts,
             aes(xintercept = adopt_year, colour = policy),
             linetype = "dashed", linewidth = 0.7) +
  geom_line(linewidth = 0.9, colour = "grey15") +
  geom_point(size = 1.2, colour = "grey15") +
  scale_y_log10(labels = label_comma()) +
  scale_colour_manual(values = c(FIT = "#1b9e77", RPS = "#d95f02", ETS = "#7570b3")) +
  facet_wrap(~ country, scales = "free_y") +
  labs(
    title = "Country case studies: renewable energy and first policy adoption",
    subtitle = "Black line = renewable (log scale). Dashed line = year policy first adopted.",
    x = NULL, y = "Renewable generation (log scale)", colour = "Policy"
  ) +
  theme(legend.position = "bottom")

ggsave("output/07_country_cases.png", p7, width = 11, height = 6.5, dpi = 150)
cat("Saved output/07_country_cases.png\n")

# ============================================================
# Plot 8 — Treated vs never-treated mean trajectory
# ------------------------------------------------------------
# For each policy, split countries into "ever adopted by 2021" vs "never".
# Plot mean lnrenewable by year for each group. Caveat-y but intuitive.
# ============================================================
build_treated <- function(policy_col, policy_label) {
  ever <- panel %>%
    group_by(country) %>%
    summarise(ever = any(.data[[policy_col]] == 1, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(group = ifelse(ever, "Ever adopted", "Never adopted"))

  panel %>%
    left_join(ever %>% select(country, group), by = "country") %>%
    group_by(year, group) %>%
    summarise(mean_ln = mean(lnrenewable, na.rm = TRUE),
              n = n(), .groups = "drop") %>%
    mutate(policy = policy_label)
}

treated_data <- bind_rows(
  build_treated("dummy_FIT", "FIT"),
  build_treated("dummy_RPS", "RPS"),
  build_treated("dummy_ETS", "ETS")
)

p8 <- ggplot(treated_data, aes(year, mean_ln, colour = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ policy) +
  scale_colour_manual(values = c("Ever adopted" = "#2c7fb8",
                                 "Never adopted" = "grey50")) +
  labs(
    title = "Mean log(renewable) over time, by policy adoption status",
    subtitle = "Country counts vary across panels (countries can be 'never' for one policy, 'ever' for another)",
    x = NULL, y = "Mean log(renewable)", colour = NULL
  ) +
  theme(legend.position = "bottom")

ggsave("output/08_treated_vs_untreated.png", p8, width = 11, height = 4.5, dpi = 150)
cat("Saved output/08_treated_vs_untreated.png\n")

# ============================================================
# Plot 9 — Growth-rate heatmap with policy-count overlay
# ------------------------------------------------------------
# Tile fill = year-over-year growth rate of renewable.
# Overlay (size of dot) = number of policies (0..3) active that year.
# Lets you scan whether high-growth cells coincide with policy presence.
# ============================================================
growth <- panel %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    growth = (renewable / dplyr::lag(renewable, 1) - 1) * 100,
    n_pol  = dummy_FIT + dummy_RPS + dummy_ETS
  ) %>%
  ungroup() %>%
  filter(!is.na(growth))

# Order countries by mean growth (highest at top)
country_order <- growth %>%
  group_by(country) %>%
  summarise(g = mean(growth, na.rm = TRUE), .groups = "drop") %>%
  arrange(g) %>%
  pull(country)

p9 <- growth %>%
  mutate(country = factor(country, levels = country_order)) %>%
  ggplot(aes(year, country)) +
  geom_tile(aes(fill = pmin(pmax(growth, -25), 50)),
            colour = "white", linewidth = 0.1) +
  geom_point(aes(size = n_pol), shape = 21, fill = NA, colour = "black",
             stroke = 0.4) +
  scale_fill_gradient2(
    low = "#d7191c", mid = "white", high = "#1a9641",
    midpoint = 0, name = "YoY growth (%)",
    limits = c(-25, 50), oob = scales::squish
  ) +
  scale_size_continuous(range = c(0, 2.5), breaks = 0:3,
                        name = "# policies active") +
  labs(
    title = "Year-over-year renewable growth by country, with policy presence",
    subtitle = "Cell colour = growth rate; circle size = number of (FIT, RPS, ETS) active",
    x = NULL, y = NULL
  ) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

ggsave("output/09_growth_heatmap.png", p9, width = 11, height = 7, dpi = 150)
cat("Saved output/09_growth_heatmap.png\n")

cat("\nAll policy-effect plots written to output/\n")
