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
# Plot 8 — Growth-since-2000 by policy adoption status
# ------------------------------------------------------------
# Compares trajectories of "ever adopted" vs "never adopted" countries
# AFTER removing each country's 2000 baseline. So every country starts
# at 0 and the lines show *cumulative growth* in log(renewable) — i.e.,
# we're measuring how much renewable energy multiplied since 2000.
#
# This avoids the misleading "never-adopted is higher" pattern caused
# by FIT non-adopters being legacy-hydro giants (USA, Norway, Iceland)
# whose huge baselines dwarf FIT-driven new renewables.
# ============================================================
baseline <- panel %>%
  filter(year == 2000) %>%
  select(country, ln2000 = lnrenewable)

panel_growth <- panel %>%
  left_join(baseline, by = "country") %>%
  mutate(ln_growth = lnrenewable - ln2000)

# Helper for FIT / RPS — binary split (ever vs never adopted).
build_binary <- function(policy_col, policy_label) {
  ever <- panel %>%
    group_by(country) %>%
    summarise(ever = any(.data[[policy_col]] == 1, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(group = ifelse(ever, "Ever adopted", "Never adopted"))

  panel_growth %>%
    left_join(ever %>% select(country, group), by = "country") %>%
    group_by(year, group) %>%
    summarise(mean_growth = mean(ln_growth, na.rm = TRUE),
              n = n(), .groups = "drop") %>%
    mutate(policy = policy_label)
}

# Helper for ETS — three coverage tiers based on each country's MAX
# observed ETS_cover. Cutoff at 30% comes from a natural gap in the
# data (France 29% vs Latvia 34%). Sidesteps the dummy/coverage
# distinction the thesis already flagged: dummy is insignificant
# but coverage is significant.
build_ets_tiers <- function() {
  tiers <- panel %>%
    group_by(country) %>%
    summarise(max_cover = max(ETS_cover, na.rm = TRUE), .groups = "drop") %>%
    mutate(group = case_when(
      max_cover == 0   ~ "No ETS",
      max_cover <= 30  ~ "Low coverage (≤30%)",
      TRUE             ~ "High coverage (>30%)"
    ))

  panel_growth %>%
    left_join(tiers %>% select(country, group), by = "country") %>%
    group_by(year, group) %>%
    summarise(mean_growth = mean(ln_growth, na.rm = TRUE),
              n = n(), .groups = "drop") %>%
    mutate(policy = "ETS")
}

# Bind FIT/RPS (binary) with ETS (3-tier). The shared `group` column
# can hold any of the 5 distinct labels; a single colour scale covers
# all of them.
treated_data <- bind_rows(
  build_binary("dummy_FIT", "FIT"),
  build_binary("dummy_RPS", "RPS"),
  build_ets_tiers()
) %>%
  mutate(group = factor(group, levels = c(
    "Never adopted", "Ever adopted",
    "No ETS", "Low coverage (≤30%)", "High coverage (>30%)"
  )))

p8 <- ggplot(treated_data, aes(year, mean_growth, colour = group)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ policy) +
  scale_colour_manual(values = c(
    "Never adopted"          = "grey55",
    "Ever adopted"           = "#2c7fb8",
    "No ETS"                 = "grey55",
    "Low coverage (≤30%)"    = "#a6cee3",
    "High coverage (>30%)"   = "#1f4e79"
  )) +
  labs(
    title = "Cumulative renewable growth since 2000, by policy intensity",
    subtitle = "Each country normalized to its 2000 level. ETS panel uses coverage tiers, not just dummy.",
    x = NULL,
    y = "Mean Δ log(renewable) since 2000",
    colour = NULL
  ) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE))

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
  geom_point(aes(size = factor(n_pol)), shape = 21, fill = NA,
             colour = "black", stroke = 0.6) +
  scale_fill_gradient2(
    low = "#d7191c", mid = "white", high = "#1a9641",
    midpoint = 0, name = "YoY growth (%)",
    limits = c(-25, 50), oob = scales::squish
  ) +
  # Discrete size per policy-count so the differences are visually obvious
  # (continuous range was too compressed). 0 -> invisible, then a steep ramp.
  scale_size_manual(
    values = c("0" = 0, "1" = 1.8, "2" = 4, "3" = 6.5),
    name   = "# policies active"
  ) +
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
