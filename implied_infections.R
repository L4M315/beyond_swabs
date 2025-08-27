library(tidyverse)
library(scales)
library(lubridate)
theme_set(theme_classic(base_size = 12))

# ---- 1) load + adapt columns from your file ----
# switch to death-based infections by replacing inf_case_* with inf_death_*
det <- readr::read_csv("data_output/est_infections_weekly.csv", show_col_types = FALSE) |>
  mutate(
    week    = as.Date(week),
    country = tolower(country)
  ) |>
  filter(
    country %in% c("france","italy","saudi arabia","united kingdom"),
    week <= as.Date("2022-12-31")   # cap at 31-Dec-2022
  ) |>
  transmute(
    country = str_to_title(country),           # nicer facet labels
    week,
    weekly_cases     = pmax(cases_w, 1),       # avoid zeros on log scale
    implied_inf_lo   = pmax(inf_case_low, 1),
    implied_inf_med  = pmax(inf_case_med, 1),
    implied_inf_hi   = pmax(inf_case_high, 1)
  )

# ---- 2) global era starts ----
alpha_start   <- as.Date("2020-12-21")
delta_start   <- as.Date("2021-05-24")
omicron_start <- as.Date("2021-12-20")

# ---- 3) data window (x) ----
min_week <- min(det$week, na.rm = TRUE)
max_week <- max(det$week, na.rm = TRUE)

# ---- 4) base eras, clamped to your window ----
eras_base <- tibble(
  variant = c("Wild-type","Alpha","Delta","Omicron"),
  start   = as.Date(c(min_week, alpha_start,   delta_start,   omicron_start)),
  end     = as.Date(c(alpha_start - 1, delta_start - 1, omicron_start - 1, max_week))
) |>
  filter(start <= end)

# ---- 5) per-country y-bounds (must be >0 for log scale) ----
ycols <- c("weekly_cases","implied_inf_lo","implied_inf_med","implied_inf_hi")
yrng <- det |>
  select(country, all_of(ycols)) |>
  pivot_longer(-country, values_to = "value") |>
  filter(is.finite(value), value > 0) |>
  group_by(country) |>
  summarise(ymin = min(value), ymax = max(value), .groups = "drop")

# ---- 6) replicate eras for each country and attach y-bounds ----
eras_cty <- tidyr::crossing(country = unique(det$country), eras_base) |>
  left_join(yrng, by = "country")

# ---- 7) colors (same as before) ----
era_cols    <- c("Wild-type"="#667FA6","Alpha"="#2E8B57","Delta"="#C96B00","Omicron"="#A24A87")
col_implied <- "#3569D1"
col_report  <- "#1A1A1A"

# ---- 8) plot ----
p <- ggplot(det, aes(week)) +
  # era shading (per-facet y-bounds, works with log scale)
  geom_rect(
    data = eras_cty,
    aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax, fill = variant),
    inherit.aes = FALSE, alpha = 0.30, colour = NA
  ) +
  # series
  geom_ribbon(aes(ymin = implied_inf_lo, ymax = implied_inf_hi),
              fill = alpha(col_implied, 0.22), colour = NA) +
  geom_line(aes(y = implied_inf_med, colour = "Estimated infections (median)"), linewidth = 0.9) +
  geom_line(aes(y = weekly_cases,    colour = "Reported cases"),             linewidth = 0.8) +
  facet_wrap(~ country, ncol = 2, scales = "free_y") +
  scale_colour_manual(NULL, values = c(
    "Estimated infections (median)" = col_implied,
    "Reported cases"             = col_report
  )) +
  scale_y_continuous(
    trans  = "log10",
    breaks = log_breaks(n = 6),
    labels = label_number(accuracy = 1, scale_cut = cut_short_scale()),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.02))) +
  labs(
    title = "Reported Cases vs Estimated Infections",
    x = NULL, y = "Weekly count (log scale)"
  ) +
  theme(
    legend.position    = "top",
    legend.key         = element_rect(fill = "#EEEEEE", colour = NA),
    strip.background   = element_rect(fill = "#F3F3F3", colour = NA),
    strip.text         = element_text(face = "bold"),
    panel.grid.major.y = element_line(colour = "#E9E9E9", linewidth = 0.55),
    panel.grid.minor.y = element_line(colour = "#F2F2F2", linewidth = 0.35),
    plot.title         = element_text(face = "bold")
  ) +
  # show a legend for the era colors (ordered)
  scale_fill_manual(
    name   = "Variant era",
    values = era_cols,
    breaks = c("Wild-type","Alpha","Delta","Omicron")
  ) +
  guides(
    fill   = guide_legend(order = 1, override.aes = list(alpha = 0.6, colour = NA)),
    colour = guide_legend(order = 2)
  )

dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)
ggsave("outputs/plots/reported_vs_implied_with_eras_log.png",
       p, width = 10, height = 7, dpi = 320, bg = "white")
