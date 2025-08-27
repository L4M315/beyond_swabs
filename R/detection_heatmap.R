# detection-fraction heatmap (years on x-axis only)
library(tidyverse)
library(lubridate)
library(scales)
library(stringr)

plot_dir <- "outputs/plots"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# data
df <- readr::read_csv("data_output/est_infections_weekly.csv", show_col_types = FALSE) |>
  mutate(
    week    = as.Date(week),
    country = tolower(country)
  )

countries <- c("france", "italy", "saudi arabia", "united kingdom")

hm <- df |>
  filter(country %in% countries) |>
  mutate(country = factor(country, levels = countries)) |>
  select(country, week, det_frac_med)

# only year numbers on x axis
year_breaks <- seq(
  floor_date(min(hm$week, na.rm = TRUE), "year"),
  ceiling_date(max(hm$week, na.rm = TRUE), "year"),
  by = "1 year"
)

p_hm <- ggplot(hm, aes(x = week, y = country, fill = det_frac_med)) +
  geom_tile() +
  scale_fill_distiller(
    palette = "Blues", direction = 1,
    name = "% detected", labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.25), limits = c(0, 1), oob = squish
  ) +
  scale_x_date(breaks = year_breaks, labels = label_date("%Y")) +
  scale_y_discrete(labels = ~ str_to_title(.x)) +
  labs(
    title    = "Estimated Detection (%) By Country and Week",
    subtitle = "Detection = IFR / dCFR. Color capped at 100%.",
    x = NULL, y = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle = element_text(size = 12, margin = margin(t = 2, b = 10)),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10),
    panel.grid   = element_blank(),
    axis.line    = element_blank(),
    axis.ticks   = element_blank(),
    panel.border = element_blank()
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0,
      barwidth = unit(6, "in"),
      barheight = unit(0.25, "in"),
      ticks = TRUE
    )
  )

ggsave(file.path(plot_dir, "detection_fraction_heatmap_weekly_report.png"),
       p_hm, width = 7, height = 6.5, dpi = 300)
