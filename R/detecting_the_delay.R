library(tidyverse); library(lubridate); library(scales)


raw <- "data_raw"

read_jhu <- function(fname, val){
  readr::read_csv(file.path(raw, fname), show_col_types = FALSE) |>
    rename(country = `Country/Region`) |>
    pivot_longer(-(1:4), names_to = "date", values_to = val) |>
    mutate(date = as.Date(date, "%m/%d/%y")) |>
    group_by(country, date) |>
    summarise(!!val := sum(.data[[val]]), .groups = "drop")
}

cases  <- read_jhu("time_series_covid19_confirmed_global.csv", "cum_cases")
deaths <- read_jhu("time_series_covid19_deaths_global.csv",    "cum_deaths")
df_daily <- inner_join(cases, deaths, by = c("country","date"))


plot_country_daily_pretty <- function(df, c, d = 14, start = NULL, end = NULL, smooth = 7){
  if ("week_start" %in% names(df) && !"date" %in% names(df)) df <- dplyr::rename(df, date = week_start)
  df <- df |> dplyr::mutate(date = as.Date(date))   # ensure Date
  
  dd <- df |>
    dplyr::filter(country == c) |>
    dplyr::arrange(.data$date) |>
    dplyr::mutate(
      daily_cases  = pmax(cum_cases  - dplyr::lag(cum_cases,  default = 0), 0),
      daily_deaths = pmax(cum_deaths - dplyr::lag(cum_deaths, default = 0), 0)
    )
  
  if (!is.null(start) && !is.null(end))
    dd <- dd |> dplyr::filter(date >= as.Date(start), date <= as.Date(end))
  
  k <- if (smooth > 1) rep(1/smooth, smooth) else 1
  dd <- dd |>
    dplyr::mutate(
      cases_s  = as.numeric(stats::filter(daily_cases,  k, sides = 2)),
      deaths_s = as.numeric(stats::filter(daily_deaths, k, sides = 2)),
      deaths_date = date - d
    )
  
  K <- max(dd$cases_s, na.rm = TRUE) / max(dd$deaths_s, na.rm = TRUE)
  
  ggplot(dd) +
    geom_line(aes(date,        cases_s,           colour = "Cases"), linewidth = 1) +
    geom_line(aes(deaths_date, deaths_s * K,      colour = "Deaths"), linewidth = 1) +
    scale_colour_manual(values = c("Cases" = "#3569D1",
                                   "Deaths" = "#e31a1c"),
                        name = NULL) +
    scale_y_continuous(
      "Daily cases",
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      sec.axis = sec_axis(~ . / K, name = "Daily deaths",
                          labels = scales::label_number(scale_cut = scales::cut_short_scale()))
    ) +
    labs(x = NULL, subtitle = paste0("Deaths shifted by ", d, " days")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linewidth = 0.5),
          panel.grid.major.y = element_line(linewidth = 0.5))
}

# If you built df_daily from the JHU CSVs:
plot_country_daily_pretty(df_daily, "Italy", d = 13,
                          start = "2020-07-01", end = "2021-07-07")

ggsave("outputs/plots/italy_daily_d13.png", width = 9, height = 4, dpi = 300)



# If you only have the weekly frame:
# plot_country_daily_pretty(wk_df, "France", d = 17, start = "2020-03-01", end = "2021-06-30")


# ----------------------------------------------------------------------------
# EDIT THIS
# ----------------------------------------------------------------------------


p <- plot_country_daily_pretty(df_daily, "Italy", d = 14,
                               start = "2020-07-01", end = "2021-07-07") +
  labs(title = "Italy") +
  theme(
    text = element_text(size = 13),
    plot.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 12)
  )

ggsave("outputs/plots/italy_daily_small_14_shift.png", p, width = 5.6, height = 3.0, dpi = 450, bg = "white")

