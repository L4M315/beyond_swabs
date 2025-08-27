library(tidyverse)
library(scales)
library(stringr)

plot_dir <- "outputs/plots"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# countries to show (title case for legend)
ctys <- c("France", "Italy", "Saudi Arabia", "United Kingdom")

# read UN-style table and tidy to 5-year bands
age_raw <- readr::read_csv("data_raw/age_distribution.csv", show_col_types = FALSE)

age_long <- age_raw %>%
  filter(Variant == "Estimates",
         Type == "Country/Area",
         Year == 2020,
         `Region, subregion, country or area *` %in% ctys) %>%
  rename(country = `Region, subregion, country or area *`) %>%
  pivot_longer(
    cols = matches("^\\d{1,3}-\\d{1,3}$|^100\\+$"),   # 0-4 ... 95-99, 100+
    names_to = "age_band", values_to = "pop"
  ) %>%
  group_by(country) %>%
  mutate(share = pop / sum(pop)) %>%
  ungroup()

# keep the natural left→right band order
age_levels <- age_long %>%
  distinct(age_band) %>%
  tidyr::separate(age_band, into = c("a","b"), sep = "-", fill = "right", remove = FALSE) %>%
  mutate(
    a = readr::parse_number(a),
    b = readr::parse_number(b),
    b = dplyr::coalesce(b, 150)  # put "100+" (or any "+") last
  ) %>%
  arrange(a, b) %>%
  pull(age_band)

# apply the ordered levels
age_long <- age_long %>%
  mutate(age_band = factor(age_band, levels = age_levels))


# colors (Saudi fixed to #005430)
cols <- c("France" = "#1f77b4",
          "Italy"  = "#e15759",
          "Saudi Arabia" = "#005430",
          "United Kingdom" = "#9467bd")

# x position for dashed guide at 35–39
xline <- which(levels(age_long$age_band) == "35-39")

p <- ggplot(age_long, aes(x = age_band, y = share, color = country, group = country)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.3) +
  { if (length(xline) == 1) geom_vline(xintercept = xline, linetype = "dashed", color = "grey50") } +
  scale_color_manual(values = cols, name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.05))) +
  labs(title = "Age Distribution By 5-year Band",
       x = "Age Band", y = "Share of Population") +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(colour = "#eeeeee"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16)
  )

ggsave(file.path(plot_dir, "age_distribution_lines.png"),
       p, width = 9.5, height = 4.8, dpi = 320, bg = "white")
