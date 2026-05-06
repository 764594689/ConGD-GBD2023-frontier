# ==============================================================================
# Figure S4: COVID-19 Sensitivity — Trends excluding 2020-2023 (GBD 2023)
# ==============================================================================
library(tidyverse)
library(scales)

cat("--- Figure S4: COVID Sensitivity (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_trend <- file.path(base_dir, "data/gbd2023_trend_1990_2023.csv.zip")
output_dir <- file.path(base_dir, "outputs/figures")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

structural_list <- c(
  "Congenital heart anomalies", "Neural tube defects", "Down syndrome",
  "Other chromosomal abnormalities", "Orofacial clefts",
  "Digestive congenital anomalies", "Urogenital congenital anomalies",
  "Congenital musculoskeletal and limb anomalies", "Other congenital birth defects"
)
hemoglobin_list <- c(
  "Sickle cell disorders", "Thalassemias",
  "G6PD deficiency", "Other hemoglobinopathies and hemolytic anemias"
)
genetic_list <- c(structural_list, hemoglobin_list)

df_raw <- read_csv(path_trend, show_col_types = FALSE)

df_base <- df_raw %>%
  filter(location_name == "Global", age_name == "<5 years",
         sex_name == "Both", measure_name == "Deaths",
         metric_name == "Number")

df_all_deaths <- df_base %>%
  filter(cause_name == "All causes") %>%
  select(year, all_deaths = val)

df_genetic <- df_base %>%
  filter(cause_name %in% genetic_list) %>%
  group_by(year) %>%
  summarise(deaths = sum(val, na.rm = TRUE), .groups = "drop") %>%
  mutate(cause_group = "ConGDs")

df_cmnn <- df_base %>%
  filter(str_detect(cause_name, "Communicable, maternal")) %>%
  select(year, deaths = val) %>%
  mutate(cause_group = "CMNN")

df_pmr <- bind_rows(df_genetic, df_cmnn) %>%
  left_join(df_all_deaths, by = "year") %>%
  mutate(pmr = deaths / all_deaths)

df_full <- df_pmr %>% mutate(panel = "A. Full period (1990\u20132023)")
df_excl <- df_pmr %>% filter(year <= 2019) %>% mutate(panel = "B. Excluding 2020\u20132023")
df_plot <- bind_rows(df_full, df_excl)

plot_s2 <- ggplot(df_plot, aes(x = year, y = pmr, color = cause_group)) +
  geom_line(linewidth = 1.3) +
  facet_wrap(~panel, ncol = 2) +
  scale_color_manual(name = "Cause Group",
                     values = c("ConGDs" = "#0072B2", "CMNN" = "#E69F00")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  labs(title = NULL, subtitle = NULL,
       x = "Year", y = "PMR (%)") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.position = "bottom", panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold"),
        strip.text = element_text(face = "bold", size = 16))

ggsave(file.path(output_dir, "Figure_S4_covid.png"), plot_s2, width = 14, height = 7, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "Figure_S4_covid.pdf"), plot_s2, width = 14, height = 7, dpi = 300, bg = "white")

cat("\n=== PMR Comparison ===\n")
for (grp in c("ConGDs", "CMNN")) {
  v2019 <- df_pmr %>% filter(year == 2019, cause_group == grp) %>% pull(pmr)
  v2023 <- df_pmr %>% filter(year == 2023, cause_group == grp) %>% pull(pmr)
  if (length(v2019) > 0 && length(v2023) > 0)
    cat(sprintf("  %s: 2019=%.2f%%, 2023=%.2f%%, diff=%.2f pp\n",
                grp, v2019*100, v2023*100, (v2023-v2019)*100))
}
cat("Figure S4 saved to:", output_dir, "\n")
