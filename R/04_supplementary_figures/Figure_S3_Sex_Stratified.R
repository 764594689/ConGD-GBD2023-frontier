# ==============================================================================
# Figure S3: Sex-Specific ConGD Mortality Trends (GBD 2023)
# Data: gbd2023_sex_stratified_1990_2023.csv.zip
# ==============================================================================
library(tidyverse)
library(patchwork)

cat("--- Figure S3: Sex-Specific Trends (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_sex   <- file.path(base_dir, "data/gbd2023_sex_stratified_1990_2023.csv.zip")
output_dir <- file.path(base_dir, "outputs/figures")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

if (!file.exists(path_sex)) {
  stop("Sex-stratified data not found: ", path_sex)
}

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

df_raw <- read_csv(path_sex, show_col_types = FALSE)

df_base <- df_raw %>%
  mutate(cause_name = str_trim(cause_name), sex_name = str_trim(sex_name)) %>%
  filter(location_name == "Global", measure_name == "Deaths",
         sex_name %in% c("Male", "Female"))

# A. Composite CGD ASMR
df_composite <- df_base %>%
  filter(cause_name %in% genetic_list, metric_name == "Rate") %>%
  group_by(year, sex_name) %>%
  summarise(asmr = sum(val, na.rm = TRUE),
            lower = sum(lower, na.rm = TRUE),
            upper = sum(upper, na.rm = TRUE), .groups = "drop")

# B. Key conditions for M:F ratio
key_conditions <- c("Congenital heart anomalies", "G6PD deficiency", "Sickle cell disorders")
df_by_cause <- df_base %>%
  filter(cause_name %in% key_conditions, metric_name == "Rate") %>%
  select(year, sex_name, cause_name, asmr = val)

df_ratio <- df_by_cause %>%
  pivot_wider(names_from = sex_name, values_from = asmr) %>%
  mutate(mf_ratio = Male / Female)

# Panel A
plot_a <- ggplot(df_composite, aes(x = year, y = asmr, color = sex_name)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex_name), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = c("Male" = "#2166AC", "Female" = "#B2182B")) +
  scale_fill_manual(values = c("Male" = "#2166AC", "Female" = "#B2182B")) +
  labs(title = "A",
       x = "Year", y = "ASMR (per 100,000)", color = "Sex", fill = "Sex") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0),
        legend.position = "bottom", panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold"))

# Panel B
plot_b <- ggplot(df_ratio, aes(x = year, y = mf_ratio, color = cause_name)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  labs(title = "B",
       x = "Year", y = "M:F Ratio", color = "Condition") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0),
        legend.position = "bottom", panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold"))

final_s4 <- plot_a / plot_b +
  plot_annotation(
    theme = theme(plot.margin = margin(10, 10, 10, 10))
  )

ggsave(file.path(output_dir, "Figure_S3_Sex.png"), final_s4, width = 12, height = 10, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "Figure_S3_Sex.pdf"), final_s4, width = 12, height = 10, dpi = 300, bg = "white")
cat("Figure S3 saved to:", output_dir, "\n")
