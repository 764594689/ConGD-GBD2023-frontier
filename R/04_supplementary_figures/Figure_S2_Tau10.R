# ==============================================================================
# Figure S2: Frontier Sensitivity — tau=0.05 vs tau=0.10 (GBD 2023)
# ==============================================================================
library(tidyverse)
library(quantreg)
library(ggrepel)
library(splines)

cat("--- Figure S2: Frontier Sensitivity (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_country   <- file.path(base_dir, "data/gbd2023_sdi_country_2023.csv.zip")
path_sdi       <- file.path(base_dir, "data/gbd2023_sdi_values_1950_2023.csv")
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

# --- Data (same as R6) ---
df_raw <- read_csv(path_country, show_col_types = FALSE)
df_mortality <- df_raw %>%
  filter(year == 2023, age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths", cause_name %in% genetic_list,
         metric_name == "Rate") %>%
  group_by(location_name) %>%
  summarise(total_rate = sum(val, na.rm = TRUE), .groups = "drop")

df_sdi <- read_csv(path_sdi, show_col_types = FALSE) %>%
  filter(year_id == 2023) %>%
  select(location_name, sdi_value = mean_value) %>%
  mutate(sdi_group = case_when(
    sdi_value <= 0.454 ~ "Low SDI", sdi_value <= 0.608 ~ "Low-middle SDI",
    sdi_value <= 0.701 ~ "Middle SDI", sdi_value <= 0.813 ~ "High-middle SDI",
    TRUE ~ "High SDI"
  ) %>% factor(levels = c("Low SDI", "Low-middle SDI", "Middle SDI",
                           "High-middle SDI", "High SDI")))

df_final <- df_mortality %>%
  inner_join(df_sdi, by = "location_name") %>%
  drop_na(sdi_value, total_rate) %>%
  filter(total_rate > 0)

# --- Two quantile regressions ---
qr_05 <- rq(log(total_rate) ~ ns(sdi_value, df = 3), tau = 0.05, data = df_final)
qr_10 <- rq(log(total_rate) ~ ns(sdi_value, df = 3), tau = 0.10, data = df_final)

sdi_grid <- data.frame(sdi_value = seq(min(df_final$sdi_value),
                                        max(df_final$sdi_value), length.out = 300))
sdi_grid$frontier_05 <- exp(predict(qr_05, newdata = sdi_grid))
sdi_grid$frontier_10 <- exp(predict(qr_10, newdata = sdi_grid))

sdi_pal <- c("Low SDI" = "#D7191C", "Low-middle SDI" = "#FDAE61",
             "Middle SDI" = "#FFFFBF", "High-middle SDI" = "#ABD9E9",
             "High SDI" = "#2C7BB6")

plot_s1 <- ggplot(df_final, aes(x = sdi_value, y = total_rate)) +
  geom_point(aes(fill = sdi_group), color = "white", shape = 21,
             alpha = 0.7, size = 2.5, stroke = 0.4) +
  geom_line(data = sdi_grid, aes(x = sdi_value, y = frontier_05, linetype = "\u03C4 = 0.05"),
            color = "#2CA25F", linewidth = 1.5, inherit.aes = FALSE) +
  geom_line(data = sdi_grid, aes(x = sdi_value, y = frontier_10, linetype = "\u03C4 = 0.10"),
            color = "#E6550D", linewidth = 1.2, inherit.aes = FALSE) +
  scale_fill_manual(name = "SDI Quintile", values = sdi_pal) +
  scale_linetype_manual(name = "Frontier",
                        values = c("\u03C4 = 0.05" = "solid", "\u03C4 = 0.10" = "dashed")) +
  scale_y_continuous(limits = c(0, max(df_final$total_rate) * 1.05), expand = c(0, 0)) +
  labs(title = NULL, subtitle = NULL,
       x = "SDI", y = "ASMR of ConGDs (per 100,000)") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.position = "bottom", legend.box = "vertical", panel.grid.minor = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.line = element_line(color = "black"),
        text = element_text(family = "serif")) +
  guides(fill = guide_legend(order = 1), linetype = guide_legend(order = 2))

ggsave(file.path(output_dir, "Figure_S2_tau10.png"), plot_s1, width = 12, height = 8.5, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "Figure_S2_tau10.pdf"), plot_s1, width = 12, height = 8.5, dpi = 300, bg = "white")

# Gap correlation
df_final$gap_05 <- df_final$total_rate - exp(predict(qr_05, df_final))
df_final$gap_10 <- df_final$total_rate - exp(predict(qr_10, df_final))
cat(sprintf("\nSpearman rho (gap_05 vs gap_10): %.3f\n",
            cor(df_final$gap_05, df_final$gap_10, method = "spearman")))
cat("Figure S2 saved to:", output_dir, "\n")
