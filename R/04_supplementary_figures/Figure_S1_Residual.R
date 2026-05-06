# ==============================================================================
# Figure S1: Frontier Model Residual Diagnostics (GBD 2023)
# ==============================================================================
library(tidyverse)
library(quantreg)
library(splines)
library(patchwork)

cat("--- Figure S1: Residual Diagnostics (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_country <- file.path(base_dir, "data/gbd2023_sdi_country_2023.csv.zip")
path_sdi     <- file.path(base_dir, "data/gbd2023_sdi_values_1950_2023.csv")
output_dir <- file.path(base_dir, "outputs/figures")

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

df_raw <- read_csv(path_country, show_col_types = FALSE)
df_mortality <- df_raw %>%
  filter(year == 2023, age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths", cause_name %in% genetic_list,
         metric_name == "Rate") %>%
  group_by(location_name) %>%
  summarise(total_rate = sum(val, na.rm = TRUE), .groups = "drop")

df_sdi <- read_csv(path_sdi, show_col_types = FALSE) %>%
  filter(year_id == 2023) %>%
  select(location_name, sdi_value = mean_value)

df_final <- df_mortality %>%
  inner_join(df_sdi, by = "location_name") %>%
  drop_na(sdi_value, total_rate) %>%
  filter(total_rate > 0)

# Model
qr_model <- rq(log(total_rate) ~ ns(sdi_value, df = 3), tau = 0.05, data = df_final)
df_final$predicted <- predict(qr_model, df_final)
df_final$residuals <- log(df_final$total_rate) - df_final$predicted
df_final$below_frontier <- df_final$residuals < 0

n_below <- sum(df_final$below_frontier)
n_total <- nrow(df_final)
pct_below <- n_below / n_total * 100

cat(sprintf("\nResidual diagnostics:\n  Total: %d, Below frontier: %d (%.1f%%)\n",
            n_total, n_below, pct_below))

# LOO cross-validation
cat("\n=== Leave-one-out validation ===\n")
beta_full <- coef(qr_model)
max_change <- 0; max_country <- ""
for (i in seq_len(nrow(df_final))) {
  qr_loo <- rq(log(total_rate) ~ ns(sdi_value, df = 3), tau = 0.05, data = df_final[-i, ])
  pct_change <- max(abs((coef(qr_loo) - beta_full) / beta_full) * 100)
  if (pct_change > max_change) {
    max_change <- pct_change; max_country <- df_final$location_name[i]
  }
}
cat(sprintf("  Max beta change: %.1f%% (removing %s) — %s\n",
            max_change, max_country, ifelse(max_change < 10, "PASS", "WARNING")))

# Plots
plot_a <- ggplot(df_final, aes(x = sdi_value, y = residuals)) +
  geom_point(aes(color = below_frontier), alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_smooth(method = "loess", color = "grey40", se = FALSE, linewidth = 0.8) +
  scale_color_manual(values = c("FALSE" = "#636363", "TRUE" = "#E31A1C"),
                     labels = c("Above frontier", "Below frontier"), name = "") +
  labs(title = "A", x = "SDI",
       y = "Residual (Observed \u2013 Predicted log ASMR)") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0), legend.position = "bottom",
        panel.grid.minor = element_blank())

plot_b <- ggplot(df_final, aes(x = predicted, y = residuals)) +
  geom_point(aes(color = below_frontier), alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_smooth(method = "loess", color = "grey40", se = FALSE, linewidth = 0.8) +
  scale_color_manual(values = c("FALSE" = "#636363", "TRUE" = "#E31A1C"),
                     labels = c("Above frontier", "Below frontier"), name = "") +
  labs(title = "B", x = "Predicted log ASMR",
       y = "Residual (Observed \u2013 Predicted log ASMR)") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0), legend.position = "bottom",
        panel.grid.minor = element_blank())

final_s5 <- (plot_a | plot_b) +
  plot_annotation(
    theme = theme(plot.margin = margin(10, 10, 10, 10))
  ) & theme(legend.position = "bottom")

ggsave(file.path(output_dir, "Figure_S1_residual.png"), final_s5, width = 14, height = 7, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "Figure_S1_residual.pdf"), final_s5, width = 14, height = 7, dpi = 300, bg = "white")
cat("Figure S1 saved to:", output_dir, "\n")
