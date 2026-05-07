# ==============================================================================
# Figure 6: Frontier Analysis (GBD 2023)
# Quantile regression with natural cubic splines
# + model diagnostics + algorithmic country selection
# + global avoidable deaths calculation
# ==============================================================================
library(tidyverse)
library(quantreg)
library(ggrepel)
library(splines)

cat("--- Figure 6: Frontier Analysis (GBD 2023) ---\n")

# --- Paths ---
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_country   <- file.path(base_dir, "data/gbd2023_sdi_country_2023.csv.zip")
path_sdi       <- file.path(base_dir, "data/gbd2023_sdi_values_1950_2023.csv")
output_dir <- file.path(base_dir, "outputs/figures")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- 13 ConGD causes ---
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

# --- Read and process mortality data ---
df_raw <- read_csv(path_country, show_col_types = FALSE)

df_mortality <- df_raw %>%
  filter(year == 2023, age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths", cause_name %in% genetic_list,
         metric_name == "Rate") %>%
  group_by(location_name) %>%
  summarise(total_rate = sum(val, na.rm = TRUE), .groups = "drop")

df_number <- df_raw %>%
  filter(year == 2023, age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths", cause_name %in% genetic_list,
         metric_name == "Number") %>%
  group_by(location_name) %>%
  summarise(total_deaths = sum(val, na.rm = TRUE), .groups = "drop")

# --- SDI data ---
df_sdi_raw <- read_csv(path_sdi, show_col_types = FALSE)

df_sdi <- df_sdi_raw %>%
  filter(year_id == 2023) %>%
  select(location_name, sdi_value = mean_value) %>%
  mutate(sdi_group = case_when(
    sdi_value <= 0.454 ~ "Low SDI",
    sdi_value <= 0.608 ~ "Low-middle SDI",
    sdi_value <= 0.701 ~ "Middle SDI",
    sdi_value <= 0.813 ~ "High-middle SDI",
    TRUE ~ "High SDI"
  )) %>%
  mutate(sdi_group = factor(sdi_group, levels = c(
    "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI")))

# --- Merge ---
df_final <- df_mortality %>%
  inner_join(df_sdi, by = "location_name") %>%
  left_join(df_number, by = "location_name") %>%
  drop_na(sdi_value, total_rate) %>%
  filter(total_rate > 0)

cat(sprintf("Countries in analysis: %d\n", nrow(df_final)))

# ==============================================================================
# Quantile regression — natural cubic splines (df = 3)
# ==============================================================================
qr_model_05 <- rq(log(total_rate) ~ ns(sdi_value, df = 3),
                   tau = 0.05, data = df_final)
qr_model_10 <- rq(log(total_rate) ~ ns(sdi_value, df = 3),
                   tau = 0.10, data = df_final)

df_final$log_frontier_05 <- predict(qr_model_05, df_final)
df_final$frontier_val    <- exp(df_final$log_frontier_05)

# Smooth frontier curve
sdi_grid <- data.frame(
  sdi_value = seq(min(df_final$sdi_value), max(df_final$sdi_value), length.out = 300)
)
sdi_grid$frontier_val <- exp(predict(qr_model_05, newdata = sdi_grid))

# Empirical coverage
n_below <- sum(df_final$total_rate < df_final$frontier_val)
cat(sprintf("\nCountries below frontier: %d / %d (%.1f%%)\n",
            n_below, nrow(df_final), n_below / nrow(df_final) * 100))

# Pseudo-R²
rho_tau <- function(u, tau) u * (tau - (u < 0))
resid_05 <- residuals(qr_model_05)
y_log <- log(df_final$total_rate)
pseudo_r2_05 <- 1 - sum(rho_tau(resid_05, 0.05)) /
  sum(rho_tau(y_log - quantile(y_log, 0.05), 0.05))

resid_10 <- residuals(qr_model_10)
pseudo_r2_10 <- 1 - sum(rho_tau(resid_10, 0.10)) /
  sum(rho_tau(y_log - quantile(y_log, 0.10), 0.10))

cat(sprintf("Pseudo-R² (τ = 0.05) = %.4f\n", pseudo_r2_05))
cat(sprintf("Pseudo-R² (tau=0.10) = %.4f\n", pseudo_r2_10))

# Coefficients
cat("\n=== Table S7: Frontier Regression Coefficients ===\n")
cat("--- tau = 0.05 ---\n")
print(summary(qr_model_05, se = "boot", R = 1000))
cat("\n--- tau = 0.10 ---\n")
print(summary(qr_model_10, se = "boot", R = 1000))

# ==============================================================================
# Efficiency gap calculation
# ==============================================================================
df_final <- df_final %>%
  mutate(
    frontier_deaths  = total_deaths * (frontier_val / total_rate),
    efficiency_gap   = total_rate - frontier_val,
    gap_pct          = efficiency_gap / total_rate * 100,
    avoidable_deaths = pmax(total_deaths - frontier_deaths, 0)
  )

global_avoidable <- sum(df_final$avoidable_deaths, na.rm = TRUE)
global_total     <- sum(df_final$total_deaths, na.rm = TRUE)
global_avoidable_pct <- global_avoidable / global_total * 100

cat(sprintf("\n=== Global Avoidable Deaths ===\n"))
cat(sprintf("  Total avoidable = %.0f\n", global_avoidable))
cat(sprintf("  Percent of ConGD deaths = %.1f%%\n", global_avoidable_pct))

# Top-10 / Bottom-10
cat("\n=== Top-10 (largest efficiency gaps) ===\n")
top10 <- df_final %>% arrange(desc(efficiency_gap)) %>% head(10) %>%
  select(location_name, sdi_group, sdi_value, total_rate, frontier_val, efficiency_gap, gap_pct)
print(as.data.frame(top10))

cat("\n=== Bottom-10 (nearest frontier) ===\n")
bottom10 <- df_final %>% arrange(efficiency_gap) %>% head(10) %>%
  select(location_name, sdi_group, sdi_value, total_rate, frontier_val, efficiency_gap, gap_pct)
print(as.data.frame(bottom10))

# ==============================================================================
# Algorithmic country selection
# ==============================================================================
select_countries <- function(data) {
  selected <- character(0)
  for (grp in levels(data$sdi_group)) {
    grp_data <- data %>% filter(sdi_group == grp)
    if (nrow(grp_data) > 0) {
      selected <- c(selected,
                    grp_data$location_name[which.max(grp_data$efficiency_gap)],
                    grp_data$location_name[which.min(grp_data$efficiency_gap)])
    }
  }
  top2 <- data %>% arrange(desc(efficiency_gap)) %>% head(2) %>% pull(location_name)
  bot2 <- data %>% arrange(efficiency_gap) %>% head(2) %>% pull(location_name)
  unique(c(selected, top2, bot2))
}

target_countries <- select_countries(df_final)
cat("\n=== Annotated countries ===\n")
cat(paste(target_countries, collapse = ", "), "\n")

# ==============================================================================
# Plot
# ==============================================================================
sdi_pal <- c("Low SDI" = "#D7191C", "Low-middle SDI" = "#FDAE61",
             "Middle SDI" = "#FFFFBF", "High-middle SDI" = "#ABD9E9",
             "High SDI" = "#2C7BB6")

plot_6 <- ggplot(df_final, aes(x = sdi_value, y = total_rate)) +
  geom_point(aes(fill = sdi_group), color = "white", shape = 21,
             alpha = 0.8, size = 2.5, stroke = 0.4) +
  geom_line(data = sdi_grid, aes(x = sdi_value, y = frontier_val),
            color = "#2CA25F", linewidth = 1.5, inherit.aes = FALSE) +
  geom_segment(data = filter(df_final, location_name %in% target_countries),
               aes(xend = sdi_value, yend = frontier_val),
               linetype = "dashed", color = "#E31A1C", linewidth = 0.6) +
  geom_point(data = filter(df_final, location_name %in% target_countries),
             color = "#E31A1C", size = 3.5) +
  geom_text_repel(data = filter(df_final, location_name %in% target_countries),
                  aes(label = location_name),
                  fontface = "bold", size = 4, box.padding = 0.8,
                  point.padding = 0.5, min.segment.length = 0,
                  seed = 42, color = "black", max.overlaps = Inf) +
  scale_fill_manual(name = "SDI Quintile", values = sdi_pal) +
  scale_x_continuous(breaks = seq(0, 1, 0.2),
                     limits = c(min(df_final$sdi_value) * 0.9,
                                max(df_final$sdi_value) * 1.05)) +
  scale_y_continuous(limits = c(0, max(df_final$total_rate) * 1.05), expand = c(0, 0)) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Socio-demographic Index (SDI)",
    y = "ASMR of Congenital and Genetic Disorders (per 100,000)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_blank(),
    plot.subtitle    = element_blank(),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.title       = element_text(face = "bold"),
    axis.line        = element_line(color = "black")
  )

ggsave(file.path(output_dir, "Figure6_Frontier.png"),
       plot_6, width = 12, height = 8.5, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "Figure6_Frontier.pdf"),
       plot_6, width = 12, height = 8.5, dpi = 300, bg = "white")

# Export CSV
write_csv(df_final %>%
            select(location_name, sdi_value, sdi_group, total_rate,
                   total_deaths, frontier_val, efficiency_gap, gap_pct,
                   avoidable_deaths) %>%
            arrange(desc(efficiency_gap)),
          file.path(output_dir, "frontier_results_all_countries.csv"))

cat("\nFigure 6 saved to:", output_dir, "\n")
