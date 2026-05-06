# ==============================================================================
# Figure 3: SDI Correlation (GBD 2023)
# Panel A: ASMR vs SDI (Spearman rho)
# Panel B: PMR vs SDI (compositional artifact)
# ==============================================================================
library(tidyverse)
library(patchwork)
library(ggrepel)

cat("--- Figure 3: SDI Correlation (GBD 2023) ---\n")

# --- Paths ---
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_country <- file.path(base_dir, "data/gbd2023_sdi_country_2023.csv.zip")
path_sdi     <- file.path(base_dir, "data/gbd2023_sdi_values_1950_2023.csv")
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

# --- Read data ---
cat("Reading country-level data...\n")
df_country <- read_csv(path_country, show_col_types = FALSE)

cat("Reading SDI data...\n")
df_sdi_raw <- read_csv(path_sdi, show_col_types = FALSE)

# --- Process country data (2023, <5, Both, Deaths) ---
df_base <- df_country %>%
  filter(year == 2023, age_name == "<5 years",
         sex_name == "Both", measure_name == "Deaths")

df_genetic_rate <- df_base %>%
  filter(cause_name %in% genetic_list, metric_name == "Rate") %>%
  group_by(location_name) %>%
  summarise(total_rate = sum(val, na.rm = TRUE), .groups = "drop")

df_genetic_num <- df_base %>%
  filter(cause_name %in% genetic_list, metric_name == "Number") %>%
  group_by(location_name) %>%
  summarise(genetic_deaths = sum(val, na.rm = TRUE), .groups = "drop")

df_all_num <- df_base %>%
  filter(cause_name == "All causes", metric_name == "Number") %>%
  select(location_name, all_deaths = val)

df_country_metrics <- df_genetic_rate %>%
  left_join(df_genetic_num, by = "location_name") %>%
  left_join(df_all_num, by = "location_name") %>%
  mutate(pmr_percent = (genetic_deaths / all_deaths) * 100)

# --- SDI data ---
df_sdi <- df_sdi_raw %>%
  filter(year_id == 2023) %>%
  select(location_name, sdi = mean_value)

# --- Merge + SDI groups ---
df_final <- df_country_metrics %>%
  left_join(df_sdi, by = "location_name") %>%
  filter(!is.na(sdi), !is.na(total_rate), !is.na(pmr_percent)) %>%
  mutate(
    sdi_group = case_when(
      sdi < 0.46               ~ "Low SDI",
      sdi >= 0.46 & sdi < 0.60 ~ "Low-middle SDI",
      sdi >= 0.60 & sdi < 0.69 ~ "Middle SDI",
      sdi >= 0.69 & sdi < 0.81 ~ "High-middle SDI",
      sdi >= 0.81              ~ "High SDI"
    ),
    sdi_group = factor(sdi_group, levels = c(
      "Low SDI", "Low-middle SDI", "Middle SDI", "High-middle SDI", "High SDI"
    ))
  )

cat(sprintf("Merged: %d countries\n", nrow(df_final)))

# --- Plot function ---
draw_sdi_plot <- function(data, y_var, title, y_lab, cor_pos_x, cor_pos_y, y_limit_max) {
  res <- cor.test(data$sdi, data[[y_var]], method = "spearman", exact = FALSE)
  rho_val <- sprintf("%.2f", res$estimate)
  label_expr <- paste0("italic(rho) == '", rho_val, "' * ',' ~ italic(P) < 0.001")

  ggplot(data, aes(x = sdi, y = !!sym(y_var))) +
    geom_point(aes(fill = sdi_group), shape = 21, size = 3, alpha = 0.7, color = "grey30") +
    geom_smooth(method = "loess", color = "black", fill = "grey85", alpha = 0.3, linewidth = 1.2) +
    scale_fill_manual(values = c(
      "Low SDI" = "#D7191C", "Low-middle SDI" = "#FDAE61", "Middle SDI" = "#FFFFBF",
      "High-middle SDI" = "#ABD9E9", "High SDI" = "#2C7BB6"
    )) +
    geom_text_repel(
      data = data %>% arrange(desc(!!sym(y_var))) %>% head(8),
      aes(label = location_name),
      size = 3.8, fontface = "italic", force = 35, max.overlaps = 20,
      segment.color = "grey50", segment.size = 0.3, box.padding = 1.0, point.padding = 0.5
    ) +
    annotate("text", x = cor_pos_x, y = cor_pos_y,
             label = label_expr, parse = TRUE,
             size = 5.5, hjust = 0, fontface = "bold") +
    scale_y_continuous(limits = c(-5, y_limit_max), expand = c(0, 0)) +
    labs(title = title, x = "Socio-demographic Index (SDI)", y = y_lab, fill = "SDI Group") +
    theme_bw(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 14, hjust = 0),
      legend.position  = "none",
      axis.title       = element_text(face = "bold")
    )
}

# --- Draw ---
p3a <- draw_sdi_plot(df_final, "total_rate",
                     "A",
                     "ASMR (per 100,000)",
                     cor_pos_x = 0.65, cor_pos_y = 220, y_limit_max = 250)

p3b <- draw_sdi_plot(df_final, "pmr_percent",
                     "B",
                     "Proportional Mortality Ratio (%)",
                     cor_pos_x = 0.05, cor_pos_y = 52, y_limit_max = 60)

final_fig3 <- (p3a / p3b) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"))

ggsave(file.path(output_dir, "Figure3_SDI_Correlation.png"),
       final_fig3, width = 10, height = 13, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "Figure3_SDI_Correlation.pdf"),
       final_fig3, width = 10, height = 13, dpi = 300, bg = "white")

# --- Console output ---
rho_asmr <- cor.test(df_final$sdi, df_final$total_rate, method = "spearman", exact = FALSE)
rho_pmr  <- cor.test(df_final$sdi, df_final$pmr_percent, method = "spearman", exact = FALSE)

cat("\n=== Figure 3 Manuscript Values ===\n")
cat(sprintf("  Panel A - ASMR vs SDI: Spearman rho = %.2f, P = %.2e\n",
            rho_asmr$estimate, rho_asmr$p.value))
cat(sprintf("  Panel B - PMR vs SDI:  Spearman rho = %.2f, P = %.2e\n",
            rho_pmr$estimate, rho_pmr$p.value))
cat("\nFigure 3 saved to:", output_dir, "\n")
