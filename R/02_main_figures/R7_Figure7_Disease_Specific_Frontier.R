# ==============================================================================
# Figure 7: Disease-Specific Frontier Analysis (promoted from Figure S3)
# Panel A: Structural Birth Defects only
# Panel B: Hemoglobinopathies only
# ==============================================================================
library(tidyverse)
library(quantreg)
library(ggrepel)
library(splines)
library(patchwork)

cat("--- Figure 7: Disease-Specific Frontier (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_country <- file.path(base_dir, "data/gbd2023_sdi_country_2023.csv.zip")
path_sdi     <- file.path(base_dir, "data/gbd2023_sdi_values_1950_2023.csv")
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

df_raw <- read_csv(path_country, show_col_types = FALSE) %>%
  filter(year == 2023, age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths", metric_name == "Rate")

df_sdi <- read_csv(path_sdi, show_col_types = FALSE) %>%
  filter(year_id == 2023) %>%
  select(location_name, sdi_value = mean_value) %>%
  mutate(sdi_group = case_when(
    sdi_value <= 0.454 ~ "Low SDI", sdi_value <= 0.608 ~ "Low-middle SDI",
    sdi_value <= 0.701 ~ "Middle SDI", sdi_value <= 0.813 ~ "High-middle SDI",
    TRUE ~ "High SDI"
  ) %>% factor(levels = c("Low SDI", "Low-middle SDI", "Middle SDI",
                           "High-middle SDI", "High SDI")))

make_group <- function(causes, label) {
  df_raw %>%
    filter(cause_name %in% causes) %>%
    group_by(location_name) %>%
    summarise(rate = sum(val, na.rm = TRUE), .groups = "drop") %>%
    inner_join(df_sdi, by = "location_name") %>%
    filter(rate > 0) %>%
    mutate(group = label)
}

df_struct <- make_group(structural_list, "Structural Birth Defects")
df_hemo   <- make_group(hemoglobin_list, "Hemoglobinopathies")

sdi_pal <- c("Low SDI" = "#D7191C", "Low-middle SDI" = "#FDAE61",
             "Middle SDI" = "#FFFFBF", "High-middle SDI" = "#ABD9E9",
             "High SDI" = "#2C7BB6")

fit_frontier <- function(data) {
  qr_mod <- rq(log(rate) ~ ns(sdi_value, df = 3), tau = 0.05, data = data)
  grid <- data.frame(sdi_value = seq(min(data$sdi_value), max(data$sdi_value), length.out = 200))
  grid$frontier <- exp(predict(qr_mod, newdata = grid))
  data$frontier_pred <- exp(predict(qr_mod, newdata = data))
  data$gap <- pmax(data$rate - data$frontier_pred, 0)
  list(data = data, grid = grid)
}

fit_struct <- fit_frontier(df_struct)
fit_hemo   <- fit_frontier(df_hemo)

# Label top-5 gap countries in each panel
lab_struct <- fit_struct$data %>% arrange(desc(gap)) %>% head(5)
lab_hemo   <- fit_hemo$data   %>% arrange(desc(gap)) %>% head(5)

plot_panel <- function(fit, labdata, panel_title) {
  ggplot(fit$data, aes(x = sdi_value, y = rate)) +
    geom_point(aes(fill = sdi_group), color = "white", shape = 21,
               alpha = 0.75, size = 4.5, stroke = 0.5) +
    geom_line(data = fit$grid, aes(x = sdi_value, y = frontier),
              color = "#2CA25F", linewidth = 1.6, inherit.aes = FALSE) +
    geom_text_repel(data = labdata, aes(label = location_name),
                    size = 5.5, fontface = "bold", color = "grey20",
                    box.padding = 0.5, max.overlaps = Inf,
                    min.segment.length = 0, seed = 42) +
    scale_fill_manual(name = "SDI Quintile", values = sdi_pal) +
    scale_y_continuous(limits = c(0, max(fit$data$rate) * 1.1), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0.25, 0.95)) +
    labs(title = panel_title, x = "SDI", y = "ASMR (per 100,000)") +
    theme_minimal(base_size = 22) +
    theme(plot.title = element_text(face = "bold", size = 26, hjust = 0),
          legend.position = "bottom", panel.grid.minor = element_blank(),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 17, face = "bold"),
          axis.title = element_text(face = "bold", size = 16),
          axis.text = element_text(size = 14),
          axis.line = element_line(color = "black"))
}

p_a <- plot_panel(fit_struct, lab_struct, "A")
p_b <- plot_panel(fit_hemo,   lab_hemo,   "B")

final_f7 <- (p_a | p_b) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "Figure7.png"), final_f7, width = 16, height = 8, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "Figure7.pdf"), final_f7, width = 16, height = 8, dpi = 300, bg = "white")
cat("Figure 7 saved to:", output_dir, "\n")

# Summary of the two fitted frontiers for manuscript
summarise_fit <- function(fit, label) {
  cat(sprintf("\n%s:\n", label))
  cat(sprintf("  N countries: %d\n", nrow(fit$data)))
  preds <- exp(predict(rq(log(rate) ~ ns(sdi_value, df = 3), tau=0.05, data=fit$data),
                       newdata = data.frame(sdi_value = c(0.3, 0.5, 0.7, 0.9))))
  cat(sprintf("  Frontier at SDI=0.3: %.2f; SDI=0.5: %.2f; SDI=0.7: %.2f; SDI=0.9: %.2f\n",
              preds[1], preds[2], preds[3], preds[4]))
  cat(sprintf("  Top-5 gap countries: %s\n", paste(head(arrange(fit$data, desc(gap))$location_name,5), collapse=", ")))
}
summarise_fit(fit_struct, "STRUCTURAL")
summarise_fit(fit_hemo,   "HEMOGLOBINOPATHIES")
