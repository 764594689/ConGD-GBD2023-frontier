# ==============================================================================
# Table S7: Frontier Model Diagnostics (GBD 2023)
# ==============================================================================
library(tidyverse)
library(quantreg)
library(splines)
library(flextable)
library(officer)

cat("--- Table S7: Frontier Diagnostics (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_country <- file.path(base_dir, "data/gbd2023_sdi_country_2023.csv.zip")
path_sdi     <- file.path(base_dir, "data/gbd2023_sdi_values_1950_2023.csv")
output_dir <- file.path(base_dir, "outputs/tables")
save_word_path <- file.path(output_dir, "Table_S5_Frontier.docx")

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
  drop_na(sdi_value, total_rate) %>% filter(total_rate > 0)

qr_05 <- rq(log(total_rate) ~ ns(sdi_value, df = 3), tau = 0.05, data = df_final)
qr_10 <- rq(log(total_rate) ~ ns(sdi_value, df = 3), tau = 0.10, data = df_final)

extract_diag <- function(model, data) {
  rho_tau <- function(u, tau) u * (tau - (u < 0))
  r1 <- sum(rho_tau(model$residuals, model$tau))
  y_log <- log(data$total_rate)
  r0 <- sum(rho_tau(y_log - quantile(y_log, model$tau), model$tau))
  pseudo_r2 <- 1 - r1 / r0
  fitted <- exp(predict(model, data))
  coverage <- sum(data$total_rate < fitted) / nrow(data) * 100
  knots <- attr(ns(data$sdi_value, df = 3), "knots")
  boundary <- attr(ns(data$sdi_value, df = 3), "Boundary.knots")
  list(pseudo_r2 = pseudo_r2, coverage = coverage,
       knots = paste(round(knots, 3), collapse = ", "),
       boundary = paste0("[", round(boundary[1], 3), ", ", round(boundary[2], 3), "]"))
}

d05 <- extract_diag(qr_05, df_final)
d10 <- extract_diag(qr_10, df_final)

cat(sprintf("τ = 0.05: R²=%.4f, Coverage=%.1f%%\n", d05$pseudo_r2, d05$coverage))
cat(sprintf("τ = 0.10: R²=%.4f, Coverage=%.1f%%\n", d10$pseudo_r2, d10$coverage))

df_table <- tibble(
  Parameter = c("Pseudo-R\u00B2", "Empirical coverage (%)", "Interior knots (SDI)", "Boundary knots (SDI)"),
  `τ = 0.05` = c(formatC(d05$pseudo_r2, format = "f", digits = 4),
                   paste0(formatC(d05$coverage, format = "f", digits = 1), "%"),
                   d05$knots, d05$boundary),
  `τ = 0.10` = c(formatC(d10$pseudo_r2, format = "f", digits = 4),
                   paste0(formatC(d10$coverage, format = "f", digits = 1), "%"),
                   d10$knots, d10$boundary)
)

ft <- df_table %>% flextable() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>% bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  border_remove() %>%
  hline_top(border = fp_border_default(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1.5), part = "body") %>%
  padding(padding = 5, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  set_caption("Table S5. Frontier Model Diagnostics (2023).")

# Apply italic formatting to statistical symbols (R², τ)
ft <- ft %>%
  compose(j = "Parameter", i = 1, part = "body",
          value = as_paragraph("Pseudo-", as_i("R\u00B2"))) %>%
  compose(j = "\u03C4 = 0.05", part = "header",
          value = as_paragraph(as_i("\u03C4"), " = 0.05")) %>%
  compose(j = "\u03C4 = 0.10", part = "header",
          value = as_paragraph(as_i("\u03C4"), " = 0.10"))

save_as_docx(ft, path = save_word_path)
cat("Table S7 saved:", save_word_path, "\n")
