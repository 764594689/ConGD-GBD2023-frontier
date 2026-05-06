# ==============================================================================
# Supplementary Table S3: Age-Specific Mortality by Cause (2023)
# Data: gbd2023_age_specific_2023.csv.zip
# ==============================================================================
library(tidyverse)
library(flextable)
library(officer)

cat("--- Table S3: Age-Specific Mortality (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_age   <- file.path(base_dir, "data/gbd2023_age_specific_2023.csv.zip")
output_dir <- file.path(base_dir, "outputs/tables")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- Disease list ---
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

# --- Read ---
df_raw <- read_csv(path_age, show_col_types = FALSE)

# Age mapping
age_order <- c("0-6 days", "7-27 days", "1-5 months", "6-11 months",
               "12-23 months", "2-4 years")
age_labels <- c("Early Neonatal\n(0\u20136 days)", "Late Neonatal\n(7\u201327 days)",
                "1\u20135 months", "6\u201311 months", "12\u201323 months", "2\u20134 years")

df_base <- df_raw %>%
  filter(measure_name == "Deaths", metric_name == "Number",
         sex_name == "Both", cause_name %in% genetic_list,
         age_name %in% age_order) %>%
  mutate(age_name = factor(age_name, levels = age_order))

# --- Wide format: causes as rows, age groups as columns ---
df_table <- df_base %>%
  mutate(deaths_fmt = paste0(
    formatC(round(val), format = "d", big.mark = ","),
    " (", formatC(round(lower), format = "d", big.mark = ","),
    "\u2013", formatC(round(upper), format = "d", big.mark = ","), ")"
  )) %>%
  select(cause_name, age_name, deaths_fmt) %>%
  pivot_wider(names_from = age_name, values_from = deaths_fmt) %>%
  mutate(Category = ifelse(cause_name %in% structural_list,
                           "Structural", "Hemoglobinopathy")) %>%
  arrange(Category, cause_name) %>%
  select(Category, Cause = cause_name, all_of(age_order))

# Add subtotals
add_subtotal <- function(df, cat_name, causes) {
  sub <- df_base %>%
    filter(cause_name %in% causes) %>%
    group_by(age_name) %>%
    summarise(val = sum(val), lower = sum(lower), upper = sum(upper), .groups = "drop") %>%
    mutate(deaths_fmt = paste0(
      formatC(round(val), format = "d", big.mark = ","),
      " (", formatC(round(lower), format = "d", big.mark = ","),
      "\u2013", formatC(round(upper), format = "d", big.mark = ","), ")"
    )) %>%
    select(age_name, deaths_fmt) %>%
    pivot_wider(names_from = age_name, values_from = deaths_fmt) %>%
    mutate(Category = cat_name, Cause = paste0(cat_name, " Total")) %>%
    select(Category, Cause, all_of(age_order))
  sub
}

subtotal_struct <- add_subtotal(df_table, "Structural", structural_list)
subtotal_hemo   <- add_subtotal(df_table, "Hemoglobinopathy", hemoglobin_list)
subtotal_all    <- add_subtotal(df_table, "All ConGDs", genetic_list) %>%
  mutate(Category = "Total")

df_final <- bind_rows(
  filter(df_table, Category == "Structural"),
  subtotal_struct,
  filter(df_table, Category == "Hemoglobinopathy"),
  subtotal_hemo,
  subtotal_all
)

# --- Flextable ---
ft_s3 <- df_final %>%
  flextable() %>%
  merge_v(j = "Category") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  bold(part = "header") %>%
  bold(i = ~ grepl("Total", Cause)) %>%
  align(align = "center", part = "header") %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 3:8, align = "center", part = "body") %>%
  valign(j = 1, valign = "top", part = "body") %>%
  border_remove() %>%
  border_inner(border = fp_border_default(width = 0, color = "white"), part = "all") %>%
  border_outer(border = fp_border_default(width = 0, color = "white"), part = "all") %>%
  hline_top(border = fp_border_default(width = 1.5, color = "black"), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1, color = "black"), part = "header") %>%
  hline(i = ~ grepl("Total", Cause),
        border = fp_border_default(width = 0.75, color = "black"),
        part = "body") %>%
  hline(i = nrow(df_final),
        border = fp_border_default(width = 1.5, color = "black"),
        part = "body") %>%
  fix_border_issues() %>%
  padding(padding = 3, part = "all") %>%
  width(j = 1, width = 0.7) %>%
  width(j = 2, width = 1.1) %>%
  width(j = 3:8, width = 0.85) %>%
  fontsize(size = 7, part = "all") %>%
  set_table_properties(layout = "fixed") %>%
  set_caption(paste0(
    "Table S4. Age-specific mortality (deaths, 95% UI) for congenital and genetic disorders ",
    "in children under 5 years, Global, 2023."
  )) %>%
  add_footer_lines("UI, uncertainty interval. Deaths shown as Number (95% UI).")

save_as_docx(ft_s3, path = file.path(output_dir, "Table_S4_Age_Specific.docx"),
             pr_section = prop_section(
               page_size = page_size(orient = "portrait"),
               page_margins = page_mar(bottom = 0.5, top = 0.5, right = 0.4, left = 0.4)
             ))

cat("Table S3 saved.\n")
