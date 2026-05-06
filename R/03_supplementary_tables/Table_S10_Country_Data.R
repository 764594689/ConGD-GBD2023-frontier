# ==============================================================================
# Supplementary Table S2: 204-Country ConGD Mortality Burden (2023)
# Data: gbd2023_country_2023.csv.zip
# ==============================================================================
library(tidyverse)
library(flextable)
library(officer)

cat("--- Table S2: Country-Level Data (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_country <- file.path(base_dir, "data/gbd2023_country_2023.csv.zip")
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

# --- Read data ---
df_raw <- read_csv(path_country, show_col_types = FALSE)
# Normalize country-name apostrophes (straight ' -> smart ’)
if ("location_name" %in% colnames(df_raw)) {
  df_raw <- df_raw %>% mutate(location_name = gsub("'", "’", location_name, fixed = TRUE))
}


df_base <- df_raw %>%
  filter(year == 2023, age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths")

# CGD Rate per country
df_rate <- df_base %>%
  filter(cause_name %in% genetic_list, metric_name == "Rate") %>%
  group_by(location_name) %>%
  summarise(cgd_rate = sum(val, na.rm = TRUE),
            cgd_rate_lower = sum(lower, na.rm = TRUE),
            cgd_rate_upper = sum(upper, na.rm = TRUE), .groups = "drop")

# CGD Number per country
df_num <- df_base %>%
  filter(cause_name %in% genetic_list, metric_name == "Number") %>%
  group_by(location_name) %>%
  summarise(cgd_deaths = sum(val, na.rm = TRUE), .groups = "drop")

# All causes Number
df_all <- df_base %>%
  filter(cause_name == "All causes", metric_name == "Number") %>%
  select(location_name, all_deaths = val)

# Merge and compute PMR
df_country <- df_rate %>%
  left_join(df_num, by = "location_name") %>%
  left_join(df_all, by = "location_name") %>%
  mutate(pmr = cgd_deaths / all_deaths * 100) %>%
  arrange(desc(cgd_rate))

# Format for display
df_display <- df_country %>%
  mutate(
    `ASMR (95% UI)` = paste0(
      formatC(round(cgd_rate, 2), format = "f", digits = 2), " (",
      formatC(round(cgd_rate_lower, 2), format = "f", digits = 2), "\u2013",
      formatC(round(cgd_rate_upper, 2), format = "f", digits = 2), ")"),
    `Deaths (N)` = formatC(round(cgd_deaths), format = "d", big.mark = ","),
    `PMR (%)` = formatC(round(pmr, 2), format = "f", digits = 2)
  ) %>%
  select(Country = location_name, `ASMR (95% UI)`, `Deaths (N)`, `PMR (%)`)

# --- Save CSV ---
write_csv(df_country, file.path(output_dir, "Table_S2_Country_Data.csv"))

# --- Flextable ---
ft_s2 <- df_display %>%
  flextable() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  bold(part = "header") %>%
  align(align = "center", part = "header") %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:4, align = "center", part = "body") %>%
  border_remove() %>%
  hline_top(border = fp_border_default(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1.5), part = "body") %>%
  padding(padding = 3, part = "all") %>%
  width(j = 1, width = 2.0) %>%
  width(j = 2, width = 2.2) %>%
  width(j = 3, width = 1.2) %>%
  width(j = 4, width = 0.8) %>%
  set_table_properties(layout = "fixed") %>%
  set_caption(paste0(
    "Table S10. Country-level congenital and genetic disorder mortality burden ",
    "in children under 5 years across 204 countries, 2023."
  )) %>%
  add_footer_lines(paste0(
    "ASMR, age-standardized mortality rate per 100,000; PMR, proportional mortality ratio; ",
    "UI, uncertainty interval. Countries ranked by ASMR (descending)."
  ))

save_as_docx(ft_s2, path = file.path(output_dir, "Table_S10_Country_Data.docx"),
             pr_section = prop_section(
               page_size = page_size(orient = "portrait"),
               page_margins = page_mar(bottom = 0.8, top = 0.8, right = 0.6, left = 0.6)
             ))

cat(sprintf("Table S2 saved. %d countries.\n", nrow(df_display)))
