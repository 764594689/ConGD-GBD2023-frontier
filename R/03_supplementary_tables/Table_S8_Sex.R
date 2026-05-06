# ==============================================================================
# Table S10: Sex-Stratified ASMR (GBD 2023)
# ==============================================================================
library(tidyverse)
library(flextable)
library(officer)

cat("--- Table S10: Sex-Stratified Analysis (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_sex   <- file.path(base_dir, "data/gbd2023_sex_stratified_1990_2023.csv.zip")
output_dir <- file.path(base_dir, "outputs/tables")
save_word_path <- file.path(output_dir, "Table_S8_Sex.docx")

if (!file.exists(path_sex)) stop("Sex-stratified data not found: ", path_sex)

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
  mutate(across(where(is.character), str_trim)) %>%
  filter(measure_name == "Deaths", sex_name %in% c("Male", "Female"),
         metric_name == "Rate", location_name == "Global")

# Use latest year available
latest_year <- max(df_base$year)
df_base <- df_base %>% filter(year == latest_year)

cat(sprintf("Using year: %d\n", latest_year))

# By cause
df_by_cause <- df_base %>%
  filter(cause_name %in% genetic_list) %>%
  group_by(cause_name, sex_name) %>%
  summarise(val = first(val), lower = first(lower), upper = first(upper), .groups = "drop") %>%
  mutate(formatted = paste0(
    formatC(round(val, 2), format = "f", digits = 2), " (",
    formatC(round(lower, 2), format = "f", digits = 2), "\u2013",
    formatC(round(upper, 2), format = "f", digits = 2), ")"))

# Composite
df_composite <- df_by_cause %>%
  group_by(sex_name) %>%
  summarise(val = sum(val), lower = sum(lower), upper = sum(upper), .groups = "drop") %>%
  mutate(cause_name = "All ConGDs (composite)",
         formatted = paste0(formatC(round(val, 2), format = "f", digits = 2), " (",
                            formatC(round(lower, 2), format = "f", digits = 2), "\u2013",
                            formatC(round(upper, 2), format = "f", digits = 2), ")"))

df_all <- bind_rows(df_composite, df_by_cause)

df_wide <- df_all %>%
  select(cause_name, sex_name, val, formatted) %>%
  pivot_wider(names_from = sex_name, values_from = c(val, formatted), names_sep = "_") %>%
  mutate(MF_ratio = round(val_Male / val_Female, 2)) %>%
  select(Condition = cause_name, Male_ASMR = formatted_Male,
         Female_ASMR = formatted_Female, MF_Ratio = MF_ratio) %>%
  mutate(Condition = factor(Condition, levels = c("All ConGDs (composite)", genetic_list))) %>%
  arrange(Condition)

cat("\n=== Sex-Stratified ASMR ===\n")
print(as.data.frame(df_wide))

ft <- df_wide %>% flextable() %>%
  set_header_labels(Male_ASMR = "Male ASMR (95% UI)",
                    Female_ASMR = "Female ASMR (95% UI)",
                    MF_Ratio = "M:F Ratio") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>% bold(part = "header") %>%
  bold(i = 1, part = "body") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  border_remove() %>%
  hline_top(border = fp_border_default(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1.5), part = "body") %>%
  hline(i = 1, border = fp_border_default(width = 0.5), part = "body") %>%
  padding(padding = 4, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  set_caption(sprintf("Table S8. Sex-stratified ASMR for ConGDs (%d).", latest_year)) %>%
  add_footer_lines("G6PD deficiency is X-linked; M:F ratio >1 = male excess.")

save_as_docx(ft, path = save_word_path)
cat("Table S10 saved:", save_word_path, "\n")
