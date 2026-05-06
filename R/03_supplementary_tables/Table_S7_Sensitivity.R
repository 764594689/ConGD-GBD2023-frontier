# ==============================================================================
# Table S9: Sensitivity — With vs Without Hemoglobinopathies (GBD 2023)
# ==============================================================================
library(tidyverse)
library(flextable)
library(officer)

cat("--- Table S9: Hemoglobinopathy Sensitivity (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_country <- file.path(base_dir, "data/gbd2023_sdi_country_2023.csv.zip")
path_sdi     <- file.path(base_dir, "data/gbd2023_sdi_values_1950_2023.csv")
output_dir <- file.path(base_dir, "outputs/tables")
save_word_path <- file.path(output_dir, "Table_S7_Sensitivity.docx")

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
all_13 <- c(structural_list, hemoglobin_list)

df_raw <- read_csv(path_country, show_col_types = FALSE)

df_base <- df_raw %>%
  filter(year == 2023, age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths")

# Assign SDI groups
df_sdi <- read_csv(path_sdi, show_col_types = FALSE) %>%
  filter(year_id == 2023) %>%
  select(location_name, sdi = mean_value) %>%
  mutate(sdi_group = case_when(
    sdi <= 0.454 ~ "Low SDI", sdi <= 0.608 ~ "Low-middle SDI",
    sdi <= 0.701 ~ "Middle SDI", sdi <= 0.813 ~ "High-middle SDI",
    TRUE ~ "High SDI"
  ))

# All causes
df_all <- df_base %>%
  filter(cause_name == "All causes", metric_name == "Number") %>%
  select(location_name, all_deaths = val)

# Function to compute burden
calc_burden <- function(causes, label) {
  rate <- df_base %>%
    filter(cause_name %in% causes, metric_name == "Rate") %>%
    group_by(location_name) %>%
    summarise(asmr = sum(val, na.rm = TRUE), .groups = "drop")
  num <- df_base %>%
    filter(cause_name %in% causes, metric_name == "Number") %>%
    group_by(location_name) %>%
    summarise(deaths = sum(val, na.rm = TRUE), .groups = "drop")
  rate %>%
    left_join(num, by = "location_name") %>%
    left_join(df_all, by = "location_name") %>%
    left_join(df_sdi %>% select(location_name, sdi_group), by = "location_name") %>%
    mutate(pmr = deaths / all_deaths * 100, group = label)
}

df_all13 <- calc_burden(all_13, "All 13")
df_struct <- calc_burden(structural_list, "Structural only")

# Aggregate by SDI group + Global
sdi_levels <- c("Global", "High SDI", "High-middle SDI", "Middle SDI",
                "Low-middle SDI", "Low SDI")

agg <- function(data) {
  by_sdi <- data %>%
    filter(!is.na(sdi_group)) %>%
    group_by(sdi_group) %>%
    summarise(asmr = weighted.mean(asmr, deaths, na.rm = TRUE),
              deaths = sum(deaths, na.rm = TRUE),
              all_deaths = sum(all_deaths, na.rm = TRUE), .groups = "drop") %>%
    mutate(pmr = deaths / all_deaths * 100) %>%
    rename(Location = sdi_group)
  global <- tibble(
    Location = "Global",
    asmr = weighted.mean(data$asmr, data$deaths, na.rm = TRUE),
    deaths = sum(data$deaths, na.rm = TRUE),
    all_deaths = sum(data$all_deaths, na.rm = TRUE)
  ) %>% mutate(pmr = deaths / all_deaths * 100)
  bind_rows(global, by_sdi)
}

df_a <- agg(df_all13) %>% rename(asmr_all = asmr, pmr_all = pmr) %>% select(Location, asmr_all, pmr_all)
df_s <- agg(df_struct) %>% rename(asmr_struct = asmr, pmr_struct = pmr) %>% select(Location, asmr_struct, pmr_struct)

df_compare <- df_a %>%
  left_join(df_s, by = "Location") %>%
  mutate(
    diff_asmr = asmr_all - asmr_struct,
    pct_asmr = diff_asmr / asmr_struct * 100,
    diff_pmr = pmr_all - pmr_struct,
    Location = factor(Location, levels = sdi_levels)
  ) %>%
  arrange(Location)

cat("\n=== Sensitivity Results ===\n")
print(as.data.frame(df_compare %>% mutate(across(where(is.numeric), ~round(., 2)))))

df_table <- df_compare %>%
  mutate(
    ASMR_all = formatC(round(asmr_all, 2), format = "f", digits = 2),
    ASMR_struct = formatC(round(asmr_struct, 2), format = "f", digits = 2),
    ASMR_diff = formatC(round(diff_asmr, 2), format = "f", digits = 2),
    ASMR_pct = paste0(formatC(round(pct_asmr, 1), format = "f", digits = 1), "%"),
    PMR_all = paste0(formatC(round(pmr_all, 2), format = "f", digits = 2), "%"),
    PMR_struct = paste0(formatC(round(pmr_struct, 2), format = "f", digits = 2), "%"),
    PMR_diff = paste0(formatC(round(diff_pmr, 2), format = "f", digits = 2), " pp")
  ) %>%
  select(Location, ASMR_all, ASMR_struct, ASMR_diff, ASMR_pct,
         PMR_all, PMR_struct, PMR_diff)

ft <- df_table %>% flextable() %>%
  set_header_labels(ASMR_all = "All 13", ASMR_struct = "Structural (9)",
                    ASMR_diff = "Diff", ASMR_pct = "Diff %",
                    PMR_all = "All 13", PMR_struct = "Structural (9)",
                    PMR_diff = "Diff (pp)") %>%
  add_header_row(values = c("", "ASMR (per 100,000)", "PMR"), colwidths = c(1, 4, 3)) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>% bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  border_remove() %>%
  hline_top(border = fp_border_default(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1.5), part = "body") %>%
  padding(padding = 4, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  set_caption("Table S7. Sensitivity: with vs. without hemoglobinopathies (2023).") %>%
  add_footer_lines("pp = percentage points.")

save_as_docx(ft, path = save_word_path)
cat("Table S9 saved:", save_word_path, "\n")
