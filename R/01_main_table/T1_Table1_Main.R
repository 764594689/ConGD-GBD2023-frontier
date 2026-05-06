# ==============================================================================
# Table 1: Global and SDI-stratified ConGD mortality burden (GBD 2023)
# Uses trend data (Global + regions) and SDI quintile composition data
# Years: 1990 vs 2023
# ==============================================================================
library(tidyverse)
library(flextable)
library(officer)

cat("--- Table 1: Main Table (GBD 2023) ---\n")

# --- Paths ---
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_trend     <- file.path(base_dir, "data/gbd2023_trend_1990_2023.csv.zip")
path_sdi_q     <- file.path(base_dir, "data/gbd2023_sdi_quintile_composition.csv.zip")
# path_country and path_sdi_vals no longer needed — SDI quintile data now has Rate + All causes
dir_table1     <- file.path(base_dir, "outputs/tables")
if (!dir.exists(dir_table1)) dir.create(dir_table1, recursive = TRUE)
save_word_path <- file.path(dir_table1, "Table1_Main.docx")

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

# ==============================================================================
# Part 1: Global row from trend data
# ==============================================================================
df_trend <- read_csv(path_trend, show_col_types = FALSE)

df_global <- df_trend %>%
  filter(location_name == "Global", age_name == "<5 years",
         sex_name == "Both", measure_name == "Deaths",
         year %in% c(1990, 2023))

# CGD Rate
df_gen_global_rate <- df_global %>%
  filter(cause_name %in% genetic_list, metric_name == "Rate") %>%
  group_by(year) %>%
  summarise(cgd_rate = sum(val, na.rm = TRUE),
            cgd_rate_lower = sum(lower, na.rm = TRUE),
            cgd_rate_upper = sum(upper, na.rm = TRUE), .groups = "drop")

# CGD Number
df_gen_global_num <- df_global %>%
  filter(cause_name %in% genetic_list, metric_name == "Number") %>%
  group_by(year) %>%
  summarise(cgd_deaths = sum(val, na.rm = TRUE), .groups = "drop")

# All causes Number
df_all_global <- df_global %>%
  filter(cause_name == "All causes", metric_name == "Number") %>%
  select(year, all_deaths = val)

# CMNN Rate
df_cmnn_global <- df_global %>%
  filter(str_detect(cause_name, "Communicable, maternal"), metric_name == "Rate") %>%
  select(year, cmnn_rate = val)

df_global_calc <- df_gen_global_rate %>%
  left_join(df_gen_global_num, by = "year") %>%
  left_join(df_all_global, by = "year") %>%
  left_join(df_cmnn_global, by = "year") %>%
  mutate(pmr = cgd_deaths / all_deaths * 100,
         location_name = "Global")

# ==============================================================================
# Part 2: SDI quintile rows from composition data (now has Rate + All causes)
# ==============================================================================
df_sdi_q <- read_csv(path_sdi_q, show_col_types = FALSE)

sdi_levels <- c("High SDI", "High-middle SDI", "Middle SDI",
                "Low-middle SDI", "Low SDI")

# Compute ASMR, PMR for each SDI quintile × year
df_sdi_calc <- df_sdi_q %>%
  filter(location_name %in% sdi_levels,
         age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths",
         year %in% c(1990, 2023)) %>%
  {
    # CGD Rate (sum of 13 causes)
    rate_data <- filter(., cause_name %in% genetic_list, metric_name == "Rate") %>%
      group_by(location_name, year) %>%
      summarise(cgd_rate = sum(val, na.rm = TRUE),
                cgd_rate_lower = sum(lower, na.rm = TRUE),
                cgd_rate_upper = sum(upper, na.rm = TRUE), .groups = "drop")

    # CGD Number (sum of 13 causes)
    num_data <- filter(., cause_name %in% genetic_list, metric_name == "Number") %>%
      group_by(location_name, year) %>%
      summarise(cgd_deaths = sum(val, na.rm = TRUE), .groups = "drop")

    # All causes Number (for PMR)
    all_data <- filter(., cause_name == "All causes", metric_name == "Number") %>%
      select(location_name, year, all_deaths = val)

    rate_data %>%
      left_join(num_data, by = c("location_name", "year")) %>%
      left_join(all_data, by = c("location_name", "year"))
  } %>%
  mutate(pmr = cgd_deaths / all_deaths * 100)

# CMNN rate for SDI quintiles (from SDI quintile data, if CMNN cause included)
df_sdi_cmnn <- df_sdi_q %>%
  filter(location_name %in% sdi_levels,
         age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths", metric_name == "Rate",
         str_detect(cause_name, "Communicable, maternal"),
         year %in% c(1990, 2023)) %>%
  select(location_name, year, cmnn_rate = val)

# ==============================================================================
# Part 3: Format table
# ==============================================================================
# Global 1990 and 2023
g1990 <- df_global_calc %>% filter(year == 1990)
g2023 <- df_global_calc %>% filter(year == 2023)

global_row <- tibble(
  Location = "Global",
  ASMR_1990 = paste0(formatC(round(g1990$cgd_rate, 2), format = "f", digits = 2), " (",
                     formatC(round(g1990$cgd_rate_lower, 2), format = "f", digits = 2), "\u2013",
                     formatC(round(g1990$cgd_rate_upper, 2), format = "f", digits = 2), ")"),
  PMR_1990  = formatC(round(g1990$pmr, 2), format = "f", digits = 2),
  ASMR_2023 = paste0(formatC(round(g2023$cgd_rate, 2), format = "f", digits = 2), " (",
                     formatC(round(g2023$cgd_rate_lower, 2), format = "f", digits = 2), "\u2013",
                     formatC(round(g2023$cgd_rate_upper, 2), format = "f", digits = 2), ")"),
  PMR_2023  = formatC(round(g2023$pmr, 2), format = "f", digits = 2),
  Change_CGD = formatC(round((g2023$cgd_rate - g1990$cgd_rate) / g1990$cgd_rate * 100, 1),
                       format = "f", digits = 1),
  Change_CMNN = formatC(round((g2023$cmnn_rate - g1990$cmnn_rate) / g1990$cmnn_rate * 100, 1),
                        format = "f", digits = 1)
)

# SDI rows: build from df_sdi_calc (has both 1990 and 2023)
fmt_rate <- function(r, lo, up) {
  paste0(formatC(round(r, 2), format = "f", digits = 2), " (",
         formatC(round(lo, 2), format = "f", digits = 2), "\u2013",
         formatC(round(up, 2), format = "f", digits = 2), ")")
}

sdi_1990 <- df_sdi_calc %>% filter(year == 1990)
sdi_2023 <- df_sdi_calc %>% filter(year == 2023)

# CMNN 1990 and 2023 per SDI
cmnn_1990 <- df_sdi_cmnn %>% filter(year == 1990) %>% select(location_name, cmnn_rate_1990 = cmnn_rate)
cmnn_2023 <- df_sdi_cmnn %>% filter(year == 2023) %>% select(location_name, cmnn_rate_2023 = cmnn_rate)

sdi_rows <- sdi_2023 %>%
  left_join(sdi_1990 %>% select(location_name,
                                 cgd_rate_1990 = cgd_rate,
                                 cgd_rate_lower_1990 = cgd_rate_lower,
                                 cgd_rate_upper_1990 = cgd_rate_upper,
                                 pmr_1990 = pmr),
            by = "location_name") %>%
  left_join(cmnn_1990, by = "location_name") %>%
  left_join(cmnn_2023, by = "location_name") %>%
  mutate(
    Location = location_name,
    ASMR_1990 = fmt_rate(cgd_rate_1990, cgd_rate_lower_1990, cgd_rate_upper_1990),
    PMR_1990  = formatC(round(pmr_1990, 2), format = "f", digits = 2),
    ASMR_2023 = fmt_rate(cgd_rate, cgd_rate_lower, cgd_rate_upper),
    PMR_2023  = formatC(round(pmr, 2), format = "f", digits = 2),
    Change_CGD = formatC(round((cgd_rate - cgd_rate_1990) / cgd_rate_1990 * 100, 1),
                         format = "f", digits = 1),
    Change_CMNN = ifelse(!is.na(cmnn_rate_1990) & !is.na(cmnn_rate_2023),
                         formatC(round((cmnn_rate_2023 - cmnn_rate_1990) / cmnn_rate_1990 * 100, 1),
                                 format = "f", digits = 1),
                         "\u2014")
  ) %>%
  select(Location, ASMR_1990, PMR_1990, ASMR_2023, PMR_2023, Change_CGD, Change_CMNN)

df_final <- bind_rows(global_row, sdi_rows) %>%
  mutate(Location = factor(Location,
                           levels = c("Global", "High SDI", "High-middle SDI",
                                      "Middle SDI", "Low-middle SDI", "Low SDI"))) %>%
  arrange(Location)

# --- Console output ---
cat("\n=== Table 1 Data ===\n")
print(as.data.frame(df_final))

# --- Flextable ---
table1_ft <- df_final %>%
  flextable() %>%
  set_header_labels(
    Location    = "Location",
    ASMR_1990   = "ASMR (95% UI)",
    PMR_1990    = "PMR (%)",
    ASMR_2023   = "ASMR (95% UI)",
    PMR_2023    = "PMR (%)",
    Change_CGD  = "ConGD ASMR\nChange (%)",
    Change_CMNN = "CMNN ASMR\nChange (%)"
  ) %>%
  add_header_row(
    values = c("", "1990", "2023", "1990\u20132023"),
    colwidths = c(1, 2, 2, 2)
  ) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body") %>%
  align(j = 1, align = "left", part = "all") %>%
  border_remove() %>%
  hline_top(border = fp_border_default(width = 1.5), part = "header") %>%
  hline(i = 1, j = 2:3, border = fp_border_default(width = 1), part = "header") %>%
  hline(i = 1, j = 4:5, border = fp_border_default(width = 1), part = "header") %>%
  hline(i = 1, j = 6:7, border = fp_border_default(width = 1), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1.5), part = "body") %>%
  padding(padding = 4, part = "all") %>%
  width(j = 1, width = 1.4) %>%
  width(j = c(2, 4), width = 1.8) %>%
  width(j = c(3, 5), width = 0.8) %>%
  width(j = c(6, 7), width = 1.0) %>%
  set_table_properties(layout = "fixed") %>%
  set_caption(caption = paste0(
    "Table 1. Baseline and temporal trends of congenital and genetic disorder (ConGD) ",
    "mortality burden globally and by Socio-demographic Index (1990\u20132023)."
  )) %>%
  add_footer_lines(paste0(
    "Note: CMNN ASMR Change is available only at the Global level. ",
    "The CMNN decline demonstrates that the rising PMR is driven by ",
    "faster CMNN reduction rather than ConGD-specific trends (compositional effect). ",
    "ASMR, age-standardized mortality rate per 100,000; PMR, proportional mortality ratio; ",
    "UI, uncertainty interval; CMNN, communicable, maternal, neonatal, and nutritional diseases."
  ))

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait", width = 8.27, height = 11.69),
  page_margins = page_mar(bottom = 1, top = 1, right = 0.8, left = 0.8)
)
save_as_docx(table1_ft, path = save_word_path, pr_section = sect_properties)

cat(paste0("\nTable 1 saved: ", save_word_path, "\n"))
