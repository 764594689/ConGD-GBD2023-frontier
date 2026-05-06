# ==============================================================================
# Table S5: 21 GBD Regions — ConGD Burden (GBD 2023, 1990 vs 2023)
# ==============================================================================
library(tidyverse)
library(flextable)
library(officer)

cat("--- Table S5: Regional (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_trend <- file.path(base_dir, "data/gbd2023_trend_1990_2023.csv.zip")
dir_out    <- file.path(base_dir, "outputs/tables")
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
save_word_path <- file.path(dir_out, "Table_S9_Regional.docx")

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

df_raw <- read_csv(path_trend, show_col_types = FALSE)

# Exclude Global, SDI groups, and Super Regions (keep only 21 GBD Regions)
exclude_locs <- c("Global", "High SDI", "High-middle SDI", "Middle SDI",
                  "Low-middle SDI", "Low SDI",
                  "High-income", "Latin America and Caribbean",
                  "Sub-Saharan Africa",
                  "Central Europe, Eastern Europe, and Central Asia",
                  "Southeast Asia, East Asia, and Oceania")

df_base <- df_raw %>%
  filter(measure_name == "Deaths", year %in% c(1990, 2023),
         !location_name %in% exclude_locs,
         age_name == "<5 years", sex_name == "Both")

# Deduplicate base data (in case of multiple population_group rows)
df_base <- df_base %>%
  group_by(location_name, year, cause_name, metric_name) %>%
  summarise(val = first(val), lower = first(lower), upper = first(upper), .groups = "drop")

# CGD Rate
df_gen_rate <- df_base %>%
  filter(cause_name %in% genetic_list, metric_name == "Rate") %>%
  group_by(location_name, year) %>%
  summarise(cgd_rate = sum(val, na.rm = TRUE),
            cgd_rate_lo = sum(lower, na.rm = TRUE),
            cgd_rate_up = sum(upper, na.rm = TRUE), .groups = "drop")

# CGD Number
df_gen_num <- df_base %>%
  filter(cause_name %in% genetic_list, metric_name == "Number") %>%
  group_by(location_name, year) %>%
  summarise(cgd_deaths = sum(val, na.rm = TRUE), .groups = "drop")

# All causes Number
df_all <- df_base %>%
  filter(cause_name == "All causes", metric_name == "Number") %>%
  select(location_name, year, all_deaths = val)

# CMNN Rate
df_cmnn <- df_base %>%
  filter(str_detect(cause_name, "Communicable, maternal"), metric_name == "Rate") %>%
  select(location_name, year, cmnn_rate = val)

# Merge all
df_calc <- df_gen_rate %>%
  left_join(df_gen_num, by = c("location_name", "year")) %>%
  left_join(df_all, by = c("location_name", "year")) %>%
  left_join(df_cmnn, by = c("location_name", "year")) %>%
  mutate(pmr_val = cgd_deaths / all_deaths * 100)

# Split into 1990 and 2023 then merge wide
d1990 <- df_calc %>% filter(year == 1990) %>%
  select(location_name, cgd_rate_1990 = cgd_rate, lo_1990 = cgd_rate_lo,
         up_1990 = cgd_rate_up, pmr_1990 = pmr_val, cmnn_1990 = cmnn_rate)
d2023 <- df_calc %>% filter(year == 2023) %>%
  select(location_name, cgd_rate_2023 = cgd_rate, lo_2023 = cgd_rate_lo,
         up_2023 = cgd_rate_up, pmr_2023 = pmr_val, cmnn_2023 = cmnn_rate)

df_wide <- d1990 %>% left_join(d2023, by = "location_name")

# Format
fmt <- function(r, lo, up) {
  paste0(formatC(round(r, 2), format = "f", digits = 2), " (",
         formatC(round(lo, 2), format = "f", digits = 2), "\u2013",
         formatC(round(up, 2), format = "f", digits = 2), ")")
}

df_formatted <- df_wide %>%
  mutate(
    ASMR_1990 = fmt(cgd_rate_1990, lo_1990, up_1990),
    PMR_1990  = formatC(round(pmr_1990, 2), format = "f", digits = 2),
    ASMR_2023 = fmt(cgd_rate_2023, lo_2023, up_2023),
    PMR_2023  = formatC(round(pmr_2023, 2), format = "f", digits = 2),
    Change_CGD  = formatC(round((cgd_rate_2023 - cgd_rate_1990) / cgd_rate_1990 * 100, 1),
                          format = "f", digits = 1),
    Change_CMNN = ifelse(!is.na(cmnn_1990) & !is.na(cmnn_2023),
                         formatC(round((cmnn_2023 - cmnn_1990) / cmnn_1990 * 100, 1),
                                 format = "f", digits = 1),
                         "\u2014")
  ) %>%
  select(Region = location_name, ASMR_1990, PMR_1990, ASMR_2023, PMR_2023,
         Change_CGD, Change_CMNN) %>%
  arrange(Region)

cat("\n=== Table S5 Data ===\n")
print(as.data.frame(df_formatted))

ft <- df_formatted %>%
  flextable() %>%
  set_header_labels(
    Region = "GBD Region", ASMR_1990 = "ASMR (95% UI)", PMR_1990 = "PMR (%)",
    ASMR_2023 = "ASMR (95% UI)", PMR_2023 = "PMR (%)",
    Change_CGD = "ConGD ASMR\nChange (%)", Change_CMNN = "CMNN ASMR\nChange (%)"
  ) %>%
  add_header_row(values = c("", "1990", "2023", "1990\u20132023"), colwidths = c(1, 2, 2, 2)) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  border_remove() %>%
  hline_top(border = fp_border_default(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1.5), part = "body") %>%
  padding(padding = 3, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  set_caption("Table S9. Regional ConGD mortality burden (1990\u20132023).") %>%
  add_footer_lines("ASMR per 100,000; PMR = proportional mortality ratio; CMNN = communicable, maternal, neonatal, nutritional.")

save_as_docx(ft, path = save_word_path,
             pr_section = prop_section(page_size = page_size(orient = "landscape")))
cat("Table S5 saved:", save_word_path, "\n")
