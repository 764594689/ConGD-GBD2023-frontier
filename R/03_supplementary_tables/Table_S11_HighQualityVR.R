# ==============================================================================
# Sensitivity Analysis: Frontier re-estimated on high-quality VR countries
# (GBD data quality rating ≥4 stars, per GBD 2019 appendix Table S7)
# Addresses reviewer concern #1: ascertainment bias inflating avoidable deaths
# ==============================================================================
library(tidyverse)
library(quantreg)
library(splines)
library(flextable)
library(officer)

cat("--- Sensitivity: High-Quality VR Frontier ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_country <- file.path(base_dir, "data/gbd2023_sdi_country_2023.csv.zip")
path_sdi     <- file.path(base_dir, "data/gbd2023_sdi_values_1950_2023.csv")
path_stars   <- file.path(base_dir, "data/gbd_vr_stars.csv")
output_dir <- file.path(base_dir, "outputs/tables")

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

# --- Load GBD stars and harmonize names to GBD location_name ---
stars <- read_csv(path_stars, show_col_types = FALSE)

name_map <- c(
  "UK" = "United Kingdom",
  "USA" = "United States of America",
  "Russia" = "Russian Federation",
  "Moldova" = "Republic of Moldova",
  "Venezuela" = "Venezuela (Bolivarian Republic of)",
  "Iran" = "Iran (Islamic Republic of)",
  "Syria" = "Syrian Arab Republic",
  "Vietnam" = "Viet Nam",
  "South Korea" = "Republic of Korea",
  "North Korea" = "Democratic People's Republic of Korea",
  "Tanzania" = "United Republic of Tanzania",
  "Bolivia" = "Bolivia (Plurinational State of)",
  "Laos" = "Lao People's Democratic Republic",
  "Czech Republic" = "Czechia",
  "Taiwan (province of China)" = "Taiwan (Province of China)",
  "Macedonia" = "North Macedonia",
  "Brunei" = "Brunei Darussalam",
  "Cape Verde" = "Cabo Verde",
  "Swaziland" = "Eswatini",
  "Gambia" = "The Gambia",
  "Bahamas" = "The Bahamas",
  "East Timor" = "Timor-Leste",
  "Micronesia" = "Micronesia (Federated States of)",
  "Cote d'Ivoire" = "Côte d'Ivoire"
)
stars <- stars %>% mutate(location_name = ifelse(country %in% names(name_map),
                                                 name_map[country], country))

high_q <- stars %>% filter(stars >= 4) %>% pull(location_name)
cat(sprintf("High-quality VR (≥4 stars): %d countries\n", length(high_q)))

# --- Country burden ---
df_raw <- read_csv(path_country, show_col_types = FALSE)

df_rate <- df_raw %>%
  filter(year == 2023, age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths", cause_name %in% genetic_list,
         metric_name == "Rate") %>%
  group_by(location_name) %>%
  summarise(total_rate = sum(val, na.rm = TRUE), .groups = "drop")

df_num <- df_raw %>%
  filter(year == 2023, age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths", cause_name %in% genetic_list,
         metric_name == "Number") %>%
  group_by(location_name) %>%
  summarise(total_deaths = sum(val, na.rm = TRUE), .groups = "drop")

df_sdi <- read_csv(path_sdi, show_col_types = FALSE) %>%
  filter(year_id == 2023) %>%
  arrange(location_id) %>%
  distinct(location_name, .keep_all = TRUE) %>%
  select(location_name, sdi_value = mean_value) %>%
  mutate(sdi_group = case_when(
    sdi_value <= 0.454 ~ "Low", sdi_value <= 0.608 ~ "Low-middle",
    sdi_value <= 0.701 ~ "Middle", sdi_value <= 0.813 ~ "High-middle",
    TRUE ~ "High"
  ))

df_final <- df_rate %>%
  left_join(df_num, by = "location_name") %>%
  inner_join(df_sdi, by = "location_name") %>%
  drop_na(sdi_value, total_rate) %>% filter(total_rate > 0) %>%
  mutate(high_quality_vr = location_name %in% high_q)

cat(sprintf("\nTotal countries with burden + SDI: %d\n", nrow(df_final)))
cat(sprintf("  of which high-quality VR: %d\n", sum(df_final$high_quality_vr)))
cat(sprintf("  of which low-quality VR:  %d\n", sum(!df_final$high_quality_vr)))

# --- (1) MAIN frontier: all 204 countries (original analysis) ---
qr_main <- rq(log(total_rate) ~ ns(sdi_value, df = 3), tau = 0.05, data = df_final)

df_final$frontier_main <- exp(predict(qr_main, df_final))
df_final$gap_main <- pmax(df_final$total_rate - df_final$frontier_main, 0)
df_final$avoid_main <- df_final$total_deaths * (df_final$gap_main / df_final$total_rate)

avoid_main_total <- sum(df_final$avoid_main, na.rm = TRUE)
avoid_main_pct   <- avoid_main_total / sum(df_final$total_deaths) * 100

# --- (2) SENSITIVITY frontier: fit on ≥4-star countries only ---
df_hq <- df_final %>% filter(high_quality_vr)
qr_hq <- rq(log(total_rate) ~ ns(sdi_value, df = 3), tau = 0.05, data = df_hq)

# Apply this frontier to ALL countries (extrapolated where needed)
df_final$frontier_hq <- exp(predict(qr_hq, df_final))
df_final$gap_hq <- pmax(df_final$total_rate - df_final$frontier_hq, 0)
df_final$avoid_hq <- df_final$total_deaths * (df_final$gap_hq / df_final$total_rate)

avoid_hq_total <- sum(df_final$avoid_hq, na.rm = TRUE)
avoid_hq_pct   <- avoid_hq_total / sum(df_final$total_deaths) * 100

# --- Print comparison ---
cat("\n================================================\n")
cat("   MAIN vs HIGH-QUALITY VR FRONTIER COMPARISON\n")
cat("================================================\n")
cat(sprintf("MAIN (all 204 countries):\n"))
cat(sprintf("  Avoidable deaths = %s (%.1f%% of total)\n",
            format(round(avoid_main_total), big.mark = ","), avoid_main_pct))
cat(sprintf("HIGH-QUALITY VR (frontier from ≥4-star only, applied globally):\n"))
cat(sprintf("  Avoidable deaths = %s (%.1f%% of total)\n",
            format(round(avoid_hq_total), big.mark = ","), avoid_hq_pct))
cat(sprintf("  Ratio vs main: %.2fx\n", avoid_hq_total / avoid_main_total))

# Frontier values at key SDI points
sdi_grid <- tibble(sdi_value = seq(0.3, 0.9, 0.1))
sdi_grid$frontier_main <- exp(predict(qr_main, sdi_grid))
sdi_grid$frontier_hq   <- exp(predict(qr_hq,   sdi_grid))
sdi_grid$ratio_hq_main <- sdi_grid$frontier_hq / sdi_grid$frontier_main
cat("\nFrontier values at key SDI levels:\n")
print(sdi_grid %>% mutate(across(where(is.numeric), ~round(., 2))) %>% as.data.frame())

# --- Per SDI quintile: avoidable deaths under both frontiers ---
by_sdi <- df_final %>%
  group_by(sdi_group) %>%
  summarise(
    n_countries = n(),
    total_deaths = sum(total_deaths, na.rm = TRUE),
    avoid_main = sum(avoid_main, na.rm = TRUE),
    avoid_hq   = sum(avoid_hq,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_main = avoid_main / total_deaths * 100,
    pct_hq   = avoid_hq   / total_deaths * 100,
    sdi_group = factor(sdi_group,
                       levels = c("High", "High-middle", "Middle", "Low-middle", "Low"))
  ) %>%
  arrange(sdi_group)

cat("\nBy SDI quintile:\n"); print(as.data.frame(by_sdi))

# --- Save sensitivity table as Word ---
ft_df <- by_sdi %>%
  transmute(
    `SDI Quintile` = sdi_group,
    `Countries (n)` = n_countries,
    `Total deaths` = formatC(round(total_deaths), format = "d", big.mark = ","),
    `Main frontier: avoidable (n)` = formatC(round(avoid_main), format = "d", big.mark = ","),
    `Main: %` = paste0(formatC(pct_main, format = "f", digits = 1), "%"),
    `HQ-VR frontier: avoidable (n)` = formatC(round(avoid_hq), format = "d", big.mark = ","),
    `HQ-VR: %` = paste0(formatC(pct_hq, format = "f", digits = 1), "%")
  )

total_row <- tibble(
  `SDI Quintile` = "Global",
  `Countries (n)` = nrow(df_final),
  `Total deaths` = formatC(round(sum(df_final$total_deaths)), format = "d", big.mark = ","),
  `Main frontier: avoidable (n)` = formatC(round(avoid_main_total), format = "d", big.mark = ","),
  `Main: %` = paste0(formatC(avoid_main_pct, format = "f", digits = 1), "%"),
  `HQ-VR frontier: avoidable (n)` = formatC(round(avoid_hq_total), format = "d", big.mark = ","),
  `HQ-VR: %` = paste0(formatC(avoid_hq_pct, format = "f", digits = 1), "%")
)
ft_df <- bind_rows(total_row, ft_df)

ft <- ft_df %>% flextable() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>% bold(part = "header") %>%
  bold(i = 1) %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  border_remove() %>%
  hline_top(border = fp_border_default(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1), part = "header") %>%
  hline(i = 1) %>%
  hline_bottom(border = fp_border_default(width = 1.5), part = "body") %>%
  padding(padding = 4, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  set_caption("Table S11. Sensitivity of avoidable ConGD deaths estimates to frontier specification.") %>%
  add_footer_lines(sprintf(
    "Main frontier: τ=0.05 quantile regression on log-ASMR vs SDI (natural cubic spline, df=3), all %d countries. HQ-VR frontier: same specification but fitted only on %d countries with GBD data quality rating ≥4 stars, then applied to all countries to recompute efficiency gaps. Source of star ratings: GBD 2019 appendix Table S7.",
    nrow(df_final), sum(df_final$high_quality_vr)
  ))

save_as_docx(ft, path = file.path(output_dir, "Table_S11_HighQualityVR.docx"),
             pr_section = prop_section(page_size = page_size(orient = "landscape")))

# Save country-level sensitivity CSV
write_csv(df_final %>%
            select(location_name, sdi_value, sdi_group, high_quality_vr,
                   total_rate, total_deaths,
                   frontier_main, gap_main, avoid_main,
                   frontier_hq,   gap_hq,   avoid_hq) %>%
            arrange(desc(avoid_hq)),
          file.path(output_dir, "Table_S11_HighQualityVR_country.csv"))

cat("\nSaved:\n  Table_S11_HighQualityVR.docx\n  Table_S11_HighQualityVR_country.csv\n")
