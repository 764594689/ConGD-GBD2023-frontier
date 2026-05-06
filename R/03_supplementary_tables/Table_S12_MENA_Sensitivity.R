# ==============================================================================
# Sensitivity Analysis: Frontier with MENA / high-consanguinity countries excluded
# Addresses reviewer concern #2: consanguinity as unmeasured confounder
# inflating baseline ConGD incidence in MENA, mistaken for "system failure"
# ==============================================================================
library(tidyverse)
library(quantreg)
library(splines)
library(flextable)
library(officer)

cat("--- Sensitivity: MENA-excluded Frontier ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_country <- file.path(base_dir, "data/gbd2023_sdi_country_2023.csv.zip")
path_sdi     <- file.path(base_dir, "data/gbd2023_sdi_values_1950_2023.csv")
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

# --- Define high-consanguinity countries ---
# GBD "North Africa and Middle East" region + Pakistan, Afghanistan
# (well-documented consanguinity rates >20%; Bittles & Black 2010, PNAS)
mena_countries <- c(
  # GBD North Africa and Middle East
  "Afghanistan", "Algeria", "Bahrain", "Egypt",
  "Iran (Islamic Republic of)", "Iraq", "Jordan", "Kuwait",
  "Lebanon", "Libya", "Morocco", "Oman", "Palestine",
  "Qatar", "Saudi Arabia", "Sudan", "Syrian Arab Republic",
  "Tunisia", "Türkiye", "Turkey", "United Arab Emirates", "Yemen",
  # South Asia high-consanguinity
  "Pakistan"
)

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
  mutate(is_mena = location_name %in% mena_countries)

cat(sprintf("\nTotal countries: %d\n", nrow(df_final)))
cat(sprintf("  MENA / high-consanguinity:    %d\n", sum(df_final$is_mena)))
cat("MENA matched in data:\n")
print(df_final$location_name[df_final$is_mena])

# --- (1) MAIN frontier: all countries ---
qr_main <- rq(log(total_rate) ~ ns(sdi_value, df = 3), tau = 0.05, data = df_final)
df_final$frontier_main <- exp(predict(qr_main, df_final))
df_final$gap_main  <- pmax(df_final$total_rate - df_final$frontier_main, 0)
df_final$avoid_main <- df_final$total_deaths * (df_final$gap_main / df_final$total_rate)
avoid_main_total <- sum(df_final$avoid_main, na.rm = TRUE)
avoid_main_pct   <- avoid_main_total / sum(df_final$total_deaths) * 100

# --- (2) SENSITIVITY: frontier fitted on non-MENA only ---
df_nonmena <- df_final %>% filter(!is_mena)
qr_nm <- rq(log(total_rate) ~ ns(sdi_value, df = 3), tau = 0.05, data = df_nonmena)

df_final$frontier_nm <- exp(predict(qr_nm, df_final))
df_final$gap_nm  <- pmax(df_final$total_rate - df_final$frontier_nm, 0)
df_final$avoid_nm <- df_final$total_deaths * (df_final$gap_nm / df_final$total_rate)

# Apply only to non-MENA countries (MENA gaps not interpretable as "system failure")
avoid_nm_nonmena <- sum(df_final$avoid_nm[!df_final$is_mena], na.rm = TRUE)
total_nonmena    <- sum(df_final$total_deaths[!df_final$is_mena], na.rm = TRUE)
avoid_nm_pct     <- avoid_nm_nonmena / total_nonmena * 100

avoid_main_nonmena <- sum(df_final$avoid_main[!df_final$is_mena], na.rm = TRUE)
avoid_main_mena    <- sum(df_final$avoid_main[df_final$is_mena],  na.rm = TRUE)

cat("\n================================================\n")
cat("   MAIN vs MENA-EXCLUDED FRONTIER COMPARISON\n")
cat("================================================\n")
cat(sprintf("MAIN (frontier from all 204):\n"))
cat(sprintf("  Total avoidable     = %s (%.1f%%)\n",
            format(round(avoid_main_total), big.mark = ","), avoid_main_pct))
cat(sprintf("  Of which non-MENA   = %s\n", format(round(avoid_main_nonmena), big.mark = ",")))
cat(sprintf("  Of which MENA       = %s (%.1f%% of total)\n",
            format(round(avoid_main_mena), big.mark = ","),
            avoid_main_mena / avoid_main_total * 100))
cat(sprintf("\nMENA-EXCLUDED frontier (fitted on non-MENA, applied to non-MENA):\n"))
cat(sprintf("  Non-MENA avoidable  = %s (%.1f%% of non-MENA total)\n",
            format(round(avoid_nm_nonmena), big.mark = ","), avoid_nm_pct))

# Top-10 efficiency gap countries under each
top_main <- df_final %>% arrange(desc(gap_main)) %>% head(10) %>% select(location_name, sdi_group, gap_main, is_mena)
top_nm   <- df_final %>% filter(!is_mena) %>% arrange(desc(gap_nm)) %>% head(10) %>% select(location_name, sdi_group, gap_nm)

cat("\nTop-10 efficiency gap (MAIN, all countries):\n"); print(as.data.frame(top_main))
cat("\nTop-10 efficiency gap (MENA-EXCLUDED frontier, non-MENA only):\n"); print(as.data.frame(top_nm))

# --- Sensitivity table ---
ft_df <- tibble(
  Specification = c("Main (all countries)",
                    "MENA excluded from frontier fit, gaps reported for non-MENA only"),
  `Countries in frontier fit` = c(nrow(df_final), nrow(df_nonmena)),
  `Avoidable deaths (n)` = c(
    formatC(round(avoid_main_total), format = "d", big.mark = ","),
    formatC(round(avoid_nm_nonmena), format = "d", big.mark = ",")
  ),
  `Avoidable share (%)` = c(
    paste0(formatC(avoid_main_pct, format = "f", digits = 1), "%"),
    paste0(formatC(avoid_nm_pct,   format = "f", digits = 1), "%")
  ),
  `Scope` = c("Global, 204 countries",
              paste0("Non-MENA, ", sum(!df_final$is_mena), " countries"))
)

ft <- ft_df %>% flextable() %>%
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
  set_caption("Table S12. Sensitivity analysis excluding high-consanguinity (MENA + Pakistan + Afghanistan) countries from the frontier.") %>%
  add_footer_lines(paste0(
    "The MENA region has documented consanguinity rates of 20-50% (Bittles & Black, PNAS 2010), ",
    "elevating baseline autosomal recessive ConGD incidence independently of health system performance. ",
    "MENA countries (n=", sum(df_final$is_mena), " in data): ",
    paste(sort(df_final$location_name[df_final$is_mena]), collapse = ", "), "."
  ))

save_as_docx(ft, path = file.path(output_dir, "Table_S12_MENA_Sensitivity.docx"),
             pr_section = prop_section(page_size = page_size(orient = "landscape")))

cat("\nSaved: Table_S12_MENA_Sensitivity.docx\n")
