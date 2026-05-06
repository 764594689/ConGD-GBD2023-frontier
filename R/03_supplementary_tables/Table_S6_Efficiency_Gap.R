# ==============================================================================
# Table S8: Top-10 / Bottom-10 Countries by Efficiency Gap (GBD 2023)
# ==============================================================================
library(tidyverse)
library(quantreg)
library(splines)
library(flextable)
library(officer)

cat("--- Table S8: Efficiency Gap Rankings (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_country <- file.path(base_dir, "data/gbd2023_sdi_country_2023.csv.zip")
path_sdi     <- file.path(base_dir, "data/gbd2023_sdi_values_1950_2023.csv")
output_dir <- file.path(base_dir, "outputs/tables")
save_word_path <- file.path(output_dir, "Table_S6_Efficiency_Gap.docx")

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
# Normalize country-name apostrophes (straight ' -> smart ’)
if ("location_name" %in% colnames(df_raw)) {
  df_raw <- df_raw %>% mutate(location_name = gsub("'", "’", location_name, fixed = TRUE))
}


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
  drop_na(sdi_value, total_rate) %>% filter(total_rate > 0)

# Frontier
qr_05 <- rq(log(total_rate) ~ ns(sdi_value, df = 3), tau = 0.05, data = df_final)
df_final <- df_final %>%
  mutate(
    frontier_asmr = exp(predict(qr_05, .)),
    efficiency_gap = total_rate - frontier_asmr,
    gap_pct = efficiency_gap / total_rate * 100,
    frontier_deaths = total_deaths * (frontier_asmr / total_rate),
    avoidable_deaths = pmax(total_deaths - frontier_deaths, 0)
  )

global_avoidable <- sum(df_final$avoidable_deaths[df_final$avoidable_deaths > 0], na.rm = TRUE)
global_total <- sum(df_final$total_deaths, na.rm = TRUE)
global_pct <- global_avoidable / global_total * 100

cat(sprintf("\nGlobal avoidable: %s (%.1f%%)\n",
            format(round(global_avoidable), big.mark = ","), global_pct))

top10 <- df_final %>% arrange(desc(efficiency_gap)) %>% head(10) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Country = location_name, SDI_Quintile = sdi_group,
         SDI = sdi_value, Observed_ASMR = total_rate,
         Frontier_ASMR = frontier_asmr, Efficiency_Gap = efficiency_gap,
         Gap_pct = gap_pct)

bottom10 <- df_final %>% arrange(efficiency_gap) %>% head(10) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Country = location_name, SDI_Quintile = sdi_group,
         SDI = sdi_value, Observed_ASMR = total_rate,
         Frontier_ASMR = frontier_asmr, Efficiency_Gap = efficiency_gap,
         Gap_pct = gap_pct)

cat("\n--- Top 10 ---\n"); print(as.data.frame(top10))
cat("\n--- Bottom 10 ---\n"); print(as.data.frame(bottom10))

format_row <- function(df) {
  df %>% mutate(
    SDI = formatC(round(SDI, 3), format = "f", digits = 3),
    Observed_ASMR = formatC(round(Observed_ASMR, 2), format = "f", digits = 2),
    Frontier_ASMR = formatC(round(Frontier_ASMR, 2), format = "f", digits = 2),
    Efficiency_Gap = formatC(round(Efficiency_Gap, 2), format = "f", digits = 2),
    Gap_pct = paste0(formatC(round(Gap_pct, 1), format = "f", digits = 1), "%")
  )
}

make_ft <- function(df, subtitle) {
  df %>% format_row() %>% flextable() %>%
    set_header_labels(Gap_pct = "Gap %") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 10, part = "all") %>% bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    align(j = 2, align = "left", part = "body") %>%
    border_remove() %>%
    hline_top(border = fp_border_default(width = 1.5), part = "header") %>%
    hline_bottom(border = fp_border_default(width = 1), part = "header") %>%
    hline_bottom(border = fp_border_default(width = 1.5), part = "body") %>%
    padding(padding = 4, part = "all") %>%
    set_table_properties(layout = "autofit") %>%
    set_caption(subtitle)
}

doc <- read_docx() %>%
  body_add_flextable(make_ft(top10, "Table S6. Efficiency Gap Rankings (2023). Panel A. Top-10 (largest gaps).")) %>%
  body_add_par("") %>%
  body_add_flextable(make_ft(bottom10, "Table S6 (continued). Panel B. Bottom-10 (nearest frontier).")) %>%
  body_add_par(sprintf("Global: %s avoidable deaths (%.1f%% of total).",
                       format(round(global_avoidable), big.mark = ","), global_pct))

print(doc, target = save_word_path)

# Export CSV
write_csv(df_final %>%
            select(location_name, sdi_value, sdi_group, total_rate,
                   total_deaths, frontier_asmr, efficiency_gap, gap_pct, avoidable_deaths) %>%
            arrange(desc(efficiency_gap)),
          file.path(output_dir, "Table_S6_Efficiency_Gap_all_countries.csv"))

cat("Table S8 saved:", save_word_path, "\n")
