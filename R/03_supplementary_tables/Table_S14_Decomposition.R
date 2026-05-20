# ==============================================================================
# Table S14: Kitagawa decomposition of 1990-2023 change in ConGD U5 deaths
#   Global + 21 GBD regions: population-growth vs ASMR-change components
# Output: outputs/tables/Table_S14_Decomposition.docx
# ==============================================================================
library(tidyverse)
library(flextable)
library(officer)

cat("--- Table S14: Decomposition (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir   <- here::here()
path_trend <- file.path(base_dir, "data/gbd2023_trend_1990_2023.csv.zip")
dir_out    <- file.path(base_dir, "outputs/tables")
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
save_word_path <- file.path(dir_out, "Table_S14_Decomposition.docx")

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

df <- read_csv(path_trend, show_col_types = FALSE) %>%
  filter(age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths", cause_name %in% all_13,
         year %in% c(1990, 2023))

agg <- df %>%
  group_by(location_name, year, metric_name) %>%
  summarise(val = sum(val, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = metric_name, values_from = val) %>%
  # GBD's "<5 years" Rate is the age-specific mortality rate per 100,000
  # under-5 population (not separately age-standardized within the 0-4 range).
  # Number ÷ Rate × 100,000 recovers GBD's internal under-5 population
  # estimate, keeping Number = Rate × Pop / 1e5 exact for the decomposition.
  mutate(under5_pop = Number / Rate * 1e5)

decomp <- agg %>%
  group_by(location_name) %>%
  summarise(
    D_1990 = Number[year == 1990],
    D_2023 = Number[year == 2023],
    R_1990 = Rate[year == 1990],
    R_2023 = Rate[year == 2023],
    P_1990 = under5_pop[year == 1990],
    P_2023 = under5_pop[year == 2023],
    .groups = "drop"
  ) %>%
  mutate(
    dD          = D_2023 - D_1990,
    pop_effect  = (P_2023 - P_1990) * ((R_1990 + R_2023) / 2) / 1e5,
    rate_effect = (R_2023 - R_1990) * ((P_1990 + P_2023) / 2) / 1e5
  ) %>%
  mutate(.ord = if_else(location_name == "Global", 0L, 1L)) %>%
  arrange(.ord, location_name) %>%
  dplyr::select(-.ord)

fmt0 <- function(x) formatC(round(x), format = "d", big.mark = ",")
fmts <- function(x) paste0(if_else(x >= 0, "+", "−"),
                           formatC(abs(round(x)), format = "d", big.mark = ","))

df_tab <- decomp %>%
  transmute(
    Location              = location_name,
    `Deaths 1990`         = fmt0(D_1990),
    `Deaths 2023`         = fmt0(D_2023),
    `Δ Deaths`            = fmts(dD),
    `Population effect`   = fmts(pop_effect),
    `ASMR-change effect`  = fmts(rate_effect)
  )

ft <- df_tab %>%
  flextable() %>%
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
  set_caption("Table S14. Kitagawa decomposition of the 1990–2023 change in ConGD under-5 deaths.") %>%
  add_footer_lines(paste0(
    "Kitagawa decomposition: ΔDeaths = ΔPopulation × mean(ASMR) + ",
    "mean(Population) × ΔASMR. Population effect = contribution of under-5 ",
    "population change; ASMR-change effect = contribution of mortality-rate change. ",
    "Their sum approximates ΔDeaths (small residual from the interaction term)."
  ))

save_as_docx(ft, path = save_word_path,
             pr_section = prop_section(page_size = page_size(orient = "landscape"),
                                       page_margins = page_mar(top = 0.8, bottom = 0.8,
                                                               left = 0.8, right = 0.8)))
cat("Table S14 saved:", save_word_path, "\n")
