# ==============================================================================
# Table S12: Estimated Annual Percentage Change (EAPC) of ConGD ASMR
#   Global + 21 GBD regions, 1990-2023, with 95% CI
# Output: outputs/tables/Table_S12_EAPC.docx
# ==============================================================================
library(tidyverse)
library(flextable)
library(officer)

cat("--- Table S12: EAPC (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir   <- here::here()
path_trend <- file.path(base_dir, "data/gbd2023_trend_1990_2023.csv.zip")
dir_out    <- file.path(base_dir, "outputs/tables")
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
save_word_path <- file.path(dir_out, "Table_S12_EAPC.docx")

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
         measure_name == "Deaths", metric_name == "Rate",
         cause_name %in% all_13) %>%
  group_by(location_name, year) %>%
  summarise(asmr = sum(val, na.rm = TRUE), .groups = "drop") %>%
  filter(asmr > 0)

calc_eapc <- function(d) {
  fit <- lm(log(asmr) ~ year, data = d)
  b   <- coef(fit)["year"]
  se  <- summary(fit)$coefficients["year", "Std. Error"]
  tibble(eapc    = 100 * (exp(b) - 1),
         eapc_lo = 100 * (exp(b - 1.96 * se) - 1),
         eapc_hi = 100 * (exp(b + 1.96 * se) - 1))
}

eapc <- df %>%
  group_by(location_name) %>%
  group_modify(~ calc_eapc(.x)) %>%
  ungroup() %>%
  mutate(Trend = case_when(eapc_lo > 0 ~ "Increasing",
                           eapc_hi < 0 ~ "Decreasing",
                           TRUE        ~ "Stable"))

# Order: Global first, then regions alphabetically
eapc <- eapc %>%
  mutate(.ord = if_else(location_name == "Global", 0L, 1L)) %>%
  arrange(.ord, location_name) %>%
  dplyr::select(-.ord)

# Use Unicode minus sign (U+2212) for negatives, consistent with manuscript
um <- function(x) gsub("-", "−", formatC(round(x, 2), format = "f", digits = 2), fixed = TRUE)

df_tab <- eapc %>%
  transmute(
    Location = location_name,
    EAPC     = um(eapc),
    `95% CI` = paste0(um(eapc_lo), ", ", um(eapc_hi)),
    Trend
  )

ft <- df_tab %>%
  flextable() %>%
  set_header_labels(EAPC = "EAPC (%)", `95% CI` = "95% CI (%)") %>%
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
  set_caption("Table S12. Estimated annual percentage change (EAPC) of ConGD age-standardized mortality rate, 1990–2023.") %>%
  add_footer_lines(paste0(
    "EAPC derived from a log-linear regression of ln(ASMR) on calendar year; ",
    "EAPC = 100 × [exp(β) − 1] with 95% CI from the standard error of β. ",
    "Trend: Decreasing (upper CI < 0), Increasing (lower CI > 0), or Stable (CI spans 0)."
  ))

save_as_docx(ft, path = save_word_path,
             pr_section = prop_section(page_size = page_size(orient = "portrait"),
                                       page_margins = page_mar(top = 0.8, bottom = 0.8,
                                                               left = 0.8, right = 0.8)))
cat("Table S12 saved:", save_word_path, "\n")
