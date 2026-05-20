# ==============================================================================
# Table S15: DALY of ConGD in children under 5, 1990 vs 2023
#   Global + 5 SDI quintiles + 21 GBD regions
#   Reports: DALY count + 95% UI in 1990 and 2023, age-standardized DALY rate
#   (per 100,000) in 1990 and 2023, percentage change, and EAPC of the rate.
#   Year coverage and presentation follow Frontiers in Public Health peer
#   convention for GBD-style DALY reporting (1990 vs endpoint + EAPC; DALY
#   is not projected — projection is reserved for mortality, Fig 9).
# Output: outputs/tables/Table_S15_DALY.docx
# ==============================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(flextable); library(officer)
})

cat("--- Table S15: DALY of ConGD (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir   <- here::here()
path_daly  <- file.path(base_dir, "data/gbd2023_daly_1990_2023.csv.zip")
dir_out    <- file.path(base_dir, "outputs/tables")
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
save_word  <- file.path(dir_out, "Table_S15_DALY.docx")

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

# Locations to include: Global + 5 SDI quintiles + 21 GBD regions (no countries)
sdi_levels <- c("Low SDI", "Low-middle SDI", "Middle SDI",
                "High-middle SDI", "High SDI")
gbd_regions <- c(
  "Andean Latin America", "Caribbean", "Central Asia", "Central Europe",
  "Central Latin America", "Central Sub-Saharan Africa", "East Asia",
  "Eastern Europe", "Eastern Sub-Saharan Africa", "High-income Asia Pacific",
  "High-income North America", "North Africa and Middle East", "Oceania",
  "South Asia", "Southeast Asia", "Southern Latin America",
  "Southern Sub-Saharan Africa", "Tropical Latin America", "Western Europe",
  "Western Sub-Saharan Africa", "Australasia"
)
keep_locs <- c("Global", sdi_levels, gbd_regions)

# --- Read DALY data ---
df <- read_csv(path_daly, show_col_types = FALSE) %>%
  filter(age_name == "<5 years", sex_name == "Both",
         cause_name %in% all_13,
         location_name %in% keep_locs)

cat(sprintf("Rows after filter: %d ; locations: %d ; years: %d\n",
            nrow(df), length(unique(df$location_name)),
            length(unique(df$year))))

# --- Aggregate the 13 causes by (location, year, metric) ---
# Number: sum across causes; UI: sum lower/upper (approximate, GBD-standard).
# Rate:   sum across causes; UI: sum lower/upper (approximate; standard for ASR).
df_agg <- df %>%
  group_by(location_name, year, metric_name) %>%
  summarise(val = sum(val, na.rm = TRUE),
            lower = sum(lower, na.rm = TRUE),
            upper = sum(upper, na.rm = TRUE),
            .groups = "drop")

# --- Wide by metric ---
df_wide <- df_agg %>%
  pivot_wider(id_cols = c(location_name, year),
              names_from = metric_name,
              values_from = c(val, lower, upper),
              names_glue = "{metric_name}_{.value}")

# --- Endpoints 1990 / 2023 ---
d1990 <- df_wide %>% filter(year == 1990) %>%
  rename_with(~ paste0(., "_1990"), -c(location_name, year)) %>%
  dplyr::select(-year)
d2023 <- df_wide %>% filter(year == 2023) %>%
  rename_with(~ paste0(., "_2023"), -c(location_name, year)) %>%
  dplyr::select(-year)

# --- EAPC on the rate (ln(rate) ~ year, 1990..2023) ---
calc_eapc <- function(d) {
  fit <- lm(log(Rate_val) ~ year, data = d)
  b   <- coef(fit)["year"]
  se  <- summary(fit)$coefficients["year", "Std. Error"]
  tibble(eapc    = 100*(exp(b)-1),
         eapc_lo = 100*(exp(b - 1.96*se)-1),
         eapc_hi = 100*(exp(b + 1.96*se)-1))
}

eapc_df <- df_wide %>%
  filter(Rate_val > 0) %>%
  group_by(location_name) %>%
  group_modify(~ calc_eapc(.x)) %>% ungroup()

df_final <- d1990 %>%
  left_join(d2023, by = "location_name") %>%
  left_join(eapc_df, by = "location_name")

# --- Formatting helpers ---
um <- function(x) gsub("-", "−",
                       formatC(round(x, 2), format = "f", digits = 2),
                       fixed = TRUE)
ucomma <- function(x) gsub("-", "−",
                            formatC(round(x), format = "d", big.mark = ","),
                            fixed = TRUE)
fmt_count <- function(v, lo, up) {
  paste0(ucomma(v), " (", ucomma(lo), "–", ucomma(up), ")")
}
fmt_rate <- function(v, lo, up) {
  paste0(formatC(round(v, 1), format = "f", digits = 1, big.mark = ","), " (",
         formatC(round(lo, 1), format = "f", digits = 1, big.mark = ","), "–",
         formatC(round(up, 1), format = "f", digits = 1, big.mark = ","), ")")
}

# Order: Global, then 5 SDI quintiles (high → low), then 21 GBD regions A-Z
ord <- function(loc) {
  if (loc == "Global") return(0L)
  if (loc %in% sdi_levels) {
    return(1L * 100L + match(loc, c("High SDI","High-middle SDI","Middle SDI",
                                     "Low-middle SDI","Low SDI")))
  }
  return(200L + match(loc, sort(gbd_regions)))
}

df_tab <- df_final %>%
  mutate(.ord = sapply(location_name, ord)) %>%
  arrange(.ord) %>%
  mutate(
    `DALYs 1990 (95% UI)`   = fmt_count(Number_val_1990, Number_lower_1990, Number_upper_1990),
    `DALYs 2023 (95% UI)`   = fmt_count(Number_val_2023, Number_lower_2023, Number_upper_2023),
    `ASR 1990 (95% UI)`     = fmt_rate(Rate_val_1990, Rate_lower_1990, Rate_upper_1990),
    `ASR 2023 (95% UI)`     = fmt_rate(Rate_val_2023, Rate_lower_2023, Rate_upper_2023),
    `ASR change (%)`        = um((Rate_val_2023 - Rate_val_1990) / Rate_val_1990 * 100),
    `EAPC (%)`              = um(eapc),
    `EAPC 95% CI`           = paste0(um(eapc_lo), ", ", um(eapc_hi))
  ) %>%
  dplyr::select(Location = location_name,
                `DALYs 1990 (95% UI)`, `DALYs 2023 (95% UI)`,
                `ASR 1990 (95% UI)`,   `ASR 2023 (95% UI)`,
                `ASR change (%)`, `EAPC (%)`, `EAPC 95% CI`)

cat("\nPreview (Global + first 3 SDI rows):\n")
print(as.data.frame(head(df_tab, 4)))

# --- flextable ---
ft <- df_tab %>%
  flextable() %>%
  add_header_row(values = c("", "DALY count", "Age-standardized DALY rate (per 100,000)", "Trend"),
                 colwidths = c(1, 2, 2, 3)) %>%
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
  set_caption(paste0("Table S15. Disability-adjusted life years (DALYs) attributable to ",
    "congenital and genetic disorders in children under 5, 1990 vs 2023, ",
    "with age-standardized rates and estimated annual percentage change."
  )) %>%
  add_footer_lines(paste0(
    "DALYs aggregate the 13 ConGD causes (9 structural anomalies + 4 ",
    "hemoglobinopathies). ASR: age-standardized DALY rate per 100,000 ",
    "population. EAPC: estimated annual percentage change derived from ",
    "log-linear regression of ln(ASR) on calendar year, 1990–2023."
  ))

save_as_docx(ft, path = save_word,
             pr_section = prop_section(
               page_size    = page_size(orient = "landscape"),
               page_margins = page_mar(top = 0.5, bottom = 0.5,
                                       left = 0.4, right = 0.4)))
cat("Table S15 saved:", save_word, "\n")
