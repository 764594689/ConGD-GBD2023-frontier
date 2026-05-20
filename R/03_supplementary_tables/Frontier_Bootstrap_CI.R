# ==============================================================================
# Frontier bootstrap CI  (robustness check on Figure 6 / Table S6 frontier)
#   Re-fit tau=0.05 quantile spline on B resamples of countries, recompute
#   country-level avoidable deaths, sum to obtain the bootstrap distribution.
#   NOT a standalone supplementary table: the resulting single 95% CI is
#   reported INLINE in the manuscript (Abstract + Results) and the procedure
#   is described in Methods. This script only prints the CI for reproducibility.
# Output: console only (no docx, no csv).
# ==============================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(quantreg); library(splines)
  library(flextable); library(officer)
})

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
DATA_DIR <- here::here("data")
OUT_DIR  <- here::here("outputs", "tables"); if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
path_country <- file.path(DATA_DIR, "gbd2023_sdi_country_2023.csv.zip")
path_sdi     <- file.path(DATA_DIR, "gbd2023_sdi_values_1950_2023.csv")

cat("--- A4 Frontier bootstrap (1000 reps) ---\n")

structural <- c("Congenital heart anomalies","Neural tube defects","Down syndrome",
                "Other chromosomal abnormalities","Orofacial clefts",
                "Digestive congenital anomalies","Urogenital congenital anomalies",
                "Congenital musculoskeletal and limb anomalies",
                "Other congenital birth defects")
hemoglob   <- c("Sickle cell disorders","Thalassemias","G6PD deficiency",
                "Other hemoglobinopathies and hemolytic anemias")
all_13     <- c(structural, hemoglob)

df_raw <- read_csv(path_country, show_col_types = FALSE)
df_rate <- df_raw %>%
  filter(year == 2023, age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths", cause_name %in% all_13,
         metric_name == "Rate") %>%
  group_by(location_id, location_name) %>%
  summarise(total_rate = sum(val, na.rm = TRUE), .groups = "drop")
df_num <- df_raw %>%
  filter(year == 2023, age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths", cause_name %in% all_13,
         metric_name == "Number") %>%
  group_by(location_id) %>%
  summarise(total_deaths = sum(val, na.rm = TRUE), .groups = "drop")
# Join on location_id to avoid subnational homonyms in the SDI file.
df_sdi <- read_csv(path_sdi, show_col_types = FALSE) %>%
  filter(year_id == 2023) %>% dplyr::select(location_id, sdi = mean_value)

df_final <- df_rate %>%
  left_join(df_num, by = "location_id") %>%
  inner_join(df_sdi, by = "location_id") %>%
  filter(total_rate > 0) %>%
  drop_na(sdi)

n <- nrow(df_final)
cat("Countries fitted:", n, "\n")

# ---- Point estimate (matches Table_S6_Efficiency_Gap.R) ----
qr_main <- rq(log(total_rate) ~ ns(sdi, df = 3), tau = 0.05, data = df_final)
df_final$frontier_asmr  <- exp(predict(qr_main, df_final))
df_final$frontier_deaths <- df_final$total_deaths * (df_final$frontier_asmr / df_final$total_rate)
df_final$avoidable       <- pmax(df_final$total_deaths - df_final$frontier_deaths, 0)
point_total <- sum(df_final$avoidable)
country_total <- sum(df_final$total_deaths)
point_pct <- 100 * point_total / country_total
cat(sprintf("Point estimate: %s avoidable deaths (%.2f%% of %s)\n",
            format(round(point_total), big.mark=","),
            point_pct,
            format(round(country_total), big.mark=",")))

# ---- Bootstrap ----
B <- 1000
set.seed(20250512)
results <- numeric(B)
errs <- 0
pb <- txtProgressBar(min = 0, max = B, style = 3)
for (b in seq_len(B)) {
  idx <- sample.int(n, n, replace = TRUE)
  d_b <- df_final[idx, ]
  fit_ok <- tryCatch({
    fit_b <- rq(log(total_rate) ~ ns(sdi, df = 3), tau = 0.05, data = d_b)
    # apply to the ORIGINAL set so the "country avoidable deaths" are comparable
    pred_b <- exp(predict(fit_b, newdata = df_final))
    front_b <- df_final$total_deaths * (pred_b / df_final$total_rate)
    avoid_b <- pmax(df_final$total_deaths - front_b, 0)
    results[b] <- sum(avoid_b)
    TRUE
  }, error = function(e) { errs <<- errs + 1; FALSE })
  if (!fit_ok) results[b] <- NA_real_
  setTxtProgressBar(pb, b)
}
close(pb)
cat("Bootstrap fits with errors:", errs, "/", B, "\n")

results <- results[is.finite(results)]
ci <- quantile(results, c(0.025, 0.975))
mean_b <- mean(results); sd_b <- sd(results)
cat(sprintf("Bootstrap: mean = %s, 95%% CI = [%s, %s]\n",
            format(round(mean_b), big.mark=","),
            format(round(ci[1]), big.mark=","),
            format(round(ci[2]), big.mark=",")))

# --- Persist the full bootstrap distribution so reviewers can audit / re-derive
#     the 95% CI without re-running the 1,000-replicate refit (~5 min). ---
boot_out <- tibble(replicate = seq_along(results), avoidable_total = results)
saveRDS(boot_out, file.path(OUT_DIR, "Frontier_Bootstrap_distribution.rds"))
readr::write_csv(boot_out, file.path(OUT_DIR, "Frontier_Bootstrap_distribution.csv"))
cat("Bootstrap distribution saved to outputs/tables/Frontier_Bootstrap_distribution.{rds,csv}\n")

# --- Print-only summary (NOT a standalone supplementary table) ---
# The bootstrap CI is a single interval reported INLINE in the manuscript
# (Abstract + Results: "253,457 (95% CI 201,529-288,180)") and the procedure
# is described in Methods. It is a robustness check on the Figure 6 / Table S6
# frontier estimate, not a numbered supplementary table, so no docx is written.
cat("\n================ Frontier bootstrap CI (reproducibility) ================\n")
cat(sprintf("  Point estimate              : %s avoidable deaths\n", format(round(point_total),  big.mark=",")))
cat(sprintf("  Bootstrap mean              : %s\n",                  format(round(mean_b),       big.mark=",")))
cat(sprintf("  95%% CI                      : [%s, %s]\n",            format(round(ci[1]),        big.mark=","), format(round(ci[2]), big.mark=",")))
cat(sprintf("  Country-aggregated total    : %s\n",                  format(round(country_total),big.mark=",")))
cat(sprintf("  Point estimate, %% of total  : %.1f%%\n",              point_pct))
cat(sprintf("  Replicates B / failed fits  : %s / %d\n",             format(B, big.mark=","), errs))
cat(  "  -> These values are quoted verbatim in the manuscript text.\n")
cat(  "=========================================================================\n")
cat("\n=== Frontier bootstrap CI done ===\n")
