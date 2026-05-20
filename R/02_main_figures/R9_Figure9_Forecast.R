# ==============================================================================
# Figure 9: ARIMA forecast of Global ConGD under-5 ASMR to 2035
#   Composite (13) + Structural birth defects (9) + Hemoglobinopathies (4)
#   ARIMA random-walk-with-drift on log-ASMR (Frequentist counterpart of a
#   single-age-band BAPC RW2 projection).
# Output: outputs/figures/Figure9_Forecast.png + .pdf
#         outputs/tables/Table_Forecast_2023_2030_2035.csv
# ==============================================================================
library(tidyverse)
if (!requireNamespace("forecast", quietly = TRUE)) install.packages("forecast")
library(forecast)

cat("--- Figure 9: ARIMA forecast to 2035 (GBD 2023) ---\n")

# --- Paths ---
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir   <- here::here()
path_trend <- file.path(base_dir, "data/gbd2023_trend_1990_2023.csv.zip")
fig_dir    <- file.path(base_dir, "outputs/figures")
tab_dir    <- file.path(base_dir, "outputs/tables")
for (d in c(fig_dir, tab_dir)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

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

df <- read_csv(path_trend, show_col_types = FALSE) %>%
  filter(location_name == "Global", age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths", metric_name == "Rate")

asmr_yr <- df %>%
  group_by(year, cause_set = case_when(
    cause_name %in% structural_list ~ "Structural",
    cause_name %in% hemoglobin_list ~ "Hemoglobinopathies",
    TRUE                            ~ "Other"
  )) %>%
  summarise(asmr = sum(val, na.rm = TRUE), .groups = "drop") %>%
  filter(cause_set != "Other")

all_yr <- bind_rows(
  asmr_yr,
  asmr_yr %>% group_by(year) %>% summarise(asmr = sum(asmr), .groups = "drop") %>%
    mutate(cause_set = "ConGD composite")
)

forecast_until <- 2035
years_obs <- sort(unique(all_yr$year))
horizon   <- forecast_until - max(years_obs)

out <- list()
for (cs in c("ConGD composite", "Structural", "Hemoglobinopathies")) {
  ser    <- all_yr %>% filter(cause_set == cs) %>% arrange(year)
  ts_obj <- ts(ser$asmr, start = min(ser$year), frequency = 1)
  fit    <- auto.arima(log(ts_obj), seasonal = FALSE, trace = TRUE)
  fc     <- forecast(fit, h = horizon, level = 95)
  raw_df <- tibble(year = ser$year, rate = ser$asmr, lo = NA_real_,
                   hi = NA_real_, observed = TRUE, cause_set = cs)
  fc_df <- tibble(
    year = (max(ser$year) + 1):forecast_until,
    rate = exp(as.numeric(fc$mean)),
    lo   = exp(as.numeric(fc$lower[, 1])),
    hi   = exp(as.numeric(fc$upper[, 1])),
    observed = FALSE, cause_set = cs
  )
  out[[cs]] <- bind_rows(raw_df, fc_df)
  cat(sprintf("  %s : ARIMA(%d,%d,%d); 2035 = %.2f (95%% PI %.2f-%.2f)\n",
              cs, fit$arma[1], fit$arma[6], fit$arma[2],
              tail(exp(as.numeric(fc$mean)), 1),
              tail(exp(as.numeric(fc$lower[,1])), 1),
              tail(exp(as.numeric(fc$upper[,1])), 1)))
}
forecast_df <- bind_rows(out)

# Forecast values for 2023/2030/2035 are reported inline in the manuscript
# Results (no separate supplementary table); this script produces only Figure 9.
summary_tbl <- forecast_df %>%
  filter(year %in% c(2023, 2030, 2035)) %>%
  dplyr::select(cause_set, year, rate, lo, hi) %>%
  arrange(cause_set, year)
cat("Forecast summary (printed only):\n")
print(as.data.frame(summary_tbl))

y_top <- max(forecast_df$hi, forecast_df$rate, na.rm = TRUE) * 1.05
p <- ggplot(forecast_df, aes(x = year, y = rate, color = cause_set, fill = cause_set)) +
  geom_ribbon(data = filter(forecast_df, !observed),
              aes(ymin = lo, ymax = hi), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(data = filter(forecast_df, observed), size = 1.2) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "grey40") +
  # "Observed" / "Forecast" panel annotations bracketing the 2023 boundary
  annotate("text", x = 2006, y = y_top, label = "Observed",
           hjust = 0.5, vjust = 1, fontface = "italic",
           color = "grey30", size = 4) +
  annotate("text", x = 2029, y = y_top, label = "Forecast",
           hjust = 0.5, vjust = 1, fontface = "italic",
           color = "grey30", size = 4) +
  scale_x_continuous(breaks = seq(1990, 2035, 5)) +
  scale_y_continuous(limits = c(0, y_top)) +
  labs(x = "Year", y = "ASMR per 100,000 (under-5)",
       color = "Series", fill = "Series") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    axis.title       = element_text(face = "bold"),
    axis.line        = element_line(color = "black"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "Figure9_Forecast.png"), p,
       width = 10, height = 6, dpi = 600, bg = "white")
ggsave(file.path(fig_dir, "Figure9_Forecast.pdf"), p,
       width = 10, height = 6, bg = "white")
cat("Figure 9 saved to:", fig_dir, "\n")
