# ==============================================================================
# Figure 8: Kitagawa decomposition of the 1990-2023 change in ConGD U5 deaths
#   Components: population growth vs ASMR (epidemiology) change
#   Global + 21 GBD regions (top 22 by |change|)
# Output: outputs/figures/Figure8_Decomposition.png + .pdf
#         outputs/tables/Table_S14_Decomposition.csv
# ==============================================================================
library(tidyverse)

cat("--- Figure 8: Kitagawa decomposition (GBD 2023) ---\n")

# --- Paths ---
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir   <- here::here()
path_trend <- file.path(base_dir, "data/gbd2023_trend_1990_2023.csv.zip")
fig_dir    <- file.path(base_dir, "outputs/figures")
tab_dir    <- file.path(base_dir, "outputs/tables")
for (d in c(fig_dir, tab_dir)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

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
  # Number ÷ Rate × 100,000 therefore recovers GBD's own under-5 population
  # estimate, which is internally consistent with the mortality numerator
  # (Number = Rate × Pop / 1e5 exactly). This denominator differs from UN WPP
  # under-5 population estimates by ~1.5-2% at single years globally; both
  # series are demographically reasonable and the Kitagawa decomposition
  # below is exact within the chosen data ecosystem.
  mutate(under5_pop = Number / Rate * 1e5)

# Kitagawa: ΔD = ΔP × mean(R) + mean(P) × ΔR
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
    rate_effect = (R_2023 - R_1990) * ((P_1990 + P_2023) / 2) / 1e5,
    residual    = dD - pop_effect - rate_effect,
    pop_share   = pop_effect / dD * 100,
    rate_share  = rate_effect / dD * 100
  ) %>%
  arrange(location_name)

# (Tabular S15 output is produced separately by
#  R/03_supplementary_tables/Table_S14_Decomposition.R as a formatted .docx;
#  this script produces only Figure 8.)

plot_df <- decomp %>%
  pivot_longer(c(pop_effect, rate_effect),
               names_to = "component", values_to = "delta") %>%
  mutate(component = recode(component,
                            "pop_effect"  = "Population growth",
                            "rate_effect" = "ASMR change"))

plot_locs <- plot_df %>%
  group_by(location_name) %>% summarise(absmax = sum(abs(delta))) %>%
  arrange(desc(absmax)) %>% head(22) %>% pull(location_name)

# --- Net-change label per location (Style A convention in published GBD
#     decomposition figures: pop + rate effects shown as stacked bars, net
#     change shown as a numeric label to the right of each row.)
fmt_signed <- function(x) {
  s <- ifelse(x >= 0, "+", "−")  # U+2212 minus
  paste0(s, formatC(abs(round(x)), format = "d", big.mark = ","))
}
fmt_axis <- function(x) {
  out <- formatC(x, format = "d", big.mark = ",", drop0trailing = TRUE)
  gsub("-", "−", out, fixed = TRUE)
}

plot_df_top <- plot_df %>%
  filter(location_name %in% plot_locs) %>%
  mutate(location_name = factor(location_name, levels = rev(plot_locs)))

net_df <- plot_df_top %>%
  group_by(location_name) %>%
  summarise(net = sum(delta), .groups = "drop")

x_range <- range(plot_df_top$delta)
x_span  <- x_range[2] - x_range[1]
# Anchor net-change labels in a tidy column to the right of the most-positive
# bar, with a comfortable gap so the green bars never touch the text.
label_x <- x_range[2] + x_span * 0.08

p <- ggplot(plot_df_top, aes(y = location_name, x = delta, fill = component)) +
  geom_col() +
  geom_vline(xintercept = 0, color = "grey30") +
  geom_text(data = net_df,
            aes(y = location_name, x = label_x, label = fmt_signed(net)),
            inherit.aes = FALSE, hjust = 0, size = 3.3,
            color = "grey20", family = "sans") +
  scale_fill_manual(values = c("Population growth" = "#A1D99B",
                               "ASMR change" = "#FB6A4A")) +
  scale_x_continuous(labels = fmt_axis,
                     expand = expansion(mult = c(0.02, 0.28))) +
  labs(x = "Change in ConGD under-5 deaths, 1990–2023",
       y = NULL, fill = "Component") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    axis.title       = element_text(face = "bold"),
    axis.line.x      = element_line(color = "black"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "Figure8_Decomposition.png"), p,
       width = 10, height = 8, dpi = 600, bg = "white")
ggsave(file.path(fig_dir, "Figure8_Decomposition.pdf"), p,
       width = 10, height = 8, bg = "white")
cat("Figure 8 saved to:", fig_dir, "\n")
