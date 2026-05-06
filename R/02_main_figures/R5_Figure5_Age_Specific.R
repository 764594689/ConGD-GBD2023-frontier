# ==============================================================================
# Figure 5: Age-Specific Mortality Patterns (GBD 2023)
# Panel A: Mortality density (deaths per day) pyramid
# Panel B: Age-specific proportional composition
# ==============================================================================
library(tidyverse)
library(scales)
library(patchwork)

cat("--- Figure 5: Age-Specific Patterns (GBD 2023) ---\n")

# --- Paths ---
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_age   <- file.path(base_dir, "data/gbd2023_age_specific_2023.csv.zip")
output_dir <- file.path(base_dir, "outputs/figures")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- Disease classification ---
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
infectious_list <- c(
  "Enteric infections", "Respiratory infections and tuberculosis",
  "Nutritional deficiencies", "Other infectious diseases", "Malaria"
)

# --- Read data ---
cat("Reading age-specific data...\n")
df_raw <- read_csv(path_age, show_col_types = FALSE)

# --- Signed sqrt transform ---
signed_sqrt_trans <- function() {
  scales::trans_new("signed_sqrt",
                    transform = function(x) sign(x) * sqrt(abs(x)),
                    inverse   = function(x) sign(x) * (x^2))
}

# --- Data processing ---
df_base <- df_raw %>%
  {if ("metric_name" %in% names(.)) filter(., metric_name == "Number") else .} %>%
  {if ("measure_name" %in% names(.)) filter(., measure_name == "Deaths") else .} %>%
  {if ("sex_name" %in% names(.)) filter(., sex_name == "Both") else .} %>%
  mutate(age_name = str_trim(age_name), cause_name = str_trim(cause_name)) %>%
  mutate(age_label = case_when(
    age_name %in% c("Early Neonatal", "0-6 days", "<7 days")       ~ "Early Neonatal\n(0\u20136d)",
    age_name %in% c("Late Neonatal", "7-27 days")                   ~ "Late Neonatal\n(7\u201327d)",
    age_name %in% c("1-5 months", "6-11 months", "28-364 days",
                     "Post Neonatal")                                ~ "Post-neonatal\n(28d\u20131y)",
    age_name %in% c("12 to 23 months", "12-23 months",
                     "2-4 years", "2 to 4", "1-4 years")            ~ "Child\n(1\u20134y)",
    TRUE ~ "Exclude"
  )) %>%
  filter(age_label != "Exclude") %>%
  mutate(days_in_period = case_when(
    age_label == "Early Neonatal\n(0\u20136d)"  ~ 7,
    age_label == "Late Neonatal\n(7\u201327d)"  ~ 21,
    age_label == "Post-neonatal\n(28d\u20131y)" ~ 337,
    age_label == "Child\n(1\u20134y)"           ~ 1460,
    TRUE ~ 1
  )) %>%
  mutate(category = case_when(
    cause_name %in% structural_list ~ "Structural Birth Defects",
    cause_name %in% hemoglobin_list ~ "Hemoglobinopathies",
    cause_name %in% infectious_list ~ "Common Infections & Nutrition",
    TRUE ~ "Others"
  )) %>%
  filter(category != "Others")

# Summary
df_summary <- df_base %>%
  group_by(age_label, category, days_in_period) %>%
  summarise(total_deaths = sum(val, na.rm = TRUE), .groups = "drop") %>%
  mutate(daily_deaths = total_deaths / days_in_period) %>%
  mutate(age_label = factor(age_label, levels = c(
    "Child\n(1\u20134y)", "Post-neonatal\n(28d\u20131y)",
    "Late Neonatal\n(7\u201327d)", "Early Neonatal\n(0\u20136d)"
  )))

# ==============================================================================
# Panel A: Pyramid (Structural + Hemoglobinopathies left, Infections right)
# Structural label: white inside blue bar
# Infection label: red outside right
# Hemoglobinopathy label: omitted from plot (see console output below)
# ==============================================================================
df_pyramid <- df_summary %>%
  mutate(plot_deaths = ifelse(category == "Common Infections & Nutrition",
                              daily_deaths, -daily_deaths))

color_pal <- c(
  "Common Infections & Nutrition" = "#E41A1C",
  "Structural Birth Defects"      = "#377EB8",
  "Hemoglobinopathies"            = "#984EA3"
)

plot_A <- ggplot(df_pyramid, aes(x = plot_deaths, y = age_label, fill = category)) +
  geom_col(width = 0.65, color = "white", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +
  # Structural labels: white, centered inside blue bar
  geom_text(data = filter(df_pyramid, category == "Structural Birth Defects"),
            aes(label = comma(round(abs(plot_deaths)))),
            hjust = 0.5, size = 3.2, fontface = "bold",
            position = position_stack(vjust = 0.5)) +
  # Infection labels: red, outside right
  geom_text(data = filter(df_pyramid, category == "Common Infections & Nutrition"),
            aes(label = comma(round(abs(plot_deaths)))),
            hjust = -0.15, size = 3.2, fontface = "bold", color = "#E41A1C") +
  scale_fill_manual(values = color_pal) +
  scale_x_continuous(
    trans = signed_sqrt_trans(),
    breaks = c(-25000, -10000, -2500, 0, 2500, 10000, 25000),
    labels = function(x) comma(abs(x)),
    expand = expansion(mult = c(0.15, 0.15))
  ) +
  labs(
    title = "A",
    x = NULL, y = NULL, fill = "Disease Category"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold", size = 14, hjust = 0),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x        = element_text(face = "bold", color = "grey30"),
    axis.text.y        = element_text(face = "bold", color = "black", size = 11)
  ) +
  guides(fill = guide_legend(nrow = 1))

# ==============================================================================
# Panel B: Proportional composition
# ==============================================================================
df_prop <- df_summary %>%
  group_by(age_label) %>%
  mutate(prop = daily_deaths / sum(daily_deaths)) %>%
  ungroup() %>%
  mutate(category = factor(category, levels = c(
    "Structural Birth Defects", "Hemoglobinopathies", "Common Infections & Nutrition"
  )))

plot_B <- ggplot(df_prop, aes(x = prop, y = age_label, fill = category)) +
  geom_col(position = "fill", width = 0.6, color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(prop >= 0.03, percent(prop, accuracy = 1), "")),
            position = position_fill(vjust = 0.5),
            size = 4, fontface = "bold", color = "white") +
  scale_fill_manual(name = "Disease Category", values = color_pal) +
  scale_x_continuous(labels = percent, expand = c(0, 0)) +
  labs(
    title = "B",
    x = "Proportion of Deaths (%)", y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 14, hjust = 0),
    panel.grid       = element_blank(),
    axis.text.x      = element_text(face = "bold", color = "black"),
    axis.text.y      = element_text(face = "bold", color = "black", size = 11)
  ) +
  guides(fill = guide_legend(nrow = 1))

# --- Combine ---
final_fig5 <- plot_A / plot_B +
  plot_layout(heights = c(1.5, 1))

ggsave(file.path(output_dir, "Figure5_Age_Specific.png"),
       final_fig5, width = 12, height = 10, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "Figure5_Age_Specific.pdf"),
       final_fig5, width = 12, height = 10, dpi = 300, bg = "white")

# ==============================================================================
# Console output: Hemoglobinopathy values for manual annotation
# ==============================================================================
cat("\n====================================================\n")
cat("  FIGURE 5 - Key values for manuscript\n")
cat("====================================================\n")

cat("\n--- Panel A: Daily deaths by category and age ---\n")
df_print <- df_summary %>%
  arrange(desc(age_label), category) %>%
  select(age_label, category, total_deaths, daily_deaths)
print(as.data.frame(df_print))

cat("\n--- Hemoglobinopathy values (for manual annotation in AI/PPT) ---\n")
df_hemo <- df_summary %>%
  filter(category == "Hemoglobinopathies") %>%
  arrange(desc(age_label)) %>%
  select(age_label, daily_deaths_hemo = daily_deaths, total_deaths_hemo = total_deaths)
print(as.data.frame(df_hemo))

cat("\n--- Panel B: Proportions ---\n")
df_prop_print <- df_prop %>%
  arrange(desc(age_label), category) %>%
  mutate(pct = round(prop * 100, 1)) %>%
  select(age_label, category, pct)
print(as.data.frame(df_prop_print))

# Combined CGD percentages
cgd_neo_pct <- df_prop %>%
  filter(category != "Common Infections & Nutrition", grepl("0.6d", age_label)) %>%
  summarise(pct = sum(prop) * 100) %>% pull(pct)
cgd_child_pct <- df_prop %>%
  filter(category != "Common Infections & Nutrition", grepl("1.4y", age_label)) %>%
  summarise(pct = sum(prop) * 100) %>% pull(pct)
cat(sprintf("\nConGDs share in early neonatal = %.0f%%\n", cgd_neo_pct))
cat(sprintf("ConGDs share in 1-4y          = %.0f%%\n", cgd_child_pct))
cat("====================================================\n")
cat("Figure 5 saved to:", output_dir, "\n")
