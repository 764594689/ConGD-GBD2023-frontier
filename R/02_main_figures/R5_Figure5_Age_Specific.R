# ==============================================================================
# Figure 5 (REVISED per reviewer): Age-Specific Mortality Patterns (GBD 2023)
# Panel A: Mortality density (deaths per day) pyramid  [unchanged except label]
# Panel B: Age-specific composition AS A SHARE OF ALL-CAUSE under-5 mortality,
#          now including an "All other under-5 causes" band (reviewer request).
#
# NEW DATA REQUIRED (extract once from GBD/GHDx, place in data/):
#   data/gbd2023_age_specific_allcause_2023.csv.zip
#   Query: Measure=Deaths, Metric=Number, Cause="All causes",
#          Location=Global, Year=2023, Sex=Both,
#          Age = the six under-5 bands (0-6 days, 7-27 days, 1-5 months,
#                6-11 months, 12-23 months, 2-4 years).
# ==============================================================================
library(tidyverse)
library(scales)
library(patchwork)

cat("--- Figure 5 (REVISED): Age-Specific Patterns (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir   <- here::here()
path_age   <- file.path(base_dir, "data/gbd2023_age_specific_2023.csv.zip")
path_all   <- file.path(base_dir, "data/gbd2023_age_specific_allcause_2023.csv.zip")  # NEW
output_dir <- file.path(base_dir, "outputs/figures")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

HEMO_LABEL <- "Hemoglobinopathies & hemolytic anemias"   # reviewer: precise GBD label
OTHER_LABEL <- "All other under-5 causes"

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

age_recode <- function(d) {
  d %>%
    mutate(age_name = str_trim(age_name)) %>%
    mutate(age_label = case_when(
      age_name %in% c("Early Neonatal", "0-6 days", "<7 days")   ~ "Early Neonatal\n(0–6d)",
      age_name %in% c("Late Neonatal", "7-27 days")              ~ "Late Neonatal\n(7–27d)",
      age_name %in% c("1-5 months", "6-11 months", "28-364 days",
                       "Post Neonatal")                          ~ "Post-neonatal\n(28d–1y)",
      age_name %in% c("12 to 23 months", "12-23 months",
                       "2-4 years", "2 to 4", "1-4 years")       ~ "Child\n(1–4y)",
      TRUE ~ "Exclude")) %>%
    filter(age_label != "Exclude") %>%
    mutate(days_in_period = case_when(
      age_label == "Early Neonatal\n(0–6d)"  ~ 7,
      age_label == "Late Neonatal\n(7–27d)"  ~ 21,
      age_label == "Post-neonatal\n(28d–1y)" ~ 337,
      age_label == "Child\n(1–4y)"           ~ 1460,
      TRUE ~ 1))
}

signed_sqrt_trans <- function() {
  scales::trans_new("signed_sqrt",
                    transform = function(x) sign(x) * sqrt(abs(x)),
                    inverse   = function(x) sign(x) * (x^2))
}

# --- Read selected-cause data (the 18 causes) ---
df_raw <- read_csv(path_age, show_col_types = FALSE)
df_base <- df_raw %>%
  {if ("metric_name" %in% names(.)) filter(., metric_name == "Number") else .} %>%
  {if ("measure_name" %in% names(.)) filter(., measure_name == "Deaths") else .} %>%
  {if ("sex_name" %in% names(.)) filter(., sex_name == "Both") else .} %>%
  mutate(cause_name = str_trim(cause_name)) %>%
  age_recode() %>%
  mutate(category = case_when(
    cause_name %in% structural_list ~ "Structural Birth Defects",
    cause_name %in% hemoglobin_list ~ HEMO_LABEL,
    cause_name %in% infectious_list ~ "Common Infections & Nutrition",
    TRUE ~ "Drop")) %>%
  filter(category != "Drop")

df_summary <- df_base %>%
  group_by(age_label, category, days_in_period) %>%
  summarise(total_deaths = sum(val, na.rm = TRUE), .groups = "drop") %>%
  mutate(daily_deaths = total_deaths / days_in_period)

age_levels <- c("Child\n(1–4y)", "Post-neonatal\n(28d–1y)",
                "Late Neonatal\n(7–27d)", "Early Neonatal\n(0–6d)")
df_summary <- df_summary %>% mutate(age_label = factor(age_label, levels = age_levels))

color_pal <- c(
  "Common Infections & Nutrition" = "#E41A1C",
  "Structural Birth Defects"      = "#377EB8",
  "#984EA3" = "#984EA3"  # placeholder replaced below
)
color_pal <- c(
  "Common Infections & Nutrition" = "#E41A1C",
  "Structural Birth Defects"      = "#377EB8")
color_pal[HEMO_LABEL]  <- "#984EA3"
color_pal[OTHER_LABEL] <- "#BDBDBD"

# ==============================================================================
# Panel A: pyramid (unchanged structure; only the hemoglobinopathy label updated)
# ==============================================================================
df_pyramid <- df_summary %>%
  mutate(plot_deaths = ifelse(category == "Common Infections & Nutrition",
                              daily_deaths, -daily_deaths))

# Purple hemoglobinopathy labels: placed just left of the (outer) purple bar,
# matching the manual annotation in the submitted figure. The outer left edge of
# each row is -(structural + hemoglobinopathy) daily deaths.
df_hemo_lab <- df_summary %>%
  filter(category %in% c("Structural Birth Defects", HEMO_LABEL)) %>%
  group_by(age_label) %>%
  # s = structural + hemoglobinopathy deaths/day = the purple bar's OUTER (left) edge.
  # We anchor the label a constant distance (in signed-sqrt display space) left of that
  # edge and right-align it (hjust = 1), so every purple number sits the same distance
  # from its purple bar — the same intent as the red labels keeping a fixed gap from
  # their bars. (hjust > 1, the direct mirror of the red hjust = -0.15, drops trailing
  # digits on this transformed axis, so we offset the anchor instead.)
  summarise(s = sum(daily_deaths),
            hemo = daily_deaths[category == HEMO_LABEL], .groups = "drop") %>%
  # geom_text left-aligns at the anchor on this transformed axis (hjust is not
  # honoured), so the text extends RIGHT toward the bar. To keep a UNIFORM gap
  # between every number's right edge and its purple bar, we offset the anchor by
  # a base gap PLUS an allowance for the label's own width (≈ digit count).
  mutate(lab = comma(round(hemo)),
         lab_x = -(sqrt(s) + 4 + nchar(lab) * 5.4 + ifelse(nchar(lab) >= 3, 6, 0))^2)

plot_A <- ggplot(df_pyramid, aes(x = plot_deaths, y = age_label, fill = category)) +
  geom_col(width = 0.65, color = "white", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +
  geom_text(data = filter(df_pyramid, category == "Structural Birth Defects"),
            aes(label = comma(round(abs(plot_deaths)))),
            hjust = 0.5, size = 3.2, fontface = "bold",
            position = position_stack(vjust = 0.5)) +
  geom_text(data = filter(df_pyramid, category == "Common Infections & Nutrition"),
            aes(label = comma(round(abs(plot_deaths)))),
            hjust = -0.15, size = 3.2, fontface = "bold", color = "#E41A1C") +
  # purple (hemoglobinopathy) labels, just outside the left end of the purple bar
  geom_text(data = df_hemo_lab, inherit.aes = FALSE,
            aes(x = lab_x, y = age_label, label = lab),
            hjust = 0, size = 3.5, fontface = "bold", color = "#984EA3") +
  scale_fill_manual(values = color_pal) +
  scale_x_continuous(trans = signed_sqrt_trans(),
                     breaks = c(-25000, -10000, -2500, 0, 2500, 10000, 25000),
                     labels = function(x) comma(abs(x)),
                     expand = expansion(mult = c(0.34, 0.15))) +
  coord_cartesian(clip = "off") +
  labs(title = "A", x = NULL, y = NULL, fill = "Disease Category") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 20, hjust = 0),
        panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        axis.text.x = element_text(face = "bold", color = "grey30"),
        axis.text.y = element_text(face = "bold", color = "black", size = 11)) +
  guides(fill = guide_legend(nrow = 1))

# ==============================================================================
# Panel B: share of ALL-CAUSE under-5 mortality (adds "All other causes")
# ==============================================================================
df_all <- read_csv(path_all, show_col_types = FALSE) %>%
  {if ("metric_name" %in% names(.)) filter(., metric_name == "Number") else .} %>%
  {if ("measure_name" %in% names(.)) filter(., measure_name == "Deaths") else .} %>%
  {if ("sex_name" %in% names(.)) filter(., sex_name == "Both") else .} %>%
  age_recode() %>%
  group_by(age_label, days_in_period) %>%
  summarise(allcause_deaths = sum(val, na.rm = TRUE), .groups = "drop") %>%
  mutate(allcause_daily = allcause_deaths / days_in_period)

named_daily <- df_summary %>%
  group_by(age_label) %>%
  summarise(named_daily = sum(daily_deaths), .groups = "drop")

df_other <- df_all %>%
  left_join(named_daily, by = "age_label") %>%
  mutate(daily_deaths = pmax(allcause_daily - named_daily, 0),
         category = OTHER_LABEL) %>%
  dplyr::select(age_label, category, daily_deaths)

df_propB <- bind_rows(
    df_summary %>% dplyr::select(age_label, category, daily_deaths),
    df_other) %>%
  group_by(age_label) %>%
  mutate(prop = daily_deaths / sum(daily_deaths)) %>%
  ungroup() %>%
  mutate(category = factor(category, levels = c(
    "Structural Birth Defects", HEMO_LABEL,
    "Common Infections & Nutrition", OTHER_LABEL)),
    age_label = factor(age_label, levels = age_levels))

plot_B <- ggplot(df_propB, aes(x = prop, y = age_label, fill = category)) +
  geom_col(position = "fill", width = 0.6, color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(prop >= 0.03, percent(prop, accuracy = 1), "")),
            position = position_fill(vjust = 0.5),
            size = 4, fontface = "bold", color = "white") +
  scale_fill_manual(name = "Disease Category", values = color_pal) +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.045))) +
  labs(title = "B", x = "Share of all-cause under-5 deaths (%)", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 20, hjust = 0),
        panel.grid = element_blank(),
        axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black", size = 11),
        plot.margin = margin(5.5, 16, 5.5, 5.5)) +
  guides(fill = guide_legend(nrow = 2))

final_fig5 <- plot_A / plot_B + plot_layout(heights = c(1.5, 1))
ggsave(file.path(output_dir, "Figure5_Age_Specific.png"), final_fig5,
       width = 12, height = 10.5, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "Figure5_Age_Specific.pdf"), final_fig5,
       width = 12, height = 10.5, dpi = 300, bg = "white")

# --- Panel A daily-death values (Panel A omits the purple hemoglobinopathy
#     numeric labels in the plot; printed here for manual annotation in AI/PPT) ---
cat("\n--- Panel A: daily deaths by category and age (deaths/day) ---\n")
print(as.data.frame(df_summary %>% arrange(desc(age_label), category) %>%
        dplyr::select(age_label, category, total_deaths, daily_deaths)))
cat("\n--- Hemoglobinopathy values (purple bars, for manual annotation) ---\n")
print(as.data.frame(df_summary %>% filter(category == HEMO_LABEL) %>%
        arrange(desc(age_label)) %>%
        dplyr::select(age_label, daily_deaths, total_deaths)))

# --- New Panel B percentages for the manuscript ---
cat("\n--- Panel B: share of ALL-CAUSE under-5 mortality ---\n")
print(as.data.frame(df_propB %>% arrange(desc(age_label), category) %>%
        mutate(pct = round(prop * 100, 1)) %>%
        dplyr::select(age_label, category, pct)))
cat("\nFigure 5 (revised) saved to:", output_dir, "\n")
