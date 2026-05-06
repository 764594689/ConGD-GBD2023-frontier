# ==============================================================================
# Figure 4: Structural and Compositional Dynamics Across SDI Quintiles (GBD 2023)
# 数据源：gbd2023_trend_1990_2023 (Panel A) + gbd2023_sdi_quintile_composition (Panel B)
# Updated from GBD 2021 -> GBD 2023, 13 individual causes
# ==============================================================================

# --- 安装缺失的包 ---
pkgs <- c("tidyverse", "patchwork", "ggalluvial", "cowplot")
for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(tidyverse)
library(patchwork)
library(ggalluvial)
library(cowplot)

# ==============================================================================
# 1. 路径
# ==============================================================================
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_trend  <- file.path(base_dir, "data/gbd2023_trend_1990_2023.csv.zip")
path_sdi_q  <- file.path(base_dir, "data/gbd2023_sdi_quintile_composition.csv.zip")
output_dir <- file.path(base_dir, "outputs/figures")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ==============================================================================
# 2. 读取数据
# ==============================================================================
cat("读取趋势数据 (Global)...\n")
df_trend <- read_csv(path_trend, show_col_types = FALSE)

cat("读取 SDI quintile 数据...\n")
df_sdi_q <- read_csv(path_sdi_q, show_col_types = FALSE)

# ==============================================================================
# 3. 调色盘与遗传病名单 (13 individual causes, GBD 2023)
# ==============================================================================
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
all_causes <- c(structural_list, hemoglobin_list)

# 为可读性，Panel A 合并为两大组 + 重点病种
# Panel B 展示个别血红蛋白病的 SDI 分布
my_palette <- c(
  "Congenital heart anomalies"       = "#E41A1C",
  "Neural tube defects"              = "#377EB8",
  "Down syndrome"                    = "#4DAF4A",
  "Other structural defects"         = "#A1D99B",
  "Sickle cell disorders"            = "#FD8D3C",
  "Thalassemias"                     = "#984EA3",
  "G6PD deficiency"                  = "#F768A1",
  "Other hemoglobinopathies and hemolytic anemias" = "#41B6C4"
)

anchor_years <- c(1990, 2000, 2010, 2023)

# ==============================================================================
# 4. Panel A 数据：Global baseline composition (from trend data)
# ==============================================================================
df_a_raw <- df_trend %>%
  filter(
    location_name == "Global",
    year         %in% anchor_years,
    age_name     == "<5 years",
    sex_name     == "Both",
    measure_name == "Deaths",
    metric_name  == "Number",
    cause_name   %in% all_causes
  )

# 合并小结构性病种为 "Other structural defects"
df_a <- df_a_raw %>%
  mutate(cause_display = case_when(
    cause_name == "Congenital heart anomalies" ~ "Congenital heart anomalies",
    cause_name == "Neural tube defects"        ~ "Neural tube defects",
    cause_name == "Down syndrome"              ~ "Down syndrome",
    cause_name %in% structural_list            ~ "Other structural defects",
    TRUE ~ cause_name
  )) %>%
  group_by(year, cause_display) %>%
  summarise(deaths = sum(val, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = deaths / sum(deaths) * 100) %>%
  ungroup() %>%
  mutate(cause_display = factor(cause_display, levels = names(my_palette)))

# ==============================================================================
# 5. Panel B 数据：SDI regions (排除大结构组，聚焦血红蛋白病)
#    SDI quintile 数据已包含 "Low SDI", "Low-middle SDI" 等 location_name
# ==============================================================================
sdi_levels <- c("Low SDI", "Low-middle SDI", "Middle SDI",
                "High-middle SDI", "High SDI")

# 排除大的 structural 类别，聚焦非结构性变化
df_b <- df_sdi_q %>%
  filter(
    location_name %in% sdi_levels,
    year %in% anchor_years,
    cause_name %in% hemoglobin_list
  ) %>%
  mutate(sdi_group = factor(location_name, levels = sdi_levels)) %>%
  group_by(year, sdi_group, cause_name) %>%
  summarise(deaths = sum(val, na.rm = TRUE), .groups = "drop") %>%
  group_by(year, sdi_group) %>%
  mutate(proportion = deaths / sum(deaths) * 100) %>%
  ungroup()

hemo_palette <- my_palette[hemoglobin_list]

# ==============================================================================
# 6. Panel A 绘图 (0-100% 堆叠柱状图，与原图一致)
# ==============================================================================
p4a <- ggplot(df_a, aes(x = factor(year), y = share, fill = cause_display)) +
  geom_col(width = 0.6, color = "white", linewidth = 0.3) +
  scale_fill_manual(values = my_palette, drop = FALSE) +
  scale_y_continuous(breaks = seq(0, 100, by = 20), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = "A", x = NULL, y = "Share (%)") +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title       = element_text(face = "bold", size = 22, hjust = 0),
    axis.text.x      = element_text(face = "bold"),
    legend.position  = "none",
    plot.margin      = margin(10, 5, 10, 10)
  )

# ==============================================================================
# 7. Panel B 绘图 (冲积图 / Alluvial — 血红蛋白病在 SDI 分组的变化)
# ==============================================================================
p4b <- ggplot(df_b, aes(x = factor(year), y = proportion,
                        stratum = cause_name, alluvium = cause_name,
                        fill = cause_name)) +
  geom_alluvium(alpha = 0.8, width = 0.35, curve_type = "sigmoid",
                color = "white", linewidth = 0.2) +
  geom_stratum(width = 0.35, color = "white", linewidth = 0.3) +
  facet_wrap(~sdi_group, nrow = 1) +
  scale_fill_manual(values = hemo_palette, drop = FALSE) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100.1)) +
  labs(title = "B", x = "Year", y = "Adjusted Share (%)") +
  theme_minimal(base_size = 18) +
  theme(
    strip.text       = element_text(face = "bold", size = 16),
    strip.background = element_rect(fill = "grey92", color = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(face = "bold", angle = 45, hjust = 1),
    plot.title         = element_text(face = "bold", size = 22, hjust = 0),
    legend.position    = "none"
  )

# ==============================================================================
# 8. 提取统一底部图例 (8个类别合并为2行)
# ==============================================================================
p_legend_temp <- ggplot(df_a, aes(x = factor(year), y = share, fill = cause_display)) +
  geom_col() +
  scale_fill_manual(name = NULL, values = my_palette, drop = FALSE) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.text  = element_text(size = 14)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

shared_legend <- get_legend(p_legend_temp)

# ==============================================================================
# 9. 最终拼图
# ==============================================================================
top_row <- (p4a | p4b) +
  plot_layout(widths = c(1.5, 4))

final_fig4 <- plot_grid(
  top_row,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.1)
)

# 保存
ggsave(file.path(output_dir, "Figure4_Composition.png"),
       final_fig4, width = 18, height = 8, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "Figure4_Composition.pdf"),
       final_fig4, width = 18, height = 8, dpi = 300, bg = "white")

cat("\nFigure 4 已保存至:", output_dir, "\n")

# ==============================================================================
# 10. 控制台输出供正文引用
# ==============================================================================
cat("\n=== Structural vs Hemoglobinopathy share (Global, 2023) ===\n")
global_2023 <- df_a_raw %>% filter(year == 2023)
struct_total <- global_2023 %>% filter(cause_name %in% structural_list) %>%
  summarise(s = sum(val, na.rm = TRUE)) %>% pull(s)
hemo_total <- global_2023 %>% filter(cause_name %in% hemoglobin_list) %>%
  summarise(s = sum(val, na.rm = TRUE)) %>% pull(s)
grand_total <- struct_total + hemo_total
cat(sprintf("  Structural: %.1f%%\n", struct_total / grand_total * 100))
cat(sprintf("  Hemoglobinopathies: %.1f%%\n", hemo_total / grand_total * 100))
