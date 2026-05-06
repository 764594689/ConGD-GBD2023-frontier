# ==============================================================================
# Figure 2: Temporal Trends and Rank Evolution of Genetic Disorders (1990-2023)
# 数据源：gbd2023_trend_1990_2023 (Panel A) + gbd2023_rank_causes (Panel B)
# Updated from GBD 2021 -> GBD 2023, 13 individual causes
# ==============================================================================

# --- 安装缺失的包 ---
pkgs <- c("tidyverse", "ggbump", "patchwork", "scales")
for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(tidyverse)
library(ggbump)
library(patchwork)
library(scales)

# ==============================================================================
# 1. 数据路径
# ==============================================================================
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
path_batch2 <- file.path(base_dir, "data/gbd2023_trend_1990_2023.csv.zip")
path_batch3 <- file.path(base_dir, "data/gbd2023_rank_causes.csv.zip")
output_dir <- file.path(base_dir, "outputs/figures")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ==============================================================================
# 2. 读取数据
# ==============================================================================
cat("读取趋势数据...\n")
df_batch2 <- read_csv(path_batch2, show_col_types = FALSE)

cat("读取排名数据...\n")
df_batch3 <- read_csv(path_batch3, show_col_types = FALSE)

# ==============================================================================
# 3. 定义遗传病白名单 (13个 GBD 2023 individual causes)
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
target_genetic <- c(structural_list, hemoglobin_list)

# ==============================================================================
# 4. Panel A 数据：全球 PMR 趋势 (1990-2023)
#    从趋势数据中提取 Global 的 Number 数据
# ==============================================================================

df_a_base <- df_batch2 %>%
  filter(
    location_name == "Global",
    age_name      == "<5 years",
    sex_name      == "Both",
    measure_name  == "Deaths",
    metric_name   == "Number"
  )

# 全死因 (分母)
df_all_deaths <- df_a_base %>%
  filter(cause_name == "All causes") %>%
  select(year, all_deaths = val)

# 遗传病总死亡数 (分子1)
df_genetic_total <- df_a_base %>%
  filter(cause_name %in% target_genetic) %>%
  group_by(year) %>%
  summarise(deaths = sum(val, na.rm = TRUE), .groups = "drop") %>%
  mutate(cause_group = "Total Genetic Disorders")

# CMNN 总死亡数 (分子2) — 用 str_detect 处理含逗号的 cause_name
df_cmnn_total <- df_a_base %>%
  filter(str_detect(cause_name, "Communicable, maternal")) %>%
  select(year, deaths = val) %>%
  mutate(cause_group = "CMNN (Total)")

# 合并计算 PMR
df_panel_a <- bind_rows(df_genetic_total, df_cmnn_total) %>%
  left_join(df_all_deaths, by = "year") %>%
  mutate(pmr = deaths / all_deaths)

# ==============================================================================
# 5. Panel B 数据：排名凹凸图
#    从排名数据中提取（Global, 关键年份, Number）
# ==============================================================================

df_b_base <- df_batch3 %>%
  filter(
    age_name     == "<5 years",
    sex_name     == "Both",
    measure_name == "Deaths",
    metric_name  == "Number"
  )

# 个体死因（用于排名对比）
rank_causes <- c(
  "Neonatal disorders",
  "Respiratory infections and tuberculosis",
  "Enteric infections",
  "Nutritional deficiencies",
  "Malaria",
  "Other infectious diseases",
  "Unintentional injuries"
)

# 检查排名数据中可用的 cause_name
available_rank_causes <- intersect(rank_causes, unique(df_b_base$cause_name))

df_individual <- df_b_base %>%
  filter(cause_name %in% available_rank_causes) %>%
  select(year, cause = cause_name, deaths = val)

# 遗传病总量（从排名数据中汇总 13 causes）
df_genetic_rank <- df_b_base %>%
  filter(cause_name %in% target_genetic) %>%
  group_by(year) %>%
  summarise(deaths = sum(val, na.rm = TRUE), .groups = "drop") %>%
  mutate(cause = "Total Genetic Disorders")

# 合并排名
key_years <- c(1990, 2000, 2010, 2023)

df_panel_b <- bind_rows(df_individual, df_genetic_rank) %>%
  filter(year %in% key_years) %>%
  group_by(year) %>%
  mutate(rank = rank(-deaths, ties.method = "first")) %>%
  ungroup() %>%
  mutate(is_genetic = ifelse(cause == "Total Genetic Disorders", "Yes", "No"))

# ==============================================================================
# 6. 画图
# ==============================================================================

theme_pub <- theme_minimal(base_size = 22) +
  theme(
    plot.title       = element_text(face = "bold", size = 26, hjust = 0),
    plot.subtitle    = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position  = "none"
  )

# --- Panel A: 剪刀差趋势图 ---
plot_a <- ggplot(df_panel_a, aes(x = year, y = pmr, color = cause_group)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c(
    "Total Genetic Disorders" = "#0072B2",
    "CMNN (Total)" = "#E69F00"
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2023),
                     expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    title    = "A",
    subtitle = NULL,
    x = "Year",
    y = "Proportional Mortality Ratio (%)"
  ) +
  geom_text(
    data = filter(df_panel_a, year == max(df_panel_a$year) - 6 & cause_group == "CMNN (Total)"),
    aes(label = cause_group),
    hjust = 0.5, nudge_y = -0.04, fontface = "bold", size = 7
  ) +
  geom_text(
    data = filter(df_panel_a, year == max(df_panel_a$year) - 6 & cause_group == "Total Genetic Disorders"),
    aes(label = cause_group),
    hjust = 0.5, nudge_y = 0.04, fontface = "bold", size = 7
  ) +
  theme_pub +
  theme(plot.margin = margin(20, 10, 20, 10))

# --- Panel B: 排名凹凸图 ---
plot_b <- ggplot(df_panel_b, aes(x = year, y = rank, color = is_genetic, group = cause)) +
  geom_bump(data = filter(df_panel_b, is_genetic == "No"),
            linewidth = 1.2, color = "grey80") +
  geom_point(data = filter(df_panel_b, is_genetic == "No"),
             size = 4, color = "grey80") +
  geom_bump(data = filter(df_panel_b, is_genetic == "Yes"),
            linewidth = 2.5, color = "#0072B2") +
  geom_point(data = filter(df_panel_b, is_genetic == "Yes"),
             size = 5, color = "#0072B2") +
  scale_y_reverse(breaks = 1:max(df_panel_b$rank)) +
  scale_x_continuous(
    breaks = key_years,
    limits = c(1975, 2039),
    expand = c(0, 0)
  ) +
  geom_text(
    data = filter(df_panel_b, year == min(key_years)),
    aes(label = str_wrap(cause, width = 18)),
    hjust = 1, nudge_x = -1.5, size = 6.5, fontface = "bold"
  ) +
  geom_text(
    data = filter(df_panel_b, year == max(key_years)),
    aes(label = str_wrap(cause, width = 18)),
    hjust = 0, nudge_x = 1.5, size = 6.5, fontface = "bold"
  ) +
  scale_color_manual(values = c("No" = "grey60", "Yes" = "#0072B2")) +
  labs(
    title    = "B",
    subtitle = NULL,
    x = "Year",
    y = "Rank"
  ) +
  coord_cartesian(clip = "off") +
  theme_pub +
  theme(
    panel.grid.major.y = element_line(color = "grey95"),
    axis.title.y       = element_text(margin = margin(r = 10)),
    plot.margin         = margin(20, 40, 20, 20)
  )

# ==============================================================================
# 7. 拼图与导出
# ==============================================================================
final_fig2 <- plot_a + plot_b +
  plot_layout(ncol = 2, widths = c(1, 1.5)) +
  plot_annotation(theme = theme(plot.margin = margin(10, 10, 10, 10)))

ggsave(file.path(output_dir, "Figure2_Trend_and_Rank.png"),
       final_fig2, width = 22, height = 8, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "Figure2_Trend_and_Rank.pdf"),
       final_fig2, width = 22, height = 8, dpi = 300, bg = "white")

cat("\nFigure 2 已保存至:", output_dir, "\n")

# ==============================================================================
# 8. 输出 Results 数值
# ==============================================================================
g1990 <- round(filter(df_panel_a, year == 1990, cause_group == "Total Genetic Disorders")$pmr * 100, 2)
g2023 <- round(filter(df_panel_a, year == 2023, cause_group == "Total Genetic Disorders")$pmr * 100, 2)
c1990 <- round(filter(df_panel_a, year == 1990, cause_group == "CMNN (Total)")$pmr * 100, 2)
c2023 <- round(filter(df_panel_a, year == 2023, cause_group == "CMNN (Total)")$pmr * 100, 2)

r1990 <- filter(df_panel_b, year == 1990, cause == "Total Genetic Disorders")$rank
r2023 <- filter(df_panel_b, year == 2023, cause == "Total Genetic Disorders")$rank

cat("\n======================================================\n")
cat("      RESULTS DATA FOR MANUSCRIPT                     \n")
cat("======================================================\n")
cat("Fig 2A - PMR Trends:\n")
cat(sprintf("  CMNN PMR:    %.2f%% (1990) -> %.2f%% (2023)\n", c1990, c2023))
cat(sprintf("  Genetic PMR: %.2f%% (1990) -> %.2f%% (2023)\n", g1990, g2023))
cat("\nFig 2B - Ranking:\n")
cat(sprintf("  Genetic Rank: #%d (1990) -> #%d (2023)\n", r1990, r2023))
cat("======================================================\n")
