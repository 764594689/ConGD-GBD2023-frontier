# ============================================================================
# Figure 1: Global ConGD Burden Maps (GBD 2023)
# Panel A: ASMR per 100,000  |  Panel B: PMR (%)
# Updated: GBD 2023 data, 13 individual causes
# ============================================================================

pkgs <- c("tidyverse", "rworldmap", "rworldxtra", "countrycode", "data.table")
for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(tidyverse)
library(rworldmap)
library(rworldxtra)
library(countrycode)
library(data.table)

# --- Paths ---
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
data_path  <- file.path(base_dir, "data/gbd2023_country_2023.csv.zip")
output_dir <- file.path(base_dir, "outputs/figures")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- 13 ConGD causes (GBD 2023 individual codes) ---
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
genetic_list <- c(structural_list, hemoglobin_list)

# --- Data Processing ---
df <- read_csv(data_path, show_col_types = FALSE)

df_processed <- df %>%
  filter(year == 2023, age_name == "<5 years", sex_name == "Both") %>%
  group_by(location_name) %>%
  summarise(
    Genetic_Rate     = sum(val[cause_name %in% genetic_list & metric_name == "Rate"], na.rm = TRUE),
    Genetic_Deaths   = sum(val[cause_name %in% genetic_list & metric_name == "Number"], na.rm = TRUE),
    All_Cause_Deaths = sum(val[cause_name == "All causes" & metric_name == "Number"], na.rm = TRUE)
  ) %>%
  mutate(
    PMR = (Genetic_Deaths / All_Cause_Deaths) * 100,
    iso3 = countrycode(location_name, "country.name", "iso3c")
  )

# --- Color groups ---
# Panel A: Mortality Rate
cuts_A <- c(0, 50, 100, 150, 200, 99999)
labels_A <- c("<50", "50-100", "100-150", "150-200", ">200")
colors_A <- c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")

df_processed$Group_A <- cut(df_processed$Genetic_Rate,
                            breaks = cuts_A, labels = labels_A,
                            include.lowest = TRUE, right = FALSE)

# Panel B: PMR
cuts_B <- c(0, 10, 20, 30, 40, 99999)
labels_B <- c("<10%", "10-20%", "20-30%", "30-40%", ">40%")
colors_B <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")

df_processed$Group_B <- cut(df_processed$PMR,
                            breaks = cuts_B, labels = labels_B,
                            include.lowest = TRUE, right = FALSE)

# --- Join to map ---
merged_A <- joinCountryData2Map(df_processed, joinCode = "ISO3",
                                nameJoinColumn = "iso3", mapResolution = "high")
merged_A <- merged_A[merged_A$ISO_A3 != "ATA", ]
merged_B <- merged_A

# --- Inset specifications ---
inset_specs <- list(
  caribbean       = list(xlim = c(-90, -60), ylim = c(5, 30),  out_w = 4.0, out_h = 3.3),
  persian_gulf    = list(xlim = c(45, 65),   ylim = c(22, 30), out_w = 4.0, out_h = 1.6),
  balkan          = list(xlim = c(18, 30),   ylim = c(39, 48), out_w = 3.0, out_h = 2.3),
  southeast_asia  = list(xlim = c(95, 145),  ylim = c(-10, 25),out_w = 4.0, out_h = 2.8),
  west_africa     = list(xlim = c(-20, 20),  ylim = c(-5, 20), out_w = 4.0, out_h = 2.5),
  eastern_med     = list(xlim = c(25, 40),   ylim = c(30, 45), out_w = 3.0, out_h = 3.0),
  northern_europe = list(xlim = c(-10, 30),  ylim = c(50, 70), out_w = 4.0, out_h = 2.0)
)

MAIN_W <- 16
MAIN_H <- 6.4

# --- Helper functions ---
save_inset <- function(merged, val_col, color_palette, cut_method,
                       name, spec, suffix) {
  fname_pdf <- file.path(output_dir, paste0(name, "_", suffix, ".pdf"))
  fname_png <- file.path(output_dir, paste0(name, "_", suffix, ".png"))

  pdf(fname_pdf, width = spec$out_w, height = spec$out_h)
  par(mai = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  mapCountryData(merged, nameColumnToPlot = val_col, addLegend = FALSE,
                 numCats = length(color_palette), catMethod = cut_method,
                 colourPalette = color_palette, borderCol = "black",
                 missingCountryCol = "grey90", mapTitle = "",
                 xlim = spec$xlim, ylim = spec$ylim)
  box(lwd = 1.5)
  dev.off()

  png(fname_png, width = spec$out_w, height = spec$out_h, units = "in", res = 400)
  par(mai = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  mapCountryData(merged, nameColumnToPlot = val_col, addLegend = FALSE,
                 numCats = length(color_palette), catMethod = cut_method,
                 colourPalette = color_palette, borderCol = "black",
                 missingCountryCol = "grey90", mapTitle = "",
                 xlim = spec$xlim, ylim = spec$ylim)
  box(lwd = 1.5)
  dev.off()

  cat(sprintf("  %-20s %5.2f x %5.2f inch\n", name, spec$out_w, spec$out_h))
}

save_main_map <- function(merged, val_col, color_palette, cut_method,
                          legend_title, legend_labels, suffix) {
  fname_pdf <- file.path(output_dir, paste0("0_main_", suffix, ".pdf"))
  fname_png <- file.path(output_dir, paste0("0_main_", suffix, ".png"))

  draw_map <- function() {
    par(mai = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
    mapCountryData(merged, nameColumnToPlot = val_col, addLegend = FALSE,
                   numCats = length(color_palette), catMethod = cut_method,
                   colourPalette = color_palette, borderCol = "black",
                   missingCountryCol = "grey90", mapTitle = "")
    legend("bottomleft", title = paste0("\n", legend_title),
           legend = legend_labels, fill = color_palette, border = "black",
           bty = "o", bg = "white", box.lwd = 0.8, cex = 0.85,
           title.cex = 0.9, x.intersp = 0.8, y.intersp = 1.1,
           inset = c(0.03, 0.06))
  }

  pdf(fname_pdf, width = MAIN_W, height = MAIN_H); draw_map(); dev.off()
  png(fname_png, width = MAIN_W, height = MAIN_H, units = "in", res = 400); draw_map(); dev.off()
  cat(sprintf("  %-20s %5.2f x %5.2f inch\n", "main_map", MAIN_W, MAIN_H))
}

# --- Output ---
cat("\n=== Panel A (Mortality Rate) ===\n")
save_main_map(merged_A, "Genetic_Rate", colors_A, cuts_A,
              "Mortality Rate\n(per 100k)", labels_A, "A")
for (nm in names(inset_specs)) {
  save_inset(merged_A, "Genetic_Rate", colors_A, cuts_A,
             nm, inset_specs[[nm]], "A")
}

cat("\n=== Panel B (PMR) ===\n")
save_main_map(merged_B, "PMR", colors_B, cuts_B,
              "Proportional\nMortality Ratio", labels_B, "B")
for (nm in names(inset_specs)) {
  save_inset(merged_B, "PMR", colors_B, cuts_B,
             nm, inset_specs[[nm]], "B")
}

cat("\nDone! Files in:", output_dir, "\n")
