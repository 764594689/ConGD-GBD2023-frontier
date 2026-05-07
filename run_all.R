# ==============================================================================
# run_all.R — Reproduce all main and supplementary tables/figures
# Companion code to: Ruan et al., Journal of Translational Medicine (under review)
# ==============================================================================
# Usage:
#   1. Open this folder as an RStudio project, OR setwd() to the folder root.
#   2. Install dependencies (see README.md or block below).
#   3. source("run_all.R")
#
# Outputs are written to ./outputs/figures/ and ./outputs/tables/
# Total runtime: ~5–15 minutes on a modern laptop.
# ==============================================================================

# --- 0. Setup -----------------------------------------------------------------
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

# Make sure output folders exist
dir.create(here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "tables"),  recursive = TRUE, showWarnings = FALSE)

# Quick package check (does NOT auto-install — see README for full install command)
required_pkgs <- c("tidyverse", "data.table", "quantreg", "ggrepel", "patchwork",
                   "cowplot", "ggsci", "ggalluvial", "ggbump", "RColorBrewer",
                   "scales", "rworldmap", "rworldxtra", "countrycode",
                   "flextable", "officer")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  stop("Missing R packages: ", paste(missing_pkgs, collapse = ", "),
       "\nRun the install.packages() block in README.md first.")
}

run_script <- function(rel_path) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("Running: ", rel_path, "\n", sep = "")
  cat(strrep("=", 70), "\n", sep = "")
  source(here(rel_path), echo = FALSE)
}

# --- 1. Main table ------------------------------------------------------------
run_script("R/01_main_table/T1_Table1_Main.R")

# --- 2. Main figures ----------------------------------------------------------
main_figs <- list.files(here("R", "02_main_figures"),
                        pattern = "\\.R$", full.names = FALSE)
for (f in sort(main_figs)) run_script(file.path("R", "02_main_figures", f))

# --- 3. Supplementary tables --------------------------------------------------
# Run S2/S3/S1 (no GBD data) first, then data-driven ones.
# Order within section is not strict — each script is self-contained.
supp_tables <- list.files(here("R", "03_supplementary_tables"),
                          pattern = "\\.R$", full.names = FALSE)
# Skip the optional VR-stars parser (requires raw PDF not redistributed)
supp_tables <- setdiff(supp_tables, "Table_S11_parse_VR_stars.R")
for (f in sort(supp_tables)) run_script(file.path("R", "03_supplementary_tables", f))

# --- 4. Supplementary figures -------------------------------------------------
supp_figs <- list.files(here("R", "04_supplementary_figures"),
                        pattern = "\\.R$", full.names = FALSE)
for (f in sort(supp_figs)) run_script(file.path("R", "04_supplementary_figures", f))


# ----------------------------------------------------------------------------
# Note on italic formatting:
# Statistical symbols (df, n, P, R², t) that appear in long inline contexts
# (e.g., paragraph footnotes within tables) should be italicized for
# publication. Tables S5 and S11 use as_paragraph(as_i(...)) for inline
# italic. For Tables S1, S6, S7, S12, italic formatting was applied
# manually in the submitted docx; re-running these scripts produces
# substantively identical output without italic styling on inline symbols.
# ----------------------------------------------------------------------------

cat("\n\nAll analyses complete. Check outputs/ for results.\n")
