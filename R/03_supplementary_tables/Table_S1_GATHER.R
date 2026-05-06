# ==============================================================================
# Supplementary Table S4: GATHER Checklist
# No GBD data needed — compliance checklist
# ==============================================================================
library(tidyverse)
library(flextable)
library(officer)

cat("--- Table S4: GATHER Checklist ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
output_dir <- file.path(base_dir, "outputs/tables")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

df_s4 <- tibble(
  Section = c(
    rep("Objectives and funding", 2),
    rep("Data inputs", 5),
    rep("Data analysis", 6),
    rep("Results and discussion", 5)
  ),
  Item = as.character(1:18),
  Checklist_Item = c(
    "Define the indicator(s), populations (including age, sex, and geographic entities), and time period(s) for which estimates were made.",
    "List the funding sources for the work.",
    "Describe how the data were identified and how the data can be accessed.",
    "Specify the inclusion and exclusion criteria. Identify all ad hoc data exclusion criteria.",
    "Provide information on all included data sources and their main characteristics.",
    "Identify and describe any categories of input data that have potentially important biases.",
    "Describe and give sources for any other data inputs.",
    "Provide a conceptual overview of the data analysis method.",
    "Provide a detailed description of all steps of the analysis, including mathematical formulae.",
    "Describe how candidate models were evaluated and how the final model(s) were selected.",
    "Provide the results of an evaluation of model performance, if done, as well as the results of any relevant sensitivity analysis.",
    "Describe methods for calculating uncertainty of the estimates.",
    "State how analytic or statistical uncertainty was factored into the evaluation of model performance, if done.",
    "Provide published estimates in a file format from which data can be efficiently extracted.",
    "Report a quantitative measure of the uncertainty of the estimates.",
    "Interpret results in light of existing evidence.",
    "Discuss limitations of the estimates. Include a discussion of any modelling assumptions or data limitations that affect interpretation.",
    "State how the study was funded."
  ),
  Reported = c(
    "Methods: 13 ConGD causes, 204 countries, under-5 population, 1990\u20132023.",
    "Declarations: Funding section.",
    "Methods: GBD 2023 Results Tool (https://vizhub.healthdata.org/gbd-results/).",
    "Methods: Table S1 lists inclusion criteria; no ad hoc exclusions applied.",
    "Methods: GBD 2023 database; Details in GBD 2023 capstone publications.",
    "Discussion: Limitations (ascertainment bias in LMICs, mortality-only framing).",
    "Methods: SDI covariate from GBD 2023 Socio-Demographic Index database.",
    "Methods: Descriptive epidemiology + quantile regression frontier analysis.",
    "Methods: Quantile regression (tau=0.05) with natural cubic splines (df=3) on log-ASMR vs SDI.",
    "Methods & Table S7: Model diagnostics, pseudo-R\u00B2, knot sensitivity.",
    "Figures S1, S3, S5; Tables S7, S9: Sensitivity and cross-validation results.",
    "Methods: GBD uncertainty intervals propagated; bootstrap CI for frontier.",
    "Table S7: Pseudo-R\u00B2 and coverage probability reported.",
    "Table S2: Full 204-country dataset available as supplementary CSV.",
    "All estimates reported with 95% uncertainty intervals.",
    "Discussion: Interpreted in context of epidemiological transition, translational gap.",
    "Discussion: Limitations subsection addresses data quality, ecological fallacy, confounding.",
    "Declarations: Funding section."
  )
)

# --- Flextable ---
ft_s4 <- df_s4 %>%
  set_names(c("Section", "Item", "GATHER Checklist Item", "Reported (Location in Manuscript)")) %>%
  flextable() %>%
  merge_v(j = "Section") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  bold(part = "header") %>%
  align(align = "center", part = "header") %>%
  align(j = 1:2, align = "center", part = "body") %>%
  valign(j = 1, valign = "top", part = "body") %>%
  border_remove() %>%
  hline_top(border = fp_border_default(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1.5), part = "body") %>%
  padding(padding = 4, part = "all") %>%
  width(j = 1, width = 1.2) %>%
  width(j = 2, width = 0.5) %>%
  width(j = 3, width = 3.5) %>%
  width(j = 4, width = 2.8) %>%
  set_table_properties(layout = "fixed") %>%
  set_caption("Table S1. Guidelines for Accurate and Transparent Health Estimates Reporting (GATHER) checklist.")

save_as_docx(ft_s4, path = file.path(output_dir, "Table_S1_GATHER.docx"),
             pr_section = prop_section(
               page_size = page_size(orient = "portrait"),
               page_margins = page_mar(bottom = 0.8, top = 0.8, right = 0.6, left = 0.6)
             ))

cat("Table S4 saved.\n")
