# ==============================================================================
# Supplementary Table S1: Disease Classification and GBD Cause IDs
# No GBD data needed — hardcoded reference table
# ==============================================================================
library(tidyverse)
library(flextable)
library(officer)

cat("--- Table S1: Disease Classification ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
output_dir <- file.path(base_dir, "outputs/tables")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

df_s1 <- tibble(
  Category = c(rep("Structural Birth Defects", 9),
               rep("Hemoglobinopathies & Hemolytic Anemias", 4)),
  Specific_Cause = c(
    "Congenital heart anomalies", "Neural tube defects", "Down syndrome",
    "Other chromosomal abnormalities", "Orofacial clefts",
    "Digestive congenital anomalies", "Urogenital congenital anomalies",
    "Congenital musculoskeletal and limb anomalies", "Other congenital birth defects",
    "Sickle cell disorders", "Thalassemias",
    "G6PD deficiency", "Other hemoglobinopathies and hemolytic anemias"
  ),
  GBD_Cause_ID = c(643, 642, 645, 644, 644, 647, 646, 645, 648,
                    614, 613, 615, 618),
  Rationale = c(
    "Largest proportion of fatal congenital defects globally.",
    "Leading congenital cause of CNS-related neonatal death.",
    "Most common survivable chromosomal disorder.",
    "Includes Trisomy 13/18, Turner syndrome, etc.",
    "Common and typically surgically correctable anomalies.",
    "Includes acute neonatal emergencies (e.g., esophageal atresia, diaphragmatic hernia).",
    "Includes renal agenesis and other genitourinary malformations.",
    "Includes clubfoot and other musculoskeletal deformities.",
    "Other unspecified structural anomalies.",
    "One of the most common monogenic diseases globally.",
    "Includes severe thalassemia major requiring regular transfusion.",
    "Leading enzymatic cause of neonatal kernicterus.",
    "Encompasses other rare hemolytic anemias."
  )
)

# --- Flextable ---
ft_s1 <- df_s1 %>%
  set_names(c("Category", "Specific Cause Name (GBD 2023)",
              "GBD Cause ID", "Inclusion Rationale")) %>%
  flextable() %>%
  merge_v(j = "Category") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  align(align = "center", part = "header") %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 3, align = "center", part = "body") %>%
  valign(j = 1, valign = "top", part = "body") %>%
  border_remove() %>%
  hline_top(border = fp_border_default(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1.5), part = "body") %>%
  padding(padding = 4, part = "all") %>%
  width(j = 1, width = 1.5) %>%
  width(j = 2, width = 2.2) %>%
  width(j = 3, width = 0.8) %>%
  width(j = 4, width = 3.0) %>%
  set_table_properties(layout = "fixed") %>%
  set_caption("Table S2. Congenital and genetic disorder classification and GBD 2023 cause identifiers.")

save_as_docx(ft_s1, path = file.path(output_dir, "Table_S2_Disease_Codes.docx"),
             pr_section = prop_section(
               page_size = page_size(orient = "portrait"),
               page_margins = page_mar(bottom = 1, top = 1, right = 0.8, left = 0.8)
             ))

cat("Table S1 saved.\n")
