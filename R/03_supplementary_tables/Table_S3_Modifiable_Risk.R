# ==============================================================================
# Table S3: Modifiable fraction — split into Etiologic vs Mortality modifiability
# Reviewer #6b: previous single-column "0% (genetic)" wording for hemoglobinopathies
# was misleading because while disease *occurrence* is Mendelian, clinical *mortality*
# is highly preventable through screening, prophylaxis, and chronic care.
# ==============================================================================
library(tidyverse)
library(flextable)
library(officer)

cat("--- Table S3: Modifiable fractions (etiologic vs mortality) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir <- here::here()  # auto-detects project root
output_dir <- file.path(base_dir, "outputs/tables")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
save_word_path <- file.path(output_dir, "Table_S3_Modifiable_Risk.docx")

df_s3 <- tibble(
  Condition = c(
    "Neural tube defects",
    "Orofacial clefts",
    "Congenital heart anomalies",
    "Down syndrome",
    "Other chromosomal abnormalities",
    "Digestive congenital anomalies",
    "Urogenital congenital anomalies",
    "Congenital musculoskeletal and limb anomalies",
    "Other congenital birth defects",
    "Sickle cell disorders",
    "Thalassemias",
    "G6PD deficiency",
    "Other hemoglobinopathies and hemolytic anemias"
  ),
  Category = c(rep("Structural", 9), rep("Hemoglobinopathy", 4)),
  Etiologic Modifiability = c(
    "50\u201370%", "10\u201325%", "5\u201310%", "<5%", "<5%",
    "<5%", "<5%", "<5%", "Variable",
    "Low (Mendelian)", "Low (Mendelian)", "Low (X-linked)", "Low (Mendelian)"
  ),
  Primary Etiologic Lever = c(
    "Folic acid fortification / supplementation",
    "Maternal smoking, alcohol, nutrition",
    "Maternal diabetes, rubella, obesity",
    "Advanced maternal age (screening-modifiable)",
    "Advanced maternal age",
    "Limited evidence",
    "Endocrine disruptors (emerging evidence)",
    "Limited evidence",
    "Multiple / unknown",
    "Carrier screening / genetic counselling",
    "Carrier screening / genetic counselling",
    "Carrier screening; avoidance of clinical triggers",
    "Carrier screening / genetic counselling"
  ),
  Mortality Modifiability = c(
    "High (surgical repair)",
    "High (surgical repair)",
    "High (surgical repair)",
    "Moderate (cardiac surgery, supportive care)",
    "Variable (condition-dependent)",
    "High (surgical repair)",
    "High (surgical repair)",
    "Moderate to high (surgical / orthopaedic)",
    "Variable",
    "High (NBS + penicillin + hydroxyurea; gene therapy where accessible)",
    "High (transfusion + iron chelation; HSCT; gene therapy)",
    "High (avoidance of triggers; phototherapy; transfusion)",
    "Moderate to high (transfusion + supportive care)"
  ),
  Primary Mortality Lever = c(
    "Pediatric surgical capacity",
    "Pediatric surgical capacity",
    "Pediatric cardiac surgical capacity",
    "Cardiac surgery + multidisciplinary follow-up",
    "Condition-specific",
    "Neonatal surgical capacity",
    "Pediatric surgical capacity",
    "Orthopaedic / reconstructive capacity",
    "Condition-specific",
    "Newborn screening, penicillin prophylaxis, hydroxyurea",
    "Safe transfusion, chelation, HSCT/gene therapy",
    "Newborn screening; trigger education",
    "Transfusion access; etiologic workup"
  ),
  Key Reference = c(
    "Czeizel 1992; Blencowe 2018",
    "Mossey 2009",
    "Jenkins 2007; Liu 2019",
    "Natoli 2012",
    "Natoli 2012",
    "\u2014",
    "Skakkebaek 2016",
    "\u2014",
    "\u2014",
    "Piel 2017; Tshilolo 2019 (REACH)",
    "Modell 2008",
    "Cappellini 2008",
    "\u2014"
  )
)

ft <- df_s3 %>%
  flextable() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 7, part = "all") %>%
  bold(part = "header") %>%
  align(align = "center", part = "header") %>%
  align(j = 1:2, align = "left", part = "body") %>%
  border_remove() %>%
  hline_top(border = fp_border_default(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1.5), part = "body") %>%
  padding(padding = 3, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  set_caption("Table S3. Estimated modifiable fraction for included conditions: etiologic vs mortality modifiability.") %>%
  add_footer_lines(paste0(
    "Etiologic modifiability = whether disease occurrence can be prevented; ",
    "mortality modifiability = whether death from existing disease can be prevented. ",
    "This distinction clarifies that hemoglobinopathies, although Mendelian in origin and ",
    "therefore not preventable through public-health measures targeting incidence, have ",
    "highly modifiable mortality through screening, prophylaxis, and chronic care. ",
    "Etiologic modifiability refers to interventions that reduce live-birth prevalence ",
    "(folic acid fortification, periconceptional care, premarital carrier screening). ",
    "Mortality modifiability refers to interventions that reduce case-fatality among ",
    "affected children (surgical repair, prophylaxis, disease-modifying therapy). ",
    "Fractions are approximate and context-dependent."
  ))

save_as_docx(ft, path = save_word_path,
             pr_section = prop_section(page_size = page_size(orient = "portrait"),
                                       page_margins = page_mar(top = 0.6, bottom = 0.6, left = 0.6, right = 0.6)))
cat("Table S3 saved:", save_word_path, "\n")
