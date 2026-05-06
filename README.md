# ConGD-GBD2023-frontier

Code accompanying:

> **Quantifying Avoidable Under-5 Mortality from Congenital and Genetic Disorders: A Composite Burden Assessment and Frontier Analysis Identifying Translational Intervention Targets Using GBD 2023**
> Ruan J*, Tao Z*, Zhang K, Wu S, Zhou Y, Yu X, Zhang H, Zhang Y. *Journal of Translational Medicine* (under review).

This repository contains all R scripts and aggregated GBD 2023 input data needed to reproduce every main and supplementary table/figure in the paper.

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- After Zenodo release, replace XXXXXXX below with your real DOI -->
<!-- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXXX) -->

---

## Repository structure

```
.
├── README.md                       This file
├── LICENSE                         MIT License
├── .gitignore
├── run_all.R                       Master script: runs every analysis end-to-end
├── data/                           Aggregated GBD 2023 inputs (see "Data" below)
└── R/
    ├── 01_main_table/              Table 1
    ├── 02_main_figures/            Figures 1–7
    ├── 03_supplementary_tables/    Tables S1–S12
    └── 04_supplementary_figures/   Figures S1–S4
```

When `run_all.R` is executed it creates an `outputs/` folder containing:

```
outputs/
├── figures/    PNG/PDF for Fig 1–7 and Fig S1–S4
└── tables/     DOCX/CSV for Table 1 and Tables S1–S12
```

---

## Quick start

### 1. Prerequisites

- **R** (≥ 4.4.0). Tested with R 4.4.3 on Windows 11.
- A LaTeX engine is **not** required.
- ~1 GB free disk for outputs.

### 2. Clone or download

```bash
git clone https://github.com/<your-user>/ConGD-GBD2023-frontier.git
cd ConGD-GBD2023-frontier
```

(Or download the ZIP from GitHub and extract.)

### 3. Open the project in RStudio

Open the folder as an **RStudio project** so the working directory and `here::here()` resolve to the repository root automatically. If you are not using RStudio, first run:

```r
setwd("/full/path/to/ConGD-GBD2023-frontier")
```

### 4. Install required R packages

The first time you run the code, install all dependencies:

```r
install.packages(c(
  "tidyverse",     # dplyr, ggplot2, tidyr, readr, purrr, stringr
  "data.table",    # fast data manipulation
  "quantreg",      # quantile regression for the frontier
  "splines",       # natural cubic splines (base R but listed for clarity)
  "ggrepel",       # non-overlapping text labels
  "patchwork",     # composing multi-panel figures
  "cowplot",       # alternative figure composition
  "ggsci",         # journal palettes
  "ggalluvial",    # alluvial diagrams (Figure 4)
  "ggbump",        # bump chart for cause-rank evolution (Figure 2)
  "RColorBrewer",  # palettes
  "scales",        # axis formatting
  "rworldmap",     # world choropleth maps (Figure 1)
  "rworldxtra",    # high-resolution map data (Figure 1)
  "countrycode",   # ISO country code conversion (Figure 1)
  "here",          # project-root path resolution
  "flextable",     # publication-quality tables
  "officer",       # DOCX export
  "pdftools"       # only needed if regenerating Table S11 from raw VR-stars PDF
))
```

### 5. Run all analyses

```r
source("run_all.R")
```

This sources every script in the order described below. Total runtime ≈ 5–15 min depending on hardware.

To run a single analysis, source it directly, e.g.:

```r
source("R/02_main_figures/R6_Figure6_Frontier.R")
```

---

## Execution order

`run_all.R` runs scripts in the following groups. Within a group, order does **not** matter (each script is self-contained and reads directly from `data/`).

| Phase | Folder | Outputs |
|---|---|---|
| 1 | `R/01_main_table/` | Table 1 (Global / SDI-stratified mortality burden) |
| 2 | `R/02_main_figures/` | Figures 1–7 |
| 3 | `R/03_supplementary_tables/` | Tables S1–S12 |
| 4 | `R/04_supplementary_figures/` | Figures S1–S4 |

Note: `Table_S11_parse_VR_stars.R` parses the GBD 2019 appendix PDF to extract VR star ratings. This is **optional** — the resulting `gbd_vr_stars.csv` is already shipped in `data/`. Re-running it requires the original PDF (`mmc1.pdf`), which is not redistributed here.

---

## Data

### What is included
The `data/` folder contains **aggregated CSV/CSV.ZIP files** generated from the GBD Global Health Data Exchange (GHDx) using standard query filters. These are *derived* products and are redistributed here under the GBD 2023 Tools terms with attribution to the Institute for Health Metrics and Evaluation (IHME).

| File | Source query | Used by |
|---|---|---|
| `gbd2023_country_2023.csv.zip` | All ConGD causes, 204 countries, age <5 yr, 2023 | Figs 3/6/7, Tables S6/S10/S11/S12 |
| `gbd2023_sdi_country_2023.csv.zip` | Same query, SDI metadata | Frontier scripts |
| `gbd2023_sdi_values_1950_2023.csv` | SDI by country & year, 1950–2023 | Frontier scripts |
| `gbd2023_trend_1990_2023.csv.zip` | Global + region time series, 1990–2023 | Fig 2/4, Table 1, Table S9, Fig S4 |
| `gbd2023_sdi_quintile_composition.csv.zip` | Cause composition by SDI quintile | Fig 4, Table 1 |
| `gbd2023_age_specific_2023.csv.zip` | Age-specific mortality, 2023 | Fig 5, Table S4 |
| `gbd2023_sex_stratified_1990_2023.csv.zip` | Sex-stratified ASMR, 1990–2023 | Fig S3, Table S8 |
| `gbd2023_rank_causes.csv.zip` | Top causes ranking by year | Fig 2 |
| `gbd_vr_stars.csv` | VR data quality stars (parsed from GBD 2019 appendix) | Table S11 |

### What is **not** included
- **Raw GBD downloads.** The full GBD database is too large and IHME's terms of use require obtaining it directly from [GHDx](https://vizhub.healthdata.org/gbd-results/).
- **Patient-level data.** None used.

### Re-downloading from GHDx
If you wish to regenerate the aggregated files in `data/`, query GHDx with:
- Causes: the 13 ConGD identifiers listed in `R/03_supplementary_tables/Table_S2_Disease_Codes.R`
- Age: `<5 years`
- Sex: `Both` (and `Male`, `Female` for sex-stratified analyses)
- Years: 1990–2023
- Measures: `Deaths`, both `Number` and `Rate`

---

## Key methods (one-paragraph summary)

We aggregated 13 GBD 2023 causes (9 structural birth defects + 4 hemoglobinopathies) into a composite under-5 mortality burden across 204 countries from 1990 to 2023. Frontier analysis used log-transformed quantile regression at τ = 0.05 with natural cubic splines (df = 3), benchmarking each country's age-standardised mortality rate against the best-achievable rate at its Socio-demographic Index level. See `R/02_main_figures/R6_Figure6_Frontier.R` for the canonical implementation.

---

## How to cite

If you use this code, please cite:

1. **The article** (citation will be added on acceptance).
2. **The Zenodo archive** of this repository:
   `[DOI to be assigned upon Zenodo release]`
3. **The underlying GBD 2023 data:**
   GBD 2023 Diseases and Injuries Collaborators. *Lancet*. (2025). Forthcoming.

---

## License

Code is released under the **MIT License** (see [LICENSE](LICENSE)).
The aggregated data in `data/` is redistributed under the GBD 2023 Tools terms with attribution to IHME.

---

## Contact

Yue Zhang — `yue.zhang@zju.edu.cn`

Issues and pull requests welcome at the repository's GitHub page.
