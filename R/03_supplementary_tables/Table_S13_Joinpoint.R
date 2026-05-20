# ==============================================================================
# Table S13: Joinpoint regression of ConGD ASMR 1990-2023
#   Detects inflection points in log(ASMR) vs year and reports the
#   Annual Percentage Change (APC) for each segment, plus the
#   Average APC (AAPC) over the full period.
#   Joinpoints found by `segmented` (Davies test, max 2 joinpoints by default).
# Output: outputs/tables/Table_S13_Joinpoint.docx
# ==============================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(segmented); library(flextable); library(officer)
})

cat("--- Table S13: Joinpoint AAPC (GBD 2023) ---\n")

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
base_dir   <- here::here()
path_trend <- file.path(base_dir, "data/gbd2023_trend_1990_2023.csv.zip")
dir_out    <- file.path(base_dir, "outputs/tables")
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
save_word  <- file.path(dir_out, "Table_S13_Joinpoint.docx")

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
all_13 <- c(structural_list, hemoglobin_list)

df <- read_csv(path_trend, show_col_types = FALSE) %>%
  filter(age_name == "<5 years", sex_name == "Both",
         measure_name == "Deaths", metric_name == "Rate",
         cause_name %in% all_13) %>%
  group_by(location_name, year) %>%
  summarise(asmr = sum(val, na.rm = TRUE), .groups = "drop") %>%
  filter(asmr > 0)

# ------------------------------------------------------------------------------
# Joinpoint per location: log(ASMR) ~ year, allow up to 2 break-points.
# Davies test guides whether any break is significant; AICc selects optimal K.
# APC_k = 100 * (exp(beta_k) - 1) per segment.
# AAPC  = weighted average of segment APCs (weights = segment length).
# ------------------------------------------------------------------------------
fit_joinpoint <- function(d, max_k = 2) {
  d <- d %>% arrange(year)
  lm0 <- lm(log(asmr) ~ year, data = d)
  best <- list(fit = lm0, k = 0, aicc = AIC(lm0) + 2*2*(2+1)/(nrow(d)-2-1))
  for (k in 1:max_k) {
    seg <- tryCatch(
      segmented(lm0, seg.Z = ~ year, npsi = k,
                control = seg.control(it.max = 50, n.boot = 50, seed = 20251119)),
      error = function(e) NULL)
    if (!is.null(seg) && inherits(seg, "segmented")) {
      p <- length(coef(seg))
      aicc <- AIC(seg) + 2*p*(p+1)/(nrow(d)-p-1)
      if (aicc < best$aicc) best <- list(fit = seg, k = k, aicc = aicc)
    }
  }
  best
}

apc_from_slopes <- function(fit, k) {
  if (k == 0) {
    b  <- coef(fit)["year"]
    se <- summary(fit)$coefficients["year","Std. Error"]
    list(segments = tibble(seg = "1990-2023",
                           apc    = 100*(exp(b)-1),
                           apc_lo = 100*(exp(b - 1.96*se) - 1),
                           apc_hi = 100*(exp(b + 1.96*se) - 1)),
         brk = NULL)
  } else {
    sl <- slope(fit)$year
    psi <- round(fit$psi[, "Est."])
    # build segment year ranges
    years <- sort(unique(c(min(fit$model$year), psi, max(fit$model$year))))
    rng <- mapply(function(a,b) paste0(a, "–", b), years[-length(years)], years[-1])
    apc_tib <- tibble(
      seg    = rng,
      apc    = 100*(exp(sl[,"Est."]) - 1),
      apc_lo = 100*(exp(sl[,"Est."] - 1.96*sl[,"St.Err."]) - 1),
      apc_hi = 100*(exp(sl[,"Est."] + 1.96*sl[,"St.Err."]) - 1)
    )
    list(segments = apc_tib, brk = psi)
  }
}

aapc_from_segments <- function(segs) {
  # weight each APC by segment length (years)
  yr_pairs <- strsplit(segs$seg, "–")
  lens     <- sapply(yr_pairs, function(p) as.numeric(p[2]) - as.numeric(p[1]))
  list(
    aapc    = sum(segs$apc    * lens) / sum(lens),
    aapc_lo = sum(segs$apc_lo * lens) / sum(lens),
    aapc_hi = sum(segs$apc_hi * lens) / sum(lens)
  )
}

um <- function(x) gsub("-", "−",
                       formatC(round(x, 2), format = "f", digits = 2),
                       fixed = TRUE)

locs <- df %>% distinct(location_name) %>% pull(location_name)
results <- list()
for (loc in locs) {
  d <- df %>% filter(location_name == loc)
  if (nrow(d) < 8) next
  best <- fit_joinpoint(d, max_k = 2)
  parts <- apc_from_slopes(best$fit, best$k)
  aa    <- aapc_from_segments(parts$segments)
  segs_str <- paste(
    sprintf("%s: %s%% (95%% CI %s, %s)",
            parts$segments$seg,
            um(parts$segments$apc),
            um(parts$segments$apc_lo),
            um(parts$segments$apc_hi)),
    collapse = " | "
  )
  brk_str <- if (is.null(parts$brk)) "none" else paste(parts$brk, collapse = ", ")
  results[[loc]] <- tibble(
    Location   = loc,
    Joinpoints = brk_str,
    Segments   = segs_str,
    AAPC       = um(aa$aapc),
    `AAPC 95% CI` = paste0(um(aa$aapc_lo), ", ", um(aa$aapc_hi))
  )
  cat(sprintf("  %-50s k=%d brk=%s AAPC=%.2f\n",
              substr(loc,1,50), best$k, brk_str, aa$aapc))
}
df_tab <- bind_rows(results) %>%
  mutate(.ord = if_else(Location == "Global", 0L, 1L)) %>%
  arrange(.ord, Location) %>% dplyr::select(-.ord)

# --- flextable ---
ft <- df_tab %>%
  flextable() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = c(1,3), align = "left", part = "all") %>%
  border_remove() %>%
  hline_top(border = fp_border_default(width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1), part = "header") %>%
  hline_bottom(border = fp_border_default(width = 1.5), part = "body") %>%
  padding(padding = 3, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  set_caption(paste0("Table S13. Joinpoint regression of ConGD age-standardized ",
                     "mortality rate, 1990–2023. Inflection years detected ",
                     "via segmented regression on ln(ASMR); model order selected ",
                     "by AICc (up to 2 joinpoints).")) %>%
  add_footer_lines(paste0(
    "APC: annual percentage change within a segment; AAPC: average APC across ",
    "the full 1990–2023 period, weighted by segment length. Negative values ",
    "indicate decline."
  ))

save_as_docx(ft, path = save_word,
             pr_section = prop_section(
               page_size    = page_size(orient = "landscape"),
               page_margins = page_mar(top = 0.6, bottom = 0.6,
                                       left = 0.5, right = 0.5)))
cat("Table S13 saved:", save_word, "\n")
