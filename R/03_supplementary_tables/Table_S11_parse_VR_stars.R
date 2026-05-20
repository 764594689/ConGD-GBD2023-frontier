library(pdftools)
pdf <- "D:/此桌面/QQ美莓/A_GBD_genetic_disease/02J Transl Med/figure/mmc1.pdf"
txt <- pdf_text(pdf)
lines <- c(strsplit(txt[1494], "\n")[[1]], strsplit(txt[1495], "\n")[[1]])

recs <- list()
for (l in lines) {
  l <- gsub("\\s+", " ", trimws(l))
  if (nchar(l) < 10) next
  m <- regmatches(l, regexec("^([A-Z][A-Za-z.,()' -]+?) ([0-5]) ([0-9.].*)$", l))[[1]]
  if (length(m) == 4) {
    country <- trimws(m[2])
    star <- as.integer(m[3])
    nums <- suppressWarnings(as.numeric(strsplit(m[4], " ")[[1]]))
    pwc_total <- tail(nums[!is.na(nums)], 1)
    recs[[length(recs) + 1]] <- data.frame(country = country, stars = star, pwc_1980_2019 = pwc_total)
  }
}
df <- do.call(rbind, recs)
df <- df[!duplicated(df$country), ]
cat("Parsed:", nrow(df), "countries\n")
cat("\nStar distribution:\n"); print(table(df$stars))
cat("\n5-star:\n"); print(df$country[df$stars == 5])
cat("\n4-star:\n"); print(df$country[df$stars == 4])
write.csv(df, "D:/此桌面/QQ美莓/A_GBD_genetic_disease/02J Transl Med/data/gbd_vr_stars.csv", row.names = FALSE)
cat("\nSaved.\n")
