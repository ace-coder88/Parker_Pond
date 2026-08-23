suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readxl)
})
source("R/haikubox.R")
source("R/import_export.R")

x <- read_excel("data/Parker.2026.07.xlsx", n_max = 50)
tf <- tempfile(fileext = ".csv")
write.csv(x, tf, row.names = FALSE)
p <- normalize_haikubox_export(read.csv(tf, check.names = FALSE), tz = "America/New_York")
stopifnot(nrow(p) > 0, all(!is.na(p$datetime)))
cat("OK excel-csv rows", nrow(p), "span", as.character(min(p$date_col)), as.character(max(p$date_col)), "\n")

td <- tempfile("data")
dir.create(file.path(td, "uploads"), recursive = TRUE)
res <- save_haikubox_upload(tf, "Parker.sample.csv", data_dir = td)
stopifnot(res$n == nrow(p), file.exists(res$path))
loaded <- load_upload_birds(td)
stopifnot(nrow(loaded) == nrow(p))
cat("OK upload persist", res$path, "loaded", nrow(loaded), "\n")

y <- data.frame(
  Species = c("American Robin", "soundscape"),
  `Scientific Name` = c("Turdus migratorius", NA),
  datetime = c("2026-08-01 06:15:00", "2026-08-01 07:00:00"),
  Count = c(2, 1),
  Score = c(0.9, 0.1),
  check.names = FALSE
)
p2 <- normalize_haikubox_export(y)
stopifnot(nrow(p2) == 1, p2$Species[[1]] == "American Robin", p2$Count[[1]] == 2)
cat("OK datetime variant\n")
cat("SMOKE_UPLOAD_OK\n")
