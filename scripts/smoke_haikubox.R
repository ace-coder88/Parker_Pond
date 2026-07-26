library(tidyverse)
library(httr2)
library(jsonlite)
library(readxl)
source("R/haikubox.R")

dir.create("data/cache", showWarnings = FALSE)
res <- refresh_live_detections(cache_path = "data/cache/detections.json")
cat("source=", res$source, " n=", nrow(res$df), " error=", res$error %||% "NULL", "\n", sep = "")
print(utils::head(res$df[, c("Species", "datetime", "time_col")], 3))
stopifnot(res$source %in% c("api", "cache"))
stopifnot(file.exists("data/cache/detections.json"))

res2 <- refresh_live_detections(serial = "NOTAREALSERIAL", cache_path = "data/cache/detections.json")
cat("fallback source=", res2$source, " n=", nrow(res2$df), "\n", sep = "")
stopifnot(identical(res2$source, "cache") || nrow(res2$df) > 0 || !is.null(res2$error))

birdfiles <- list.files("data", pattern = "\\.xlsx$", full.names = TRUE)
birds <- bind_rows(lapply(birdfiles, read_excel))
excel <- birds %>%
  mutate(
    `Local Time` = substr(as.character(`Local Time`), 12, 19),
    datetime = ymd_hms(paste(`Local Date`, `Local Time`))
  ) %>%
  filter(!is.na(datetime)) %>%
  transmute(
    Species,
    scientific_name = `Scientific Name`,
    datetime,
    Count = as.numeric(Count),
    Score = as.numeric(Score),
    date_col = as.Date(datetime),
    time_col = as.numeric(format(datetime, "%H"))
  )

merged <- merge_excel_and_live(excel, res$df)
cat("excel=", nrow(excel), " merged=", nrow(merged), "\n", sep = "")
stopifnot(nrow(merged) >= nrow(excel))
parse("app.R")
cat("SMOKE_OK\n")
