#!/usr/bin/env Rscript
# Fetch last 24h of Haikubox detections and upsert into the SQLite archive.
# Intended to run once daily from the Compose archiver service.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(tibble)
  library(lubridate)
  library(DBI)
  library(RSQLite)
})

root <- Sys.getenv("PARKER_APP_ROOT", unset = "/app")
if (!file.exists(file.path(root, "R", "haikubox.R"))) {
  root <- getwd()
}

source(file.path(root, "R", "haikubox.R"))
source(file.path(root, "R", "archive.R"))

data_dir <- Sys.getenv("PARKER_DATA_DIR", unset = file.path(root, "data"))
path <- archive_path(data_dir)
serial <- haikubox_serial()
tz <- haikubox_tz()

cat(
  format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  " archiving Haikubox detections serial=", serial,
  " -> ", path, "\n",
  sep = ""
)

payload <- tryCatch(
  fetch_detections_raw(serial = serial, hours = 24L),
  error = function(e) {
    message("ERROR fetch failed: ", conditionMessage(e))
    quit(status = 1)
  }
)

df <- normalize_detections(payload, tz = tz)
# Keep cache warm for the Shiny live panel as a side effect
tryCatch(
  write_detections_cache(payload, path = haikubox_cache_path(data_dir), serial = serial),
  error = function(e) message("WARN cache write failed: ", conditionMessage(e))
)

result <- upsert_detections(df, path = path)
summary <- archive_summary(path)

cat(
  "OK fetched=", nrow(df),
  " inserted=", result$inserted,
  " archive_total=", summary$n,
  " span=", as.character(summary$date_min), "..", as.character(summary$date_max),
  "\n",
  sep = ""
)
