# Smoke test: heatmap plotting for <1 month date spans (no Shiny UI).
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(ggplot2)
  library(lubridate)
})

source("R/haikubox.R")
source("R/sun.R")

# Pull helpers from app.R without launching Shiny / loading globals.
app_env <- new.env(parent = environment())
app_lines <- readLines("app.R")
start <- grep("^aggregate_heatmap\\s*<-", app_lines)[1]
end <- grep("^ui\\s*<-", app_lines)[1] - 1L
stopifnot(is.finite(start), is.finite(end), end > start)
eval(parse(text = app_lines[start:end]), envir = app_env)

load_birds <- function(dir = "data") {
  birdfiles <- list.files(dir, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
  stopifnot(length(birdfiles) > 0)
  bind_rows(lapply(birdfiles, read_excel)) %>%
    mutate(
      `Local Time` = substr(as.character(`Local Time`), 12, 19),
      datetime = ymd_hms(paste(`Local Date`, `Local Time`))
    ) %>%
    filter(!is.na(datetime)) %>%
    transmute(
      Species,
      Count = as.numeric(Count),
      date_col = as.Date(datetime),
      time_col = as.numeric(format(datetime, "%H"))
    )
}

birds <- load_birds("data")
stopifnot(nrow(birds) > 0)

dir.create("Outputs", showWarnings = FALSE)

for (span in c(1L, 3L, 7L, 14L, 21L, 45L)) {
  d0 <- as.Date("2026-07-01")
  d1 <- d0 + span - 1L
  df <- dplyr::filter(birds, date_col >= d0, date_col <= d1)
  if (nrow(df) == 0) {
    # Fall back to earliest span in the data
    d0 <- min(birds$date_col)
    d1 <- d0 + span - 1L
    df <- dplyr::filter(birds, date_col >= d0, date_col <= d1)
  }
  stopifnot(nrow(df) > 0)

  agg <- app_env$aggregate_heatmap(df)
  sun <- sun_curves_for_dates(agg$date_col)
  p <- app_env$plot_heatmap(
    agg,
    title = paste0("Smoke ", span, "d"),
    show_sun = TRUE,
    sun_curves = sun
  )

  err <- tryCatch(
    {
      tf <- file.path("Outputs", paste0("smoke_short_", span, "d.png"))
      png(tf, width = 900, height = 500)
      print(p)
      dev.off()
      ggplot_gtable(ggplot_build(p))
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  if (!is.null(err)) {
    stop("Short-range plot failed for span=", span, ": ", err)
  }
  cat("OK span=", span, " days rows=", nrow(agg), " sun=", nrow(sun), "\n", sep = "")
}

cat("SMOKE_SHORT_RANGE_OK\n")
