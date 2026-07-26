# Smoke test for Parker Birds heatmap pipeline (no Shiny UI)
library(tidyverse)
library(readxl)
library(ggplot2)

data_dir <- "data"

load_birds <- function(dir = data_dir) {
  birdfiles <- list.files(dir, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
  stopifnot(length(birdfiles) > 0)
  birds <- bind_rows(lapply(birdfiles, read_excel))
  birds %>%
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
}

aggregate_heatmap <- function(df) {
  if (nrow(df) == 0) {
    return(tibble(date_col = as.Date(character()), time_col = numeric(), Count = numeric()))
  }
  date_min <- min(df$date_col)
  date_max <- max(df$date_col)
  df %>%
    filter(!is.na(date_col), !is.na(time_col)) %>%
    group_by(date_col, time_col) %>%
    summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
    complete(date_col = seq(date_min, date_max, by = "1 day"), time_col = 0:23, fill = list(Count = 0)) %>%
    filter(!is.na(time_col))
}

birds <- load_birds()
stopifnot(nrow(birds) > 0)
stopifnot(n_distinct(birds$Species) > 1)

all_agg <- aggregate_heatmap(birds)
owl_agg <- aggregate_heatmap(filter(birds, grepl("\\bOwl\\b", Species, perl = TRUE)))
barred_agg <- aggregate_heatmap(filter(birds, Species == "Barred Owl"))

stopifnot(sum(all_agg$Count) > 0)
stopifnot(sum(owl_agg$Count) > 0)
stopifnot(sum(barred_agg$Count) > 0)

dir.create("Outputs", showWarnings = FALSE)
ggsave(
  "Outputs/smoke_all.jpg",
  ggplot(all_agg, aes(date_col, time_col, fill = Count)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_viridis_c(option = "magma"),
  width = 8, height = 6, dpi = 100
)
ggsave(
  "Outputs/smoke_owls.jpg",
  ggplot(owl_agg, aes(date_col, time_col, fill = Count)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_viridis_c(option = "magma"),
  width = 8, height = 6, dpi = 100
)
ggsave(
  "Outputs/smoke_barred.jpg",
  ggplot(barred_agg, aes(date_col, time_col, fill = Count)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_viridis_c(option = "magma"),
  width = 8, height = 6, dpi = 100
)

cat(
  "LOADED rows=", nrow(birds),
  " species=", n_distinct(birds$Species),
  " score=", min(birds$Score, na.rm = TRUE), "-", max(birds$Score, na.rm = TRUE),
  "\n", sep = ""
)
cat("ALL calls=", sum(all_agg$Count), " OWL calls=", sum(owl_agg$Count),
    " BARRED calls=", sum(barred_agg$Count), "\n", sep = "")
cat("SMOKE_OK\n")
