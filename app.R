library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(httr2)
library(jsonlite)
library(DBI)
library(RSQLite)

source(file.path("R", "haikubox.R"))
source(file.path("R", "sun.R"))
source(file.path("R", "moon.R"))
source(file.path("R", "species_groups.R"))
source(file.path("R", "archive.R"))
source(file.path("R", "heatmap_plot.R"))
source(file.path("R", "dashboard.R"))

data_dir <- Sys.getenv("PARKER_DATA_DIR", unset = "data")
live_refresh_ms <- as.integer(Sys.getenv("HAIKUBOX_REFRESH_MS", unset = "600000"))

load_excel_birds <- function(dir = data_dir) {
  birdfiles <- list.files(dir, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
  if (length(birdfiles) == 0) {
    return(tibble(
      Species = character(),
      scientific_name = character(),
      datetime = as.POSIXct(character()),
      Count = numeric(),
      Score = numeric(),
      date_col = as.Date(character()),
      time_col = numeric()
    ))
  }

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

# Excel (legacy) + SQLite archive; prefer Excel on Species+datetime overlap (keeps Score).
load_birds <- function(dir = data_dir) {
  excel <- load_excel_birds(dir)
  archived <- tryCatch(
    read_archive(archive_path(dir), tz = haikubox_tz()),
    error = function(e) {
      warning("Failed to read archive: ", conditionMessage(e))
      tibble(
        Species = character(),
        scientific_name = character(),
        datetime = as.POSIXct(character()),
        Count = numeric(),
        Score = numeric(),
        date_col = as.Date(character()),
        time_col = numeric()
      )
    }
  )

  if (nrow(excel) == 0 && nrow(archived) == 0) {
    stop(
      "No bird data found. Add Excel files under ",
      normalizePath(dir, mustWork = FALSE),
      " or wait for the daily archiver to populate data/archive/detections.sqlite"
    )
  }

  bind_rows(
    excel %>% mutate(source = "excel"),
    archived %>% mutate(source = "archive")
  ) %>%
    distinct(Species, datetime, .keep_all = TRUE) %>%
    select(-source)
}

# Load once at process start (not per browser session) to avoid memory spikes / OOM → 502
excel_birds_global <- tryCatch(
  load_birds(data_dir),
  error = function(e) {
    warning("Failed to load bird data at startup: ", conditionMessage(e))
    NULL
  }
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #f7f5f2; }
      .title-block { margin: 1rem 0 0.75rem; }
      .title-block h1 { margin: 0; font-size: 1.8rem; }
      .title-block p { color: #555; margin: 0.35rem 0 0; }
      .sidebar-panel { background: #fff; border: 1px solid #e6e1d9; border-radius: 8px; padding: 1rem; }
      .summary-box { background: #fff; border: 1px solid #e6e1d9; border-radius: 8px; padding: 0.85rem 1rem; margin-bottom: 1rem; }
      .summary-box h4 { margin-top: 0; }
      .live-box { background: #fff; border: 1px solid #e6e1d9; border-radius: 8px; padding: 0.85rem 1rem; margin-bottom: 1rem; }
      .live-box h4 { margin-top: 0; }
      .species-group .shiny-options-group {
        display: grid;
        grid-template-columns: 1fr 1fr;
        column-gap: 0.5rem;
        row-gap: 0.15rem;
        align-items: center;
      }
      .species-group .radio {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
        min-width: 0;
      }
      .species-group .radio > label {
        display: block;
        font-weight: normal;
        padding-left: 1.4em;
        margin-bottom: 0;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .species-group .radio input[type='radio'] {
        margin-left: -1.4em;
      }
    "))
  ),
  div(
    class = "title-block",
    h1("Parker Birds"),
    p(
      "Haikubox detections at Parker Pond — ",
      tags$a(
        href = "https://birds.haikubox.com/listen/ECDA3B96F3AC",
        target = "_blank",
        rel = "noopener noreferrer",
        "listen live"
      )
    )
  ),
  dashboard_ui("main")
)

server <- function(input, output, session) {
  excel_data <- reactiveVal(excel_birds_global)
  live_data <- reactiveVal(empty_detections_df())
  live_meta <- reactiveVal(list(fetched_at = NA, source = "none", error = NULL))

  refresh_excel <- function() {
    birds <- load_birds()
    excel_birds_global <<- birds
    excel_data(birds)
  }

  refresh_live <- function(notify = FALSE, network = TRUE) {
    result <- tryCatch(
      refresh_live_detections(
        serial = haikubox_serial(),
        hours = 24L,
        cache_path = haikubox_cache_path(data_dir),
        tz = haikubox_tz(),
        network = network
      ),
      error = function(e) {
        list(
          df = empty_detections_df(),
          fetched_at = NA,
          source = "none",
          error = conditionMessage(e)
        )
      }
    )
    live_data(result$df)
    live_meta(list(
      fetched_at = result$fetched_at,
      source = result$source,
      error = result$error
    ))
    # Do not rebuild species/date inputs on live refresh — that re-renders the whole UI
    # and can OOM the container (nginx then returns 502).

    if (notify) {
      if (!is.null(result$error) && identical(result$source, "none")) {
        showNotification(paste("Live refresh failed:", result$error), type = "error")
      } else if (!is.null(result$error) && identical(result$source, "cache")) {
        showNotification("API unreachable; showing cached live detections.", type = "warning")
      } else {
        showNotification(
          paste0("Live data refreshed (", nrow(result$df), " detections)"),
          type = "message"
        )
      }
    }
  }

  birds_data <- reactive({
    excel <- excel_data()
    req(excel)
    merge_excel_and_live(excel, live_data())
  })

  # Seed session from in-memory data + disk cache (no per-session reload)
  observe({
    refresh_live(notify = FALSE, network = FALSE)
  })

  # Network fetch after first paint; then every ~10 minutes
  session$onFlushed(once = TRUE, function() {
    tryCatch(
      refresh_live(notify = FALSE, network = TRUE),
      error = function(e) {
        message("Live refresh after flush failed: ", conditionMessage(e))
      }
    )
  })

  skip_first_timer <- TRUE
  observe({
    invalidateLater(live_refresh_ms)
    if (isTRUE(skip_first_timer)) {
      skip_first_timer <<- FALSE
      return()
    }
    tryCatch(
      refresh_live(notify = FALSE, network = TRUE),
      error = function(e) {
        message("Scheduled live refresh failed: ", conditionMessage(e))
      }
    )
  })

  dashboard_server(
    "main",
    birds_data = birds_data,
    excel_data = excel_data,
    live_data = live_data,
    live_meta = live_meta,
    refresh_excel = refresh_excel,
    refresh_live = refresh_live,
    data_dir = data_dir
  )
}

shinyApp(ui, server)
