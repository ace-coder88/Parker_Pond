library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(httr2)
library(jsonlite)

source(file.path("R", "haikubox.R"))
source(file.path("R", "sun.R"))
source(file.path("R", "species_groups.R"))

data_dir <- Sys.getenv("PARKER_DATA_DIR", unset = "data")
live_refresh_ms <- as.integer(Sys.getenv("HAIKUBOX_REFRESH_MS", unset = "600000"))

load_birds <- function(dir = data_dir) {
  birdfiles <- list.files(dir, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
  if (length(birdfiles) == 0) {
    stop("No Excel files found in ", normalizePath(dir, mustWork = FALSE))
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

# Load Excel once at process start (not per browser session) to avoid memory spikes / OOM → 502
excel_birds_global <- tryCatch(
  load_birds(data_dir),
  error = function(e) {
    warning("Failed to load Excel at startup: ", conditionMessage(e))
    NULL
  }
)

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

plot_heatmap <- function(
    agg,
    title,
    palette = "magma",
    brightness = 0,
    reverse = FALSE,
    show_sun = TRUE,
    sun_curves = NULL
) {
  if (nrow(agg) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No calls match the current filters") +
        theme_void()
    )
  }

  # brightness: -1 = darker (use deeper end of scale), +1 = brighter (crop dark end)
  brightness <- max(-1, min(1, as.numeric(brightness)))
  if (brightness >= 0) {
    begin <- brightness * 0.45
    end <- 1
  } else {
    begin <- 0
    end <- 1 + brightness * 0.45
  }

  p <- ggplot(agg, aes(x = date_col, y = time_col, fill = Count)) +
    geom_raster(interpolate = FALSE) +
    scale_fill_viridis_c(
      option = palette,
      name = "# Calls",
      begin = begin,
      end = end,
      direction = if (isTRUE(reverse)) -1 else 1
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(breaks = seq(0, 23, by = 3), limits = c(0, 23), expand = c(0, 0)) +
    labs(y = "Time of Day", title = title) +
    theme_minimal(base_size = 13) +
    theme(
      axis.title.x = element_blank(),
      panel.grid = element_blank()
    )

  if (isTRUE(show_sun) && !is.null(sun_curves) && nrow(sun_curves) > 0) {
    # Sunrise near bottom of y-axis (early hours); sunset near top (evening)
    p <- p +
      geom_line(
        data = sun_curves,
        aes(x = date, y = sunrise_hour, color = "Sunrise", group = 1),
        inherit.aes = FALSE,
        linewidth = 0.7,
        alpha = 0.95
      ) +
      geom_line(
        data = sun_curves,
        aes(x = date, y = sunset_hour, color = "Sunset", group = 1),
        inherit.aes = FALSE,
        linewidth = 0.7,
        alpha = 0.95
      ) +
      scale_color_manual(
        name = NULL,
        values = c(Sunrise = "#FFE082", Sunset = "#80DEEA")
      ) +
      guides(color = guide_legend(override.aes = list(linewidth = 1.2)))
  }

  p
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #f7f5f2; }
      .title-block { margin: 1rem 0 0.5rem; }
      .title-block h1 { margin: 0; font-size: 1.8rem; }
      .title-block p { color: #555; margin: 0.35rem 0 0; }
      .sidebar-panel { background: #fff; border: 1px solid #e6e1d9; border-radius: 8px; padding: 1rem; }
      .summary-box { background: #fff; border: 1px solid #e6e1d9; border-radius: 8px; padding: 0.85rem 1rem; margin-bottom: 1rem; }
      .summary-box h4 { margin-top: 0; }
      .live-box { background: #fff; border: 1px solid #e6e1d9; border-radius: 8px; padding: 0.85rem 1rem; margin-bottom: 1rem; }
      .live-box h4 { margin-top: 0; }
      .species-group .shiny-options-group {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        column-gap: 0.75rem;
        row-gap: 0.2rem;
        align-items: start;
      }
      .species-group .radio { margin-top: 0 !important; margin-bottom: 0; }
      .species-group .radio > label {
        font-weight: normal;
        padding-left: 1.35em;
        white-space: nowrap;
      }
    "))
  ),
  div(
    class = "title-block",
    h1("Parker Birds"),
    p(
      "Haikubox detections at Tuxedo Rock — ",
      tags$a(
        href = "https://birds.haikubox.com/listen/64E8334476B0",
        target = "_blank",
        rel = "noopener noreferrer",
        "listen live"
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      width = 3,
      selectizeInput(
        "species",
        "Species",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Search species… (empty = all birds)")
      ),
      div(
        class = "species-group",
        radioButtons(
          "species_group",
          "Species group",
          choices = species_group_choices,
          selected = "all"
        )
      ),
      helpText("Group radios fill the species box with matching names from the loaded data."),
      dateRangeInput("date_range", "Date range"),
      sliderInput(
        "hour_range",
        "Hour of day",
        min = 0,
        max = 23,
        value = c(0, 23),
        step = 1,
        ticks = FALSE
      ),
      sliderInput(
        "min_score",
        "Minimum detection score",
        min = 0,
        max = 1,
        value = 0,
        step = 0.01
      ),
      hr(),
      selectInput(
        "palette",
        "Color palette",
        choices = c(
          "Magma" = "magma",
          "Inferno" = "inferno",
          "Plasma" = "plasma",
          "Viridis" = "viridis",
          "Cividis" = "cividis",
          "Rocket" = "rocket",
          "Mako" = "mako",
          "Turbo" = "turbo"
        ),
        selected = "magma"
      ),
      sliderInput(
        "brightness",
        "Brightness",
        min = -1,
        max = 1,
        value = 0,
        step = 0.05,
        ticks = FALSE
      ),
      checkboxInput("reverse_palette", "Reverse palette", value = FALSE),
      checkboxInput("show_sun", "Show sunrise / sunset", value = TRUE),
      helpText("Sun curves use Mount Vernon, ME coordinates (override with HAIKUBOX_LAT / HAIKUBOX_LON)."),
      actionButton("reload", "Reload Excel data", class = "btn-primary", width = "100%"),
      br(), br(),
      actionButton("refresh_live", "Refresh live data", width = "100%"),
      br(), br(),
      helpText("Live detections refresh automatically about every 10 minutes from the public Haikubox API (no account key).")
    ),
    mainPanel(
      width = 9,
      div(
        class = "summary-box",
        h4("Summary"),
        verbatimTextOutput("summary", placeholder = TRUE)
      ),
      plotOutput("heatmap", height = "520px"),
      fluidRow(
        column(
          width = 6,
          h4("Top species in selection"),
          tableOutput("top_species")
        ),
        column(
          width = 6,
          div(
            class = "live-box",
            h4("Live now (last 24h API)"),
            tableOutput("live_now")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  excel_data <- reactiveVal(excel_birds_global)
  live_data <- reactiveVal(empty_detections_df())
  live_meta <- reactiveVal(list(fetched_at = NA, source = "none", error = NULL))
  available_species <- reactiveVal(character(0))
  syncing_species_group <- reactiveVal(FALSE)

  update_filters_from <- function(birds) {
    if (is.null(birds) || nrow(birds) == 0) {
      return(invisible(NULL))
    }

    species <- sort(unique(birds$Species))
    available_species(species)
    updateSelectizeInput(session, "species", choices = species, server = TRUE)

    date_min <- min(birds$date_col)
    date_max <- max(birds$date_col)
    updateDateRangeInput(session, "date_range", start = date_min, end = date_max, min = date_min, max = date_max)

    score_max <- suppressWarnings(max(birds$Score, na.rm = TRUE))
    if (!is.finite(score_max) || score_max <= 0) score_max <- 1
    updateSliderInput(session, "min_score", max = score_max, value = isolate(input$min_score) %||% 0)
  }

  refresh_excel <- function() {
    birds <- load_birds()
    excel_birds_global <<- birds
    excel_data(birds)
    update_filters_from(merge_excel_and_live(birds, live_data()))
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

  # Seed session from in-memory Excel + disk cache (no per-session Excel reload)
  observe({
    excel <- excel_data()
    if (!is.null(excel)) {
      update_filters_from(excel)
    }
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

  observeEvent(input$reload, {
    refresh_excel()
    showNotification("Excel data reloaded", type = "message")
  })

  observeEvent(input$refresh_live, {
    refresh_live(notify = TRUE, network = TRUE)
  })

  observeEvent(input$species_group, {
    group <- input$species_group
    if (identical(group, "custom")) {
      return()
    }

    selected <- if (identical(group, "all")) {
      character(0)
    } else {
      match_species_group(available_species(), group)
    }

    syncing_species_group(TRUE)
    updateSelectizeInput(
      session,
      "species",
      choices = available_species(),
      selected = selected,
      server = TRUE
    )
  }, ignoreInit = TRUE)

  observeEvent(input$species, {
    if (isTRUE(syncing_species_group())) {
      syncing_species_group(FALSE)
      return()
    }

    inferred <- infer_species_group(input$species, available_species())
    if (!identical(isolate(input$species_group), inferred)) {
      updateRadioButtons(session, "species_group", selected = inferred)
    }
  }, ignoreNULL = FALSE)

  filtered <- reactive({
    birds <- birds_data()
    req(birds)

    out <- birds

    if (!is.null(input$species) && length(input$species) > 0) {
      out <- out %>% filter(Species %in% input$species)
    }

    if (!is.null(input$date_range) && length(input$date_range) == 2 &&
        !any(is.na(input$date_range))) {
      out <- out %>%
        filter(date_col >= input$date_range[1], date_col <= input$date_range[2])
    }

    hours <- input$hour_range
    if (!is.null(hours) && length(hours) == 2) {
      out <- out %>% filter(time_col >= hours[1], time_col <= hours[2])
    }

    if (!is.null(input$min_score) && input$min_score > 0) {
      # Keep API rows (Score NA) so live detections still appear when filtering scores
      out <- out %>% filter(is.na(Score) | Score >= input$min_score)
    }

    out
  })

  heatmap_title <- reactive({
    group <- input$species_group
    if (is.null(group) || identical(group, "all")) {
      "All bird calls by date and hour"
    } else if (identical(group, "custom")) {
      n <- length(input$species)
      if (n == 0) {
        "All bird calls by date and hour"
      } else if (n == 1) {
        paste0(input$species[[1]], " calls by date and hour")
      } else {
        paste0(n, " selected species — calls by date and hour")
      }
    } else {
      paste0(species_group_label(group), " — calls by date and hour")
    }
  })

  output$summary <- renderText({
    df <- filtered()
    birds <- birds_data()
    meta <- live_meta()
    req(birds)

    sync_line <- if (is.na(meta$fetched_at)) {
      "Last API sync: never"
    } else {
      paste0(
        "Last API sync: ", format(meta$fetched_at, "%Y-%m-%d %H:%M:%S %Z"),
        " (", meta$source, ")"
      )
    }

    err_line <- if (!is.null(meta$error) && identical(meta$source, "cache")) {
      paste0("\nAPI note: ", meta$error, " (using cache)")
    } else if (!is.null(meta$error) && identical(meta$source, "none")) {
      paste0("\nAPI note: ", meta$error)
    } else {
      ""
    }

    if (nrow(df) == 0) {
      return(paste0("No rows match the current filters.\n", sync_line, err_line))
    }

    paste0(
      "Calls in selection: ", format(sum(df$Count, na.rm = TRUE), big.mark = ","), "\n",
      "Detections (rows): ", format(nrow(df), big.mark = ","), "\n",
      "Species: ", n_distinct(df$Species), "\n",
      "Date span: ", as.character(min(df$date_col)), " to ", as.character(max(df$date_col)), "\n",
      "Merged rows: ", format(nrow(birds), big.mark = ","), " / ",
      n_distinct(birds$Species), " species\n",
      "Live API detections: ", format(nrow(live_data()), big.mark = ","), "\n",
      sync_line,
      err_line
    )
  })

  output$live_now <- renderTable({
    live <- live_data()
    if (nrow(live) == 0) {
      return(tibble(Species = character(), Local_time = character(), Audio = character()))
    }

    live %>%
      slice_head(n = 20) %>%
      transmute(
        Species,
        Local_time = format(datetime, "%Y-%m-%d %H:%M:%S"),
        Audio = ifelse(
          !is.na(wav) & nzchar(wav),
          paste0("<a href=\"", wav, "\" target=\"_blank\" rel=\"noopener noreferrer\">listen</a>"),
          ""
        )
      )
  }, sanitize.text.function = identity, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$heatmap <- renderPlot({
    df <- filtered()
    agg <- aggregate_heatmap(df)
    sun <- NULL
    if (isTRUE(input$show_sun) && nrow(agg) > 0) {
      sun <- sun_curves_for_dates(
        agg$date_col,
        lat = haikubox_lat(),
        lon = haikubox_lon(),
        tz = haikubox_tz()
      )
    }
    plot_heatmap(
      agg,
      heatmap_title(),
      palette = input$palette %||% "magma",
      brightness = input$brightness %||% 0,
      reverse = isTRUE(input$reverse_palette),
      show_sun = isTRUE(input$show_sun),
      sun_curves = sun
    )
  })

  output$top_species <- renderTable({
    df <- filtered()
    if (nrow(df) == 0) {
      return(tibble(Species = character(), Calls = numeric()))
    }

    df %>%
      group_by(Species) %>%
      summarise(Calls = sum(Count, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Calls)) %>%
      slice_head(n = 15) %>%
      mutate(Calls = format(Calls, big.mark = ","))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
