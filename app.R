library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)

data_dir <- Sys.getenv("PARKER_DATA_DIR", unset = "data")

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

plot_heatmap <- function(agg, title) {
  if (nrow(agg) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No calls match the current filters") +
        theme_void()
    )
  }

  ggplot(agg, aes(x = date_col, y = time_col, fill = Count)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_viridis_c(option = "magma", name = "# Calls") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(breaks = seq(0, 23, by = 3)) +
    labs(y = "Time of Day", title = title) +
    theme_minimal(base_size = 13) +
    theme(
      axis.title.x = element_blank(),
      panel.grid = element_blank()
    )
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
      selectInput(
        "preset",
        "Preset",
        choices = c("All birds" = "all", "Owls" = "owls", "Custom species" = "custom"),
        selected = "all"
      ),
      helpText("Top species presets update when data is reloaded."),
      conditionalPanel(
        condition = "input.preset == 'custom'",
        selectizeInput(
          "species",
          "Species",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Search species…")
        )
      ),
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
      actionButton("reload", "Reload data", class = "btn-primary", width = "100%"),
      br(), br(),
      helpText("Drop new monthly Excel files into the data folder, then reload.")
    ),
    mainPanel(
      width = 9,
      div(
        class = "summary-box",
        h4("Summary"),
        verbatimTextOutput("summary", placeholder = TRUE)
      ),
      plotOutput("heatmap", height = "520px"),
      h4("Top species in selection"),
      tableOutput("top_species")
    )
  )
)

server <- function(input, output, session) {
  birds_data <- reactiveVal(NULL)

  refresh_data <- function() {
    birds <- load_birds()
    birds_data(birds)

    species <- sort(unique(birds$Species))
    updateSelectizeInput(session, "species", choices = species, server = TRUE)

    top10 <- birds %>%
      group_by(Species) %>%
      summarise(Calls = sum(Count, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Calls)) %>%
      slice_head(n = 10) %>%
      pull(Species)

    preset_choices <- c(
      "All birds" = "all",
      "Owls" = "owls",
      setNames(top10, top10),
      "Custom species" = "custom"
    )
    current <- isolate(input$preset)
    selected <- if (!is.null(current) && current %in% preset_choices) current else "all"
    updateSelectInput(session, "preset", choices = preset_choices, selected = selected)

    date_min <- min(birds$date_col)
    date_max <- max(birds$date_col)
    updateDateRangeInput(session, "date_range", start = date_min, end = date_max, min = date_min, max = date_max)

    score_max <- suppressWarnings(max(birds$Score, na.rm = TRUE))
    if (!is.finite(score_max) || score_max <= 0) score_max <- 1
    updateSliderInput(session, "min_score", max = score_max, value = 0)
  }

  observe({
    refresh_data()
  })

  observeEvent(input$reload, {
    refresh_data()
    showNotification("Data reloaded", type = "message")
  })

  filtered <- reactive({
    birds <- birds_data()
    req(birds)

    out <- birds

    preset <- input$preset
    if (identical(preset, "owls")) {
      out <- out %>% filter(grepl("Owl", Species, ignore.case = TRUE))
    } else if (identical(preset, "custom")) {
      req(length(input$species) > 0)
      out <- out %>% filter(Species %in% input$species)
    } else if (!identical(preset, "all") && !is.null(preset) && nzchar(preset)) {
      out <- out %>% filter(Species == preset)
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
      out <- out %>% filter(!is.na(Score), Score >= input$min_score)
    }

    out
  })

  heatmap_title <- reactive({
    preset <- input$preset
    if (identical(preset, "owls")) {
      "Owl calls by date and hour"
    } else if (identical(preset, "custom")) {
      n <- length(input$species)
      if (n == 1) {
        paste0(input$species[[1]], " calls by date and hour")
      } else {
        paste0(n, " selected species — calls by date and hour")
      }
    } else if (identical(preset, "all") || is.null(preset)) {
      "All bird calls by date and hour"
    } else {
      paste0(preset, " calls by date and hour")
    }
  })

  output$summary <- renderText({
    df <- filtered()
    birds <- birds_data()
    req(birds)

    if (nrow(df) == 0) {
      return("No rows match the current filters.")
    }

    paste0(
      "Calls in selection: ", format(sum(df$Count, na.rm = TRUE), big.mark = ","), "\n",
      "Detections (rows): ", format(nrow(df), big.mark = ","), "\n",
      "Species: ", n_distinct(df$Species), "\n",
      "Date span: ", as.character(min(df$date_col)), " to ", as.character(max(df$date_col)), "\n",
      "Loaded from disk: ", format(nrow(birds), big.mark = ","), " rows / ",
      n_distinct(birds$Species), " species"
    )
  })

  output$heatmap <- renderPlot({
    df <- filtered()
    agg <- aggregate_heatmap(df)
    plot_heatmap(agg, heatmap_title())
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
