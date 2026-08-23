dashboard_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      class = "sidebar-panel",
      width = 3,
      shiny::selectizeInput(
        ns("species"),
        "Species",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Search species… (empty = all birds)")
      ),
      shiny::div(
        class = "species-group",
        shiny::radioButtons(
          ns("species_group"),
          "Species group",
          choices = species_group_choices,
          selected = "all"
        )
      ),
      shiny::helpText("Group radios fill the species box with matching names from the loaded data."),
      shiny::dateRangeInput(ns("date_range"), "Date range"),
      shiny::sliderInput(
        ns("hour_range"),
        "Hour of day",
        min = 0,
        max = 23,
        value = c(0, 23),
        step = 1,
        ticks = FALSE
      ),
      shiny::sliderInput(
        ns("min_score"),
        "Minimum detection score",
        min = 0,
        max = 1,
        value = 0,
        step = 0.01
      ),
      shiny::hr(),
      shiny::selectInput(
        ns("plot_type"),
        "Plot type",
        choices = c(
          "Hourly counts" = "hourly",
          "Density" = "density"
        ),
        selected = "hourly"
      ),
      shiny::selectInput(
        ns("palette"),
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
      shiny::sliderInput(
        ns("brightness"),
        "Brightness",
        min = -1,
        max = 1,
        value = 0,
        step = 0.05,
        ticks = FALSE
      ),
      shiny::checkboxInput(ns("reverse_palette"), "Reverse palette", value = FALSE),
      shiny::checkboxInput(ns("show_sun"), "Show sunrise / sunset", value = TRUE),
      shiny::checkboxInput(ns("show_moon"), "Show full / new moons", value = TRUE),
      shiny::helpText("Sun/moon overlays use Mount Vernon, ME coordinates (override with HAIKUBOX_LAT / HAIKUBOX_LON). Moon dots mark local peak phase."),
      shiny::actionButton(ns("reload"), "Reload data", class = "btn-primary", width = "100%"),
      shiny::br(), shiny::br(),
      shiny::fileInput(
        ns("upload_export"),
        "Upload Haikubox CSV / Excel",
        multiple = FALSE,
        accept = c(
          ".csv",
          ".txt",
          ".xlsx",
          ".xls",
          "text/csv",
          "text/plain",
          "application/vnd.ms-excel",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        ),
        width = "100%"
      ),
      shiny::helpText(
        "Download individual detections from listen.haikubox.com (All → Download CSV), ",
        "then upload here. Files are saved under data/uploads/ and preferred over the API sample archive."
      ),
      shiny::br(),
      shiny::actionButton(ns("refresh_live"), "Refresh live data", width = "100%"),
      shiny::br(), shiny::br(),
      shiny::helpText(
        "Live panel refreshes about every 10 minutes. The daily archiver only stores an API sample; ",
        "upload CSV exports for full hour-level history."
      )
    ),
    shiny::mainPanel(
      width = 9,
      shiny::div(
        class = "summary-box",
        shiny::h4("Summary"),
        shiny::verbatimTextOutput(ns("summary"), placeholder = TRUE)
      ),
      shiny::plotOutput(ns("heatmap"), height = "520px"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::h4("Top species in selection"),
          shiny::tableOutput(ns("top_species"))
        ),
        shiny::column(
          width = 6,
          shiny::div(
            class = "live-box",
            shiny::h4("Live now (last 24h API)"),
            shiny::tableOutput(ns("live_now"))
          )
        )
      )
    )
  )
}

dashboard_server <- function(
    id,
    birds_data,
    excel_data,
    live_data,
    live_meta,
    refresh_excel,
    refresh_live,
    data_dir = Sys.getenv("PARKER_DATA_DIR", unset = "data")
) {
  shiny::moduleServer(id, function(input, output, session) {
    available_species <- shiny::reactiveVal(character(0))
    syncing_species_group <- shiny::reactiveVal(FALSE)

    update_filters_from <- function(birds) {
      if (is.null(birds) || nrow(birds) == 0) {
        return(invisible(NULL))
      }

      species <- sort(unique(birds$Species))
      available_species(species)
      shiny::updateSelectizeInput(session, "species", choices = species, server = TRUE)

      date_min <- min(birds$date_col)
      date_max <- max(birds$date_col)
      shiny::updateDateRangeInput(
        session,
        "date_range",
        start = date_min,
        end = date_max,
        min = date_min,
        max = date_max
      )

      score_max <- suppressWarnings(max(birds$Score, na.rm = TRUE))
      if (!is.finite(score_max) || score_max <= 0) score_max <- 1
      shiny::updateSliderInput(
        session,
        "min_score",
        max = score_max,
        value = shiny::isolate(input$min_score) %||% 0
      )
    }

    # Seed / refresh filter choices when data reloads (not on live-only merges).
    shiny::observe({
      excel <- excel_data()
      if (!is.null(excel)) {
        update_filters_from(merge_excel_and_live(excel, shiny::isolate(live_data())))
      }
    })

    shiny::observeEvent(input$reload, {
      refresh_excel()
      shiny::showNotification("Data reloaded (Excel + uploads + archive)", type = "message")
    })

    shiny::observeEvent(input$upload_export, {
      f <- input$upload_export
      shiny::req(f)
      shiny::req(f$datapath)

      result <- tryCatch(
        save_haikubox_upload(
          src_path = f$datapath,
          original_name = f$name,
          data_dir = data_dir,
          tz = haikubox_tz()
        ),
        error = function(e) e
      )

      if (inherits(result, "error") || inherits(result, "condition")) {
        shiny::showNotification(
          paste("Upload failed:", conditionMessage(result)),
          type = "error",
          duration = 12
        )
        return()
      }

      refresh_excel()
      shiny::showNotification(
        paste0(
          "Uploaded ", format(result$n, big.mark = ","), " detections (",
          result$species, " species, ",
          as.character(result$date_min), " to ", as.character(result$date_max), ")"
        ),
        type = "message",
        duration = 10
      )
    })

    shiny::observeEvent(input$refresh_live, {
      refresh_live(notify = TRUE, network = TRUE)
    })

    shiny::observeEvent(input$species_group, {
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
      shiny::updateSelectizeInput(
        session,
        "species",
        choices = available_species(),
        selected = selected,
        server = TRUE
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$species, {
      if (isTRUE(syncing_species_group())) {
        syncing_species_group(FALSE)
        return()
      }

      inferred <- infer_species_group(input$species, available_species())
      if (!identical(shiny::isolate(input$species_group), inferred)) {
        shiny::updateRadioButtons(session, "species_group", selected = inferred)
      }
    }, ignoreNULL = FALSE)

    filtered <- shiny::reactive({
      birds <- birds_data()
      shiny::req(birds)

      out <- birds

      if (!is.null(input$species) && length(input$species) > 0) {
        out <- out %>% dplyr::filter(Species %in% input$species)
      }

      if (!is.null(input$date_range) && length(input$date_range) == 2 &&
          !any(is.na(input$date_range))) {
        out <- out %>%
          dplyr::filter(date_col >= input$date_range[1], date_col <= input$date_range[2])
      }

      hours <- input$hour_range
      if (!is.null(hours) && length(hours) == 2) {
        out <- out %>% dplyr::filter(time_col >= hours[1], time_col <= hours[2])
      }

      if (!is.null(input$min_score) && input$min_score > 0) {
        out <- out %>% dplyr::filter(is.na(Score) | Score >= input$min_score)
      }

      out
    })

    heatmap_title <- shiny::reactive({
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

    output$summary <- shiny::renderText({
      df <- filtered()
      birds <- birds_data()
      meta <- live_meta()
      shiny::req(birds)

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
        "Species: ", dplyr::n_distinct(df$Species), "\n",
        "Date span: ", as.character(min(df$date_col)), " to ", as.character(max(df$date_col)), "\n",
        "Merged rows: ", format(nrow(birds), big.mark = ","), " / ",
        dplyr::n_distinct(birds$Species), " species\n",
        "Live API detections: ", format(nrow(live_data()), big.mark = ","), "\n",
      {
        n_uploads <- length(list.files(
          uploads_dir(data_dir),
          pattern = "\\.(csv|txt|xlsx|xls)$",
          ignore.case = TRUE
        ))
        if (n_uploads > 0) {
          paste0("Uploaded export files: ", n_uploads, "\n")
        } else {
          "Uploaded export files: none\n"
        }
      },
      {
        arch <- tryCatch(
          archive_summary(archive_path(data_dir)),
          error = function(e) list(n = 0L, date_min = NA, date_max = NA)
        )
        if (is.null(arch$n) || arch$n == 0) {
          "Archive: empty (daily archiver not run yet)\n"
        } else {
          paste0(
            "Archive: ", format(arch$n, big.mark = ","), " rows (",
            as.character(arch$date_min), " to ", as.character(arch$date_max), ")\n"
          )
        }
      },
      sync_line,
      err_line
    )
    })

    output$live_now <- shiny::renderTable({
      live <- live_data()
      if (nrow(live) == 0) {
        return(tibble::tibble(Species = character(), Local_time = character(), Audio = character()))
      }

      live %>%
        dplyr::slice_head(n = 20) %>%
        dplyr::transmute(
          Species,
          Local_time = format(datetime, "%Y-%m-%d %H:%M:%S"),
          Audio = ifelse(
            !is.na(wav) & nzchar(wav),
            paste0("<a href=\"", wav, "\" target=\"_blank\" rel=\"noopener noreferrer\">listen</a>"),
            ""
          )
        )
    }, sanitize.text.function = identity, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$heatmap <- shiny::renderPlot({
      df <- filtered()
      plot_type <- input$plot_type %||% "hourly"
      use_density <- identical(plot_type, "density")

      date_vals <- if (nrow(df) > 0) {
        unique(as.Date(df$datetime))
      } else {
        as.Date(character())
      }

      curves <- NULL
      phase_marks <- NULL
      show_sun <- isTRUE(input$show_sun)
      show_moon <- isTRUE(input$show_moon)

      if (length(date_vals) > 0) {
        if (show_sun) {
          curves <- sun_curves_for_dates(
            date_vals,
            lat = haikubox_lat(),
            lon = haikubox_lon(),
            tz = haikubox_tz()
          )
        }
        if (show_moon) {
          phase_marks <- moon_phase_marks_for_dates(
            date_vals,
            tz = haikubox_tz()
          )
        }
      }

      overlay_args <- list(
        show_curves = show_sun,
        curves = curves,
        rise_col = "sunrise_hour",
        set_col = "sunset_hour",
        rise_label = "Sunrise",
        set_label = "Sunset",
        curve_colors = c(Sunrise = "#FFE082", Sunset = "#80DEEA"),
        phase_marks = phase_marks,
        phase_colors = c("Full moon" = "#FFF59D", "New moon" = "#B0BEC5")
      )

      common <- list(
        title = heatmap_title(),
        palette = input$palette %||% "magma",
        brightness = input$brightness %||% 0,
        reverse = isTRUE(input$reverse_palette)
      )

      if (use_density) {
        do.call(
          plot_density_heatmap,
          c(list(df = df), common, overlay_args)
        )
      } else {
        do.call(
          plot_heatmap,
          c(list(agg = aggregate_heatmap(df)), common, overlay_args)
        )
      }
    })

    output$top_species <- shiny::renderTable({
      df <- filtered()
      if (nrow(df) == 0) {
        return(tibble::tibble(Species = character(), Calls = numeric()))
      }

      df %>%
        dplyr::group_by(Species) %>%
        dplyr::summarise(Calls = sum(Count, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(Calls)) %>%
        dplyr::slice_head(n = 15) %>%
        dplyr::mutate(Calls = format(Calls, big.mark = ","))
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
  })
}
