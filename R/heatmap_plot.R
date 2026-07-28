aggregate_heatmap <- function(df) {
  if (nrow(df) == 0) {
    return(tibble::tibble(
      date_col = as.Date(character()),
      time_col = numeric(),
      Count = numeric()
    ))
  }

  date_min <- min(df$date_col)
  date_max <- max(df$date_col)

  df %>%
    dplyr::filter(!is.na(date_col), !is.na(time_col)) %>%
    dplyr::group_by(date_col, time_col) %>%
    dplyr::summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(
      date_col = seq(date_min, date_max, by = "1 day"),
      time_col = 0:23,
      fill = list(Count = 0)
    ) %>%
    dplyr::filter(!is.na(time_col))
}

decimal_hour_of_day <- function(datetime) {
  if (length(datetime) == 0) {
    return(numeric())
  }
  lt <- as.POSIXlt(datetime)
  lt$hour + lt$min / 60 + lt$sec / 3600
}

.palette_begin_end <- function(brightness) {
  brightness <- max(-1, min(1, as.numeric(brightness)))
  if (brightness >= 0) {
    list(begin = brightness * 0.45, end = 1)
  } else {
    list(begin = 0, end = 1 + brightness * 0.45)
  }
}

.empty_calls_plot <- function() {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No calls match the current filters") +
    ggplot2::theme_void()
}

# Break line segments when time-of-day wraps (e.g. moonrise jumping 23:00 → 00:30).
.curve_segments <- function(df, max_jump_hours = 6) {
  if (nrow(df) == 0) {
    return(dplyr::mutate(df, segment = integer()))
  }

  df <- df[order(df$series, df$date), , drop = FALSE]
  seg <- integer(nrow(df))
  current <- 1L
  seg[[1]] <- current

  if (nrow(df) > 1) {
    for (i in 2:nrow(df)) {
      same_series <- identical(df$series[[i]], df$series[[i - 1]])
      jump <- abs(df$hour[[i]] - df$hour[[i - 1]])
      day_gap <- as.numeric(df$date[[i]] - df$date[[i - 1]])
      if (!same_series || is.na(jump) || jump > max_jump_hours || is.na(day_gap) || day_gap > 2) {
        current <- current + 1L
      }
      seg[[i]] <- current
    }
  }

  df$segment <- seg
  df
}

.add_sky_overlays <- function(
    p,
    show_curves = TRUE,
    curves = NULL,
    rise_col = NULL,
    set_col = NULL,
    rise_label = "Rise",
    set_label = "Set",
    curve_colors = NULL,
    phase_marks = NULL,
    phase_colors = NULL
) {
  has_curves <- isTRUE(show_curves) &&
    !is.null(curves) &&
    nrow(curves) > 0 &&
    !is.null(rise_col) &&
    !is.null(set_col) &&
    rise_col %in% names(curves) &&
    set_col %in% names(curves)

  has_phases <- !is.null(phase_marks) && nrow(phase_marks) > 0

  if (is.null(curve_colors)) {
    curve_colors <- stats::setNames(c("#FFE082", "#80DEEA"), c(rise_label, set_label))
  }
  if (is.null(phase_colors)) {
    phase_colors <- c("Full moon" = "#FFF59D", "New moon" = "#B0BEC5")
  }

  color_values <- c()
  if (has_curves) {
    curve_long <- dplyr::bind_rows(
      tibble::tibble(
        date = curves$date,
        hour = curves[[rise_col]],
        series = rise_label
      ),
      tibble::tibble(
        date = curves$date,
        hour = curves[[set_col]],
        series = set_label
      )
    )
    curve_long <- curve_long[!is.na(curve_long$hour), , drop = FALSE]
    curve_long <- .curve_segments(curve_long)

    if (nrow(curve_long) > 0) {
      p <- p +
        ggplot2::geom_line(
          data = curve_long,
          ggplot2::aes(x = date, y = hour, color = series, group = interaction(series, segment)),
          inherit.aes = FALSE,
          linewidth = 0.7,
          alpha = 0.95
        )
      color_values <- c(color_values, curve_colors)
    } else {
      has_curves <- FALSE
    }
  }

  if (has_phases) {
    p <- p +
      ggplot2::geom_point(
        data = phase_marks,
        ggplot2::aes(x = date, y = hour, color = series, shape = series),
        inherit.aes = FALSE,
        size = 3.2,
        stroke = 0.8
      )
    color_values <- c(color_values, phase_colors)
  }

  if (length(color_values) > 0) {
    # Drop duplicate names if any; keep first
    color_values <- color_values[!duplicated(names(color_values))]
    p <- p + ggplot2::scale_color_manual(name = NULL, values = color_values)
  }

  if (has_phases) {
    p <- p +
      ggplot2::scale_shape_manual(
        name = NULL,
        values = c("Full moon" = 16, "New moon" = 1)
      )
  }

  if (has_curves && has_phases) {
    legend_names <- names(color_values)
    ov_shape <- ifelse(legend_names %in% c("Full moon"), 16,
                  ifelse(legend_names %in% c("New moon"), 1, NA_real_))
    ov_lwd <- ifelse(legend_names %in% c(rise_label, set_label), 1.2, NA_real_)
    ov_size <- ifelse(legend_names %in% c("Full moon", "New moon"), 3.5, NA_real_)
    p <- p +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(shape = ov_shape, linewidth = ov_lwd, size = ov_size)
        ),
        shape = "none"
      )
  } else if (has_curves) {
    p <- p +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linewidth = 1.2)))
  } else if (has_phases) {
    p <- p +
      ggplot2::guides(
        color = ggplot2::guide_legend(override.aes = list(size = 3.5)),
        shape = ggplot2::guide_legend()
      )
  }

  p
}

plot_heatmap <- function(
    agg,
    title,
    palette = "magma",
    brightness = 0,
    reverse = FALSE,
    show_curves = TRUE,
    curves = NULL,
    rise_col = NULL,
    set_col = NULL,
    rise_label = "Rise",
    set_label = "Set",
    curve_colors = NULL,
    phase_marks = NULL,
    phase_colors = NULL
) {
  if (nrow(agg) == 0) {
    return(.empty_calls_plot())
  }

  be <- .palette_begin_end(brightness)

  p <- ggplot2::ggplot(agg, ggplot2::aes(x = date_col, y = time_col, fill = Count)) +
    ggplot2::geom_raster(interpolate = FALSE) +
    ggplot2::scale_fill_viridis_c(
      option = palette,
      name = "# Calls",
      begin = be$begin,
      end = be$end,
      direction = if (isTRUE(reverse)) -1 else 1
    ) +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    ggplot2::scale_y_continuous(breaks = seq(0, 23, by = 3), limits = c(0, 23), expand = c(0, 0)) +
    ggplot2::labs(y = "Time of Day", title = title) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

  .add_sky_overlays(
    p,
    show_curves = show_curves,
    curves = curves,
    rise_col = rise_col,
    set_col = set_col,
    rise_label = rise_label,
    set_label = set_label,
    curve_colors = curve_colors,
    phase_marks = phase_marks,
    phase_colors = phase_colors
  )
}

#' Density heatmap from raw detection datetimes (2D KDE raster).
plot_density_heatmap <- function(
    df,
    title,
    palette = "magma",
    brightness = 0,
    reverse = FALSE,
    bins = c(180, 48),
    show_curves = TRUE,
    curves = NULL,
    rise_col = NULL,
    set_col = NULL,
    rise_label = "Rise",
    set_label = "Set",
    curve_colors = NULL,
    phase_marks = NULL,
    phase_colors = NULL
) {
  if (is.null(df) || nrow(df) == 0) {
    return(.empty_calls_plot())
  }

  ok <- !is.na(df$datetime) & !is.na(df$Count) & df$Count > 0
  pts <- df[ok, , drop = FALSE]
  pts$date_col <- as.Date(pts$datetime)
  pts$hour <- decimal_hour_of_day(pts$datetime)
  pts <- pts[!is.na(pts$date_col) & !is.na(pts$hour) & pts$hour >= 0 & pts$hour < 24, , drop = FALSE]

  if (nrow(pts) == 0) {
    return(.empty_calls_plot())
  }

  # Expand rare Count>1 rows so density reflects call volume (Count is usually 1).
  if (any(pts$Count > 1, na.rm = TRUE)) {
    pts <- pts[rep(seq_len(nrow(pts)), pmax(1L, as.integer(round(pts$Count)))), , drop = FALSE]
  }

  be <- .palette_begin_end(brightness)
  pts$date_num <- as.numeric(pts$date_col)
  date_range <- range(pts$date_num, na.rm = TRUE)

  # Continuous KDE field (not coarse rectangular bins).
  p <- ggplot2::ggplot(pts, ggplot2::aes(x = date_num, y = hour)) +
    ggplot2::stat_density_2d(
      ggplot2::aes(fill = ggplot2::after_stat(density)),
      geom = "raster",
      contour = FALSE,
      n = 200
    ) +
    ggplot2::scale_fill_viridis_c(
      option = palette,
      name = "Density",
      begin = be$begin,
      end = be$end,
      direction = if (isTRUE(reverse)) -1 else 1
    ) +
    ggplot2::scale_x_continuous(
      breaks = as.numeric(seq(
        as.Date(date_range[[1]], origin = "1970-01-01"),
        as.Date(date_range[[2]], origin = "1970-01-01"),
        by = "1 month"
      )),
      labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%b"),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 23, by = 3),
      limits = c(0, 24),
      expand = c(0, 0),
      oob = scales::squish
    ) +
    ggplot2::labs(y = "Time of Day", title = title) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::coord_cartesian(xlim = date_range, ylim = c(0, 24), expand = FALSE)

  # Overlays expect Date on x; convert mark/curve dates to numeric for this scale.
  if (!is.null(curves) && nrow(curves) > 0 && "date" %in% names(curves)) {
    curves <- curves
    curves$date <- as.numeric(as.Date(curves$date))
  }
  if (!is.null(phase_marks) && nrow(phase_marks) > 0 && "date" %in% names(phase_marks)) {
    phase_marks <- phase_marks
    phase_marks$date <- as.numeric(as.Date(phase_marks$date))
  }

  .add_sky_overlays(
    p,
    show_curves = show_curves,
    curves = curves,
    rise_col = rise_col,
    set_col = set_col,
    rise_label = rise_label,
    set_label = set_label,
    curve_colors = curve_colors,
    phase_marks = phase_marks,
    phase_colors = phase_colors
  )
}
