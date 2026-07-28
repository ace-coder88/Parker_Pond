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
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No calls match the current filters") +
        ggplot2::theme_void()
    )
  }

  brightness <- max(-1, min(1, as.numeric(brightness)))
  if (brightness >= 0) {
    begin <- brightness * 0.45
    end <- 1
  } else {
    begin <- 0
    end <- 1 + brightness * 0.45
  }

  p <- ggplot2::ggplot(agg, ggplot2::aes(x = date_col, y = time_col, fill = Count)) +
    ggplot2::geom_raster(interpolate = FALSE) +
    ggplot2::scale_fill_viridis_c(
      option = palette,
      name = "# Calls",
      begin = begin,
      end = end,
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

  has_curves <- isTRUE(show_curves) &&
    !is.null(curves) &&
    nrow(curves) > 0 &&
    !is.null(rise_col) &&
    !is.null(set_col) &&
    rise_col %in% names(curves) &&
    set_col %in% names(curves)

  if (has_curves) {
    if (is.null(curve_colors)) {
      curve_colors <- stats::setNames(c("#FFE082", "#80DEEA"), c(rise_label, set_label))
    }

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
        ) +
        ggplot2::scale_color_manual(name = NULL, values = curve_colors) +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linewidth = 1.2)))
    }
  }

  has_phases <- !is.null(phase_marks) && nrow(phase_marks) > 0
  if (has_phases) {
    if (is.null(phase_colors)) {
      phase_colors <- c("Full moon" = "#FFF59D", "New moon" = "#ECEFF1")
    }
    p <- p +
      ggplot2::geom_point(
        data = phase_marks,
        ggplot2::aes(x = date, y = hour, color = series, shape = series),
        inherit.aes = FALSE,
        size = 3.2,
        stroke = 0.8
      ) +
      ggplot2::scale_color_manual(name = NULL, values = phase_colors) +
      ggplot2::scale_shape_manual(name = NULL, values = c("Full moon" = 16, "New moon" = 1)) +
      ggplot2::guides(
        color = ggplot2::guide_legend(override.aes = list(size = 3.5)),
        shape = ggplot2::guide_legend()
      )
  }

  p
}
