# Full moon / new moon markers (via suncalc phase).

.posix_to_hour <- function(x) {
  if (length(x) == 0) {
    return(numeric())
  }
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  if (any(ok)) {
    lt <- as.POSIXlt(x[ok])
    out[ok] <- lt$hour + lt$min / 60 + lt$sec / 3600
  }
  out
}

.phase_distance <- function(phase, target) {
  # phase is on [0, 1) circle; target 0 = new, 0.5 = full
  d <- abs(phase - target)
  pmin(d, 1 - d)
}

.local_minima_idx <- function(x) {
  n <- length(x)
  if (n < 3) {
    return(integer())
  }
  which(x[-c(1, n)] <= x[-c(n - 1L, n)] & x[-c(1, n)] < x[-c(1L, 2L)]) + 1L
}

.refine_phase_hour <- function(date, target, tz) {
  hours <- 0:23
  stamps <- as.POSIXct(
    paste(as.Date(date), sprintf("%02d:00:00", hours)),
    tz = tz
  )
  illum <- suncalc::getMoonIllumination(date = stamps)
  dists <- .phase_distance(illum$phase, target)
  hours[[which.min(dists)]]
}

#' Return full/new moon marks in a date range as local decimal hours.
moon_phase_marks_for_dates <- function(
    dates,
    tz = haikubox_tz()
) {
  dates <- sort(unique(as.Date(dates)))
  dates <- dates[!is.na(dates)]
  empty <- tibble::tibble(
    date = as.Date(character()),
    hour = numeric(),
    series = character()
  )

  if (length(dates) == 0) {
    return(empty)
  }

  if (!requireNamespace("suncalc", quietly = TRUE)) {
    warning("Package 'suncalc' is required for full/new moon markers")
    return(empty)
  }

  # Pad one day on each side so edge extrema can be detected, then clip.
  pad <- seq(min(dates) - 1, max(dates) + 1, by = "1 day")
  noon <- as.POSIXct(paste(pad, "12:00:00"), tz = tz)
  illum <- tryCatch(
    suncalc::getMoonIllumination(date = noon),
    error = function(e) NULL
  )
  if (is.null(illum) || nrow(illum) == 0) {
    return(empty)
  }

  phase <- illum$phase
  dist_new <- .phase_distance(phase, 0)
  dist_full <- .phase_distance(phase, 0.5)

  new_idx <- .local_minima_idx(dist_new)
  full_idx <- .local_minima_idx(dist_full)

  # Keep only clear syzygies (within ~1.5 days of exact phase)
  new_idx <- new_idx[dist_new[new_idx] < 0.04]
  full_idx <- full_idx[dist_full[full_idx] < 0.04]

  marks <- dplyr::bind_rows(
    lapply(full_idx, function(i) {
      d <- pad[[i]]
      if (d < min(dates) || d > max(dates)) {
        return(NULL)
      }
      tibble::tibble(
        date = d,
        hour = as.numeric(.refine_phase_hour(d, 0.5, tz)),
        series = "Full moon"
      )
    }),
    lapply(new_idx, function(i) {
      d <- pad[[i]]
      if (d < min(dates) || d > max(dates)) {
        return(NULL)
      }
      tibble::tibble(
        date = d,
        hour = as.numeric(.refine_phase_hour(d, 0, tz)),
        series = "New moon"
      )
    })
  )

  if (is.null(marks) || nrow(marks) == 0) {
    return(empty)
  }

  marks$hour <- pmax(0, pmin(23.99, marks$hour))
  marks[order(marks$date, marks$series), , drop = FALSE]
}
