# Approximate sunrise/sunset (NOAA-style) as local decimal hours.
# Defaults: Mount Vernon, ME (Haikubox location).

haikubox_lat <- function() {
  as.numeric(Sys.getenv("HAIKUBOX_LAT", unset = "44.5012"))
}

haikubox_lon <- function() {
  as.numeric(Sys.getenv("HAIKUBOX_LON", unset = "-69.9876"))
}

# Timezone offset from UTC in hours for a given date (handles DST).
.tz_offset_hours <- function(date, tz) {
  noon <- as.POSIXct(paste(as.Date(date), "12:00:00"), tz = tz)
  as.numeric(as.POSIXlt(noon)$gmtoff) / 3600
}

# Minutes from local midnight for sunrise (rise=TRUE) or sunset (rise=FALSE).
.sun_event_minutes <- function(date, lat, lon, tz, rise = TRUE) {
  date <- as.Date(date)
  n <- as.numeric(format(date, "%j"))
  gamma <- 2 * pi / 365 * (n - 1 + (12 - 12) / 24)

  eqtime <- 229.18 * (
    0.000075 +
      0.001868 * cos(gamma) - 0.032077 * sin(gamma) -
      0.014615 * cos(2 * gamma) - 0.040849 * sin(2 * gamma)
  )
  decl <- (
    0.006918 -
      0.399912 * cos(gamma) + 0.070257 * sin(gamma) -
      0.006758 * cos(2 * gamma) + 0.000907 * sin(2 * gamma) -
      0.002697 * cos(3 * gamma) + 0.00148 * sin(3 * gamma)
  )

  lat_r <- lat * pi / 180
  cos_ha <- (
    cos(90.833 * pi / 180) / (cos(lat_r) * cos(decl)) -
      tan(lat_r) * tan(decl)
  )
  cos_ha <- max(-1, min(1, cos_ha))
  ha <- acos(cos_ha) * 180 / pi
  if (!rise) {
    ha <- -ha
  }

  tz_off <- .tz_offset_hours(date, tz)
  time_offset <- eqtime + 4 * lon - 60 * tz_off
  720 - 4 * ha - time_offset
}

sun_curves_for_dates <- function(
    dates,
    lat = haikubox_lat(),
    lon = haikubox_lon(),
    tz = haikubox_tz()
) {
  dates <- sort(unique(as.Date(dates)))
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0 || !is.finite(lat) || !is.finite(lon)) {
    return(tibble::tibble(
      date = as.Date(character()),
      sunrise_hour = numeric(),
      sunset_hour = numeric()
    ))
  }

  sunrise_min <- vapply(dates, .sun_event_minutes, numeric(1), lat = lat, lon = lon, tz = tz, rise = TRUE)
  sunset_min <- vapply(dates, .sun_event_minutes, numeric(1), lat = lat, lon = lon, tz = tz, rise = FALSE)

  tibble::tibble(
    date = dates,
    sunrise_hour = pmax(0, pmin(23.99, sunrise_min / 60)),
    sunset_hour = pmax(0, pmin(23.99, sunset_min / 60))
  )
}
