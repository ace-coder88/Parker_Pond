# Parse Haikubox listen-site CSV / Excel detection exports into the app schema.

empty_birds_df <- function() {
  tibble::tibble(
    Species = character(),
    scientific_name = character(),
    datetime = as.POSIXct(character()),
    Count = numeric(),
    Score = numeric(),
    date_col = as.Date(character()),
    time_col = numeric()
  )
}

uploads_dir <- function(data_dir = Sys.getenv("PARKER_DATA_DIR", unset = "data")) {
  file.path(data_dir, "uploads")
}

.norm_col <- function(x) {
  x <- tolower(trimws(as.character(x)))
  gsub("[^a-z0-9]+", "_", x)
}

.pick_col <- function(nm, candidates) {
  hit <- intersect(candidates, nm)
  if (length(hit) == 0) {
    return(NULL)
  }
  hit[[1]]
}

#' Normalize a raw Haikubox export data frame to birds schema.
normalize_haikubox_export <- function(raw, tz = haikubox_tz()) {
  if (is.null(raw) || nrow(raw) == 0) {
    return(empty_birds_df())
  }

  nm_orig <- names(raw)
  nm <- .norm_col(nm_orig)
  names(raw) <- nm

  species_col <- .pick_col(nm, c("species", "common_name", "common", "bird", "cn", "name"))
  sci_col <- .pick_col(nm, c("scientific_name", "scientific", "sci_name", "sn"))
  score_col <- .pick_col(nm, c("score", "confidence", "conf"))
  count_col <- .pick_col(nm, c("count", "counts", "n", "detections"))

  local_date_col <- .pick_col(nm, c("local_date", "date", "day"))
  local_time_col <- .pick_col(nm, c("local_time", "time"))
  datetime_col <- .pick_col(
    nm,
    c(
      "datetime", "local_datetime", "timestamp", "dt",
      "detection_time", "observed_at", "utc_datetime"
    )
  )
  utc_date_col <- .pick_col(nm, c("utc_date"))
  utc_time_col <- .pick_col(nm, c("utc_time"))

  if (is.null(species_col)) {
    stop(
      "Could not find a species column. Expected something like ",
      "'Species', 'Common Name', or 'bird'. Columns: ",
      paste(nm_orig, collapse = ", ")
    )
  }

  datetime <- rep(as.POSIXct(NA), nrow(raw))

  if (!is.null(datetime_col)) {
    raw_dt <- as.character(raw[[datetime_col]])
    datetime <- suppressWarnings(lubridate::ymd_hms(raw_dt, tz = tz, quiet = TRUE))
    if (all(is.na(datetime))) {
      datetime <- suppressWarnings(lubridate::ymd_hm(raw_dt, tz = tz, quiet = TRUE))
    }
    if (all(is.na(datetime))) {
      datetime <- suppressWarnings(
        as.POSIXct(raw_dt, tz = tz, origin = "1970-01-01")
      )
    }
  }

  if (all(is.na(datetime)) && !is.null(local_date_col) && !is.null(local_time_col)) {
    date_chr <- as.character(raw[[local_date_col]])
    time_chr <- as.character(raw[[local_time_col]])
    # Excel-ish times often arrive as "1899-12-31 HH:MM:SS"
    has_tod <- grepl("\\d{1,2}:\\d{2}", time_chr)
    if (any(has_tod)) {
      time_chr[has_tod] <- regmatches(
        time_chr[has_tod],
        regexpr("\\d{1,2}:\\d{2}(:\\d{2})?", time_chr[has_tod])
      )
    }
    datetime <- suppressWarnings(
      lubridate::ymd_hms(paste(date_chr, time_chr), tz = tz, quiet = TRUE)
    )
    if (all(is.na(datetime))) {
      datetime <- suppressWarnings(
        lubridate::parse_date_time(
          paste(date_chr, time_chr),
          orders = c("Ymd HMS", "Ymd HM", "mdy HMS", "mdy HM"),
          tz = tz,
          quiet = TRUE
        )
      )
    }
  }

  if (all(is.na(datetime)) && !is.null(utc_date_col) && !is.null(utc_time_col)) {
    date_chr <- as.character(raw[[utc_date_col]])
    time_chr <- as.character(raw[[utc_time_col]])
    has_tod <- grepl("\\d{1,2}:\\d{2}", time_chr)
    if (any(has_tod)) {
      time_chr[has_tod] <- regmatches(
        time_chr[has_tod],
        regexpr("\\d{1,2}:\\d{2}(:\\d{2})?", time_chr[has_tod])
      )
    }
    datetime_utc <- suppressWarnings(
      lubridate::ymd_hms(paste(date_chr, time_chr), tz = "UTC", quiet = TRUE)
    )
    datetime <- lubridate::with_tz(datetime_utc, tzone = tz)
  }

  if (all(is.na(datetime))) {
    stop(
      "Could not parse detection timestamps. Need either a datetime column, ",
      "Local Date + Local Time, or UTC Date + UTC Time. Columns: ",
      paste(nm_orig, collapse = ", ")
    )
  }

  score <- if (!is.null(score_col)) {
    as.numeric(raw[[score_col]])
  } else {
    rep(NA_real_, nrow(raw))
  }
  # Confidence labels → rough numeric scores
  if (all(is.na(score)) && !is.null(score_col)) {
    lab <- tolower(as.character(raw[[score_col]]))
    score <- dplyr::case_when(
      lab %in% c("high", "h") ~ 0.9,
      lab %in% c("medium", "med", "m") ~ 0.6,
      lab %in% c("low", "l") ~ 0.3,
      TRUE ~ NA_real_
    )
  }

  count <- if (!is.null(count_col)) {
    as.numeric(raw[[count_col]])
  } else {
    rep(1, nrow(raw))
  }
  count[!is.finite(count) | count <= 0] <- 1

  sci <- if (!is.null(sci_col)) {
    as.character(raw[[sci_col]])
  } else {
    rep(NA_character_, nrow(raw))
  }

  out <- tibble::tibble(
    Species = as.character(raw[[species_col]]),
    scientific_name = sci,
    datetime = as.POSIXct(datetime),
    Count = count,
    Score = score,
    date_col = as.Date(datetime),
    time_col = as.numeric(format(datetime, "%H"))
  ) |>
    dplyr::filter(
      !is.na(Species),
      nzchar(Species),
      tolower(Species) != "soundscape",
      !is.na(datetime)
    )

  if (nrow(out) == 0) {
    stop("File parsed but no valid detection rows remained.")
  }

  out
}

read_haikubox_export_file <- function(path, tz = haikubox_tz()) {
  ext <- tolower(tools::file_ext(path))
  raw <- if (ext %in% c("csv", "txt")) {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(path)
  } else {
    stop("Unsupported file type: .", ext, " (use .csv, .xlsx, or .xls)")
  }
  normalize_haikubox_export(raw, tz = tz)
}

load_upload_birds <- function(dir = Sys.getenv("PARKER_DATA_DIR", unset = "data"), tz = haikubox_tz()) {
  up <- uploads_dir(dir)
  if (!dir.exists(up)) {
    return(empty_birds_df())
  }
  files <- list.files(up, pattern = "\\.(csv|txt|xlsx|xls)$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    return(empty_birds_df())
  }

  parts <- lapply(files, function(f) {
    tryCatch(
      read_haikubox_export_file(f, tz = tz),
      error = function(e) {
        warning("Skipping upload ", basename(f), ": ", conditionMessage(e))
        empty_birds_df()
      }
    )
  })
  dplyr::bind_rows(parts)
}

#' Save an uploaded export under data/uploads/ and return parse summary.
save_haikubox_upload <- function(
    src_path,
    original_name,
    data_dir = Sys.getenv("PARKER_DATA_DIR", unset = "data"),
    tz = haikubox_tz()
) {
  # Validate before keeping the file
  parsed <- read_haikubox_export_file(src_path, tz = tz)

  dest_dir <- uploads_dir(data_dir)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  safe_name <- gsub("[^A-Za-z0-9._-]+", "_", basename(original_name))
  if (!nzchar(safe_name)) {
    safe_name <- "upload.csv"
  }
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  dest <- file.path(dest_dir, paste0(stamp, "-", safe_name))

  ok <- file.copy(src_path, dest, overwrite = FALSE)
  if (!isTRUE(ok)) {
    stop("Failed to save upload to ", dest)
  }

  list(
    path = dest,
    n = nrow(parsed),
    species = dplyr::n_distinct(parsed$Species),
    date_min = min(parsed$date_col),
    date_max = max(parsed$date_col),
    calls = sum(parsed$Count, na.rm = TRUE)
  )
}
