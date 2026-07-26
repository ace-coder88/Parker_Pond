# Haikubox public API helpers (no account key — serial in URL only).
# Docs: https://api.haikubox.com/docs

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

haikubox_serial <- function() {
  Sys.getenv("HAIKUBOX_SERIAL", unset = "64E8334476B0")
}

haikubox_tz <- function() {
  Sys.getenv("HAIKUBOX_TZ", unset = "America/New_York")
}

haikubox_cache_path <- function(data_dir = Sys.getenv("PARKER_DATA_DIR", unset = "data")) {
  file.path(data_dir, "cache", "detections.json")
}

empty_detections_df <- function() {
  tibble::tibble(
    Species = character(),
    scientific_name = character(),
    datetime = as.POSIXct(character()),
    Count = numeric(),
    Score = numeric(),
    date_col = as.Date(character()),
    time_col = numeric(),
    wav = character(),
    sp_code = character()
  )
}

fetch_detections_raw <- function(serial = haikubox_serial(), hours = 24L) {
  hours <- as.integer(hours)
  if (is.na(hours) || hours < 1L || hours > 24L) {
    stop("hours must be an integer between 1 and 24")
  }

  url <- sprintf(
    "https://api.haikubox.com/haikubox/%s/detections?hours=%d",
    serial,
    hours
  )

  req <- httr2::request(url) |>
    httr2::req_timeout(8) |>
    httr2::req_retry(max_tries = 2) |>
    httr2::req_error(is_error = \(resp) FALSE)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (status < 200 || status >= 300) {
    stop("Haikubox API returned HTTP ", status)
  }

  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

write_detections_cache <- function(payload, path = haikubox_cache_path(), serial = haikubox_serial()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  envelope <- list(
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    serial = serial,
    payload = payload
  )
  jsonlite::write_json(envelope, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible
}

read_detections_cache <- function(path = haikubox_cache_path()) {
  if (!file.exists(path)) {
    return(NULL)
  }
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

normalize_detections <- function(payload, tz = haikubox_tz()) {
  dets <- payload$detections
  if (is.null(dets) || length(dets) == 0) {
    return(empty_detections_df())
  }

  rows <- lapply(dets, function(d) {
    dt_raw <- d$dt
    if (is.null(dt_raw) || !nzchar(dt_raw)) {
      return(NULL)
    }
    # Drop non-bird soundscape entries if present
    cn <- d$cn %||% NA_character_
    if (!is.na(cn) && identical(tolower(cn), "soundscape")) {
      return(NULL)
    }

    datetime_utc <- suppressWarnings(lubridate::ymd_hms(dt_raw, tz = "UTC", quiet = TRUE))
    if (is.na(datetime_utc)) {
      return(NULL)
    }
    datetime_local <- lubridate::with_tz(datetime_utc, tzone = tz)

    tibble::tibble(
      Species = as.character(cn),
      scientific_name = as.character(d$sn %||% NA_character_),
      datetime = as.POSIXct(datetime_local),
      Count = 1,
      Score = NA_real_,
      date_col = as.Date(datetime_local),
      time_col = as.numeric(format(datetime_local, "%H")),
      wav = as.character(d$wav %||% NA_character_),
      sp_code = as.character(d$spCode %||% NA_character_)
    )
  })

  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) {
    return(empty_detections_df())
  }

  out |>
    dplyr::filter(!is.na(Species), !is.na(datetime)) |>
    dplyr::arrange(dplyr::desc(datetime))
}

# Fetch API (or fall back to cache). Returns list(df, fetched_at, source, error).
# Set network=FALSE to seed from cache only (instant; used at UI startup).
refresh_live_detections <- function(
    serial = haikubox_serial(),
    hours = 24L,
    cache_path = haikubox_cache_path(),
    tz = haikubox_tz(),
    network = TRUE
) {
  error_msg <- NULL
  payload <- NULL
  source <- "api"
  fetched_at <- Sys.time()

  cached <- read_detections_cache(cache_path)

  if (!isTRUE(network)) {
    if (!is.null(cached) && !is.null(cached$payload)) {
      fetched_at <- suppressWarnings(
        lubridate::ymd_hms(cached$fetched_at, quiet = TRUE)
      )
      if (length(fetched_at) != 1 || is.na(fetched_at)) {
        fetched_at <- file.mtime(cache_path)
      }
      return(list(
        df = normalize_detections(cached$payload, tz = tz),
        fetched_at = fetched_at,
        source = "cache",
        error = NULL
      ))
    }
    return(list(
      df = empty_detections_df(),
      fetched_at = NA,
      source = "none",
      error = NULL
    ))
  }

  tryCatch(
    {
      payload <- fetch_detections_raw(serial = serial, hours = hours)
      write_detections_cache(payload, path = cache_path, serial = serial)
    },
    error = function(e) {
      error_msg <<- conditionMessage(e)
      if (!is.null(cached) && !is.null(cached$payload)) {
        payload <<- cached$payload
        source <<- "cache"
        fetched_at <<- suppressWarnings(
          lubridate::ymd_hms(cached$fetched_at, quiet = TRUE)
        )
        if (length(fetched_at) != 1 || is.na(fetched_at)) {
          fetched_at <<- file.mtime(cache_path)
        }
      } else {
        payload <<- NULL
      }
    }
  )

  if (is.null(payload)) {
    return(list(
      df = empty_detections_df(),
      fetched_at = NA,
      source = "none",
      error = error_msg %||% "No live detections available"
    ))
  }

  list(
    df = normalize_detections(payload, tz = tz),
    fetched_at = fetched_at,
    source = source,
    error = error_msg
  )
}

merge_excel_and_live <- function(excel_df, live_df) {
  excel_cols <- c(
    "Species", "scientific_name", "datetime", "Count", "Score", "date_col", "time_col"
  )

  excel_part <- excel_df |>
    dplyr::select(dplyr::any_of(excel_cols)) |>
    dplyr::mutate(source = "excel")

  live_part <- live_df |>
    dplyr::select(dplyr::any_of(excel_cols)) |>
    dplyr::mutate(source = "api")

  dplyr::bind_rows(excel_part, live_part) |>
    dplyr::distinct(Species, datetime, .keep_all = TRUE) |>
    dplyr::select(-source)
}
