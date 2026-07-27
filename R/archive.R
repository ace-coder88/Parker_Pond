# Durable SQLite archive of Haikubox detections (hour-level, going forward).

archive_path <- function(data_dir = Sys.getenv("PARKER_DATA_DIR", unset = "data")) {
  file.path(data_dir, "archive", "detections.sqlite")
}

archive_db_connect <- function(path = archive_path()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  init_archive(con)
  con
}

init_archive <- function(con) {
  DBI::dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS detections (
      species TEXT NOT NULL,
      scientific_name TEXT,
      datetime TEXT NOT NULL,
      count REAL NOT NULL DEFAULT 1,
      score REAL,
      sp_code TEXT,
      ingested_at TEXT NOT NULL,
      UNIQUE(species, datetime)
    )
    "
  )
  DBI::dbExecute(
    con,
    "CREATE INDEX IF NOT EXISTS idx_detections_datetime ON detections(datetime)"
  )
  invisible
}

# Upsert normalized detection rows (from normalize_detections()).
# Stores datetime as ISO local (or whatever tz was used when normalizing).
upsert_detections <- function(df, path = archive_path()) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(attempted = 0L, inserted = 0L))
  }

  con <- archive_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  before <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM detections")$n[[1]]

  rows <- df |>
    dplyr::filter(!is.na(Species), !is.na(datetime)) |>
    dplyr::transmute(
      species = as.character(Species),
      scientific_name = as.character(scientific_name),
      # Store UTC so UNIQUE(species, datetime) is timezone-stable
      datetime = format(
        lubridate::with_tz(as.POSIXct(datetime), tzone = "UTC"),
        "%Y-%m-%dT%H:%M:%SZ"
      ),
      count = as.numeric(Count),
      score = as.numeric(Score),
      sp_code = as.character(sp_code),
      ingested_at = format(lubridate::with_tz(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%SZ")
    )

  # Normalize empty strings / NaN for SQLite
  rows$scientific_name[is.na(rows$scientific_name)] <- NA_character_
  rows$sp_code[is.na(rows$sp_code)] <- NA_character_
  rows$score[!is.finite(rows$score)] <- NA_real_

  DBI::dbWithTransaction(con, {
    for (i in seq_len(nrow(rows))) {
      DBI::dbExecute(
        con,
        "
        INSERT INTO detections
          (species, scientific_name, datetime, count, score, sp_code, ingested_at)
        VALUES
          (?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT(species, datetime) DO NOTHING
        ",
        params = list(
          rows$species[[i]],
          rows$scientific_name[[i]],
          rows$datetime[[i]],
          rows$count[[i]],
          rows$score[[i]],
          rows$sp_code[[i]],
          rows$ingested_at[[i]]
        )
      )
    }
  })

  after <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM detections")$n[[1]]
  list(attempted = nrow(rows), inserted = as.integer(after - before))
}

# Read archive into the same shape as load_birds() Excel output.
read_archive <- function(path = archive_path(), tz = haikubox_tz()) {
  empty <- tibble::tibble(
    Species = character(),
    scientific_name = character(),
    datetime = as.POSIXct(character()),
    Count = numeric(),
    Score = numeric(),
    date_col = as.Date(character()),
    time_col = numeric()
  )

  if (!file.exists(path)) {
    return(empty)
  }

  con <- tryCatch(
    DBI::dbConnect(RSQLite::SQLite(), path),
    error = function(e) NULL
  )
  if (is.null(con)) {
    return(empty)
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  has_table <- "detections" %in% DBI::dbListTables(con)
  if (!has_table) {
    return(empty)
  }

  raw <- DBI::dbGetQuery(
    con,
    "
    SELECT species, scientific_name, datetime, count, score, sp_code
    FROM detections
    ORDER BY datetime
    "
  )

  if (nrow(raw) == 0) {
    return(empty)
  }

  datetime_parsed <- suppressWarnings(
    lubridate::ymd_hms(raw$datetime, tz = "UTC", quiet = TRUE)
  )
  missing <- is.na(datetime_parsed)
  if (any(missing)) {
    datetime_parsed[missing] <- suppressWarnings(
      lubridate::ymd_hms(raw$datetime[missing], quiet = TRUE)
    )
  }
  datetime_local <- lubridate::with_tz(datetime_parsed, tzone = tz)

  tibble::tibble(
    Species = as.character(raw$species),
    scientific_name = as.character(raw$scientific_name),
    datetime = as.POSIXct(datetime_local),
    Count = as.numeric(raw$count),
    Score = as.numeric(raw$score),
    date_col = as.Date(datetime_local),
    time_col = as.numeric(format(datetime_local, "%H"))
  ) |>
    dplyr::filter(!is.na(datetime))
}

archive_summary <- function(path = archive_path()) {
  df <- read_archive(path)
  if (nrow(df) == 0) {
    return(list(n = 0L, date_min = NA, date_max = NA))
  }
  list(
    n = nrow(df),
    date_min = min(df$date_col),
    date_max = max(df$date_col)
  )
}
