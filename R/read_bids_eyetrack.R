#' Read BIDS eye-tracking data into gazeR format
#'
#' Reads eye-tracking recordings stored in the BIDS format for physiological
#' recordings (BEP020): `*_recording-<label>_physio.tsv.gz` files (no header)
#' with JSON sidecars describing their columns, plus optional
#' `*_physioevents.tsv.gz` files with messages and events. Returns one long
#' data frame, one row per sample, in the same layout as [read_asc()].
#'
#' * **Participants:** `subject` is built from the `sub-`, `ses-`, `task-` and
#'   `run-` entities of each file name (e.g. `sub-01_task-rest_run-1`).
#' * **Columns:** `timestamp`, `x_coordinate`, `y_coordinate` and `pupil_size`
#'   become `time`, `x`, `y` and `pupil`; other columns are kept. Timestamps
#'   are converted to ms from the `Units` of `timestamp` in the sidecar (`s`,
#'   `ms`, `us` or `ns`, so Pupil Labs and Tobii conversions work too), or,
#'   without units, from the step between samples and `SamplingFrequency`.
#' * **Eyes:** recordings of the left and right eye (e.g. `recording-eye1`
#'   and `recording-eye2`, identified by `RecordedEye` in the sidecar) are
#'   merged by timestamp. `eye` works as in [read_asc()]: `"average"`
#'   (monocular mean: both eyes' mean, or the one valid eye; `blink` is 1 if
#'   either eye blinks),
#'   `"left"`, `"right"`, or `"both"` (per-eye columns plus the average).
#' * **Events:** rows of `*_physioevents.tsv.gz` with a `message` are attached
#'   to the first sample at or after their onset; rows with
#'   `trial_type == "blink"` (or `blink == 1`) mark `blink` for their duration.
#' * **Trials:** a BIDS recording is continuous, so by default each run is one
#'   trial. With `trial_start`, a new trial starts at each message matching
#'   one of these strings, and `time` is ms from that message; samples before
#'   the first one are dropped.
#'
#' @param path a BIDS dataset folder (searched recursively), or a vector of
#'   `*_physio.tsv.gz` file paths.
#' @param eye which eye to use for binocular data: `"average"`, `"left"`,
#'   `"right"` or `"both"`.
#' @param trial_start optional character vector of messages that start a
#'   trial (matched against the start of each message).
#' @return A tibble with `subject`, `trial`, `time`, `pupil`, `x`, `y`,
#'   `blink`, `message`, `tracker_time`, `sample_rate`, `eyes_recorded` and
#'   any extra columns. Attribute `"bids_info"` has one row per subject with
#'   `sample_rate`, `eyes_recorded`, `manufacturer` and `pupil_units` from the
#'   sidecars, and `blink_events` (whether any blink events were found).
#' @export
#' @examples
#' \dontrun{
#' d <- read_bids_eyetrack("my_bids_dataset", eye = "left")
#' d <- read_bids_eyetrack("my_bids_dataset", trial_start = "TRIALID")
#' }
read_bids_eyetrack <- function(path, eye = c("average", "left", "right", "both"),
                               trial_start = NULL) {
  eye <- match.arg(eye)
  files <- if (length(path) == 1 && dir.exists(path)) {
    list.files(path, pattern = "_physio\\.tsv(\\.gz)?$", recursive = TRUE, full.names = TRUE)
  } else path
  files <- files[!grepl("_physioevents\\.", files)]
  if (!length(files)) stop("No *_physio.tsv.gz files found.", call. = FALSE)

  entity <- function(f, key) {
    m <- regmatches(basename(f), regexpr(paste0("(^|_)", key, "-[A-Za-z0-9]+"), basename(f)))
    if (length(m)) sub("^_", "", m) else NA_character_
  }
  run_id <- vapply(files, function(f) {
    parts <- stats::na.omit(c(entity(f, "sub"), entity(f, "ses"), entity(f, "task"), entity(f, "run")))
    paste(parts, collapse = "_")
  }, character(1))

  # fread() needs R.utils for .gz input, so decompress with base R instead.
  fread_tsv <- function(f, ...) {
    if (!grepl("\\.gz$", f)) return(data.table::fread(f, sep = "\t", ...))
    con <- gzfile(f, "rt")
    on.exit(close(con))
    data.table::fread(text = readLines(con), sep = "\t", ...)
  }

  read_one <- function(f) {
    side <- sub("\\.tsv(\\.gz)?$", ".json", f)
    if (!file.exists(side)) stop("No JSON sidecar for ", basename(f), call. = FALSE)
    meta <- jsonlite::read_json(side, simplifyVector = TRUE)
    cols <- meta$Columns
    if (is.null(cols)) stop(basename(side), " has no `Columns` field.", call. = FALSE)
    d <- fread_tsv(f, header = FALSE, col.names = cols, na.strings = c("n/a", "NA", ""))
    # Times in ms. Pupil Labs exports are in ns, Tobii in us; without Units,
    # guess from the median step and SamplingFrequency.
    units <- meta$timestamp$Units
    scale <- if (!is.null(units)) unit_scale(units) else {
      step <- stats::median(diff(d$timestamp[seq_len(min(nrow(d), 1000))]), na.rm = TRUE)
      hz <- meta$SamplingFrequency
      if (is.null(hz) || !is.finite(step) || step <= 0) 1 else {
        cand <- c(1000, 1, 1e-3, 1e-6)  # s, ms, us, ns
        cand[which.min(abs(log(step * cand * hz / 1000)))]
      }
    }
    d$timestamp <- d$timestamp * scale
    names(d)[names(d) == "x_coordinate"] <- "x"
    names(d)[names(d) == "y_coordinate"] <- "y"
    names(d)[names(d) == "pupil_size"] <- "pupil"
    if ("pupil" %in% names(d)) d$pupil[!is.na(d$pupil) & d$pupil <= 0] <- NA
    d$blink <- 0L

    # Events: messages and blinks
    ev_file <- sub("_physio\\.tsv(\\.gz)?$", "_physioevents.tsv.gz", f)
    if (!file.exists(ev_file)) ev_file <- sub("\\.gz$", "", ev_file)
    msgs <- NULL
    if (file.exists(ev_file)) {
      ev_meta <- jsonlite::read_json(sub("\\.tsv(\\.gz)?$", ".json", ev_file), simplifyVector = TRUE)
      ev <- fread_tsv(ev_file, header = FALSE, col.names = ev_meta$Columns, na.strings = c("n/a", "NA", ""))
      on_ts <- is.null(ev_meta$OnsetSource) || identical(ev_meta$OnsetSource, "timestamp")
      ev$onset_ms <- if (on_ts) ev$onset * scale else min(d$timestamp) + ev$onset * 1000
      dur_ms <- if ("duration" %in% names(ev)) suppressWarnings(as.numeric(ev$duration)) * 1000 else NA_real_
      is_blink <- rep(FALSE, nrow(ev))
      if ("trial_type" %in% names(ev)) is_blink <- is_blink | (!is.na(ev$trial_type) & ev$trial_type == "blink")
      if ("blink" %in% names(ev)) is_blink <- is_blink | (!is.na(ev$blink) & ev$blink == 1)
      for (i in which(is_blink & !is.na(dur_ms))) {
        d$blink[d$timestamp >= ev$onset_ms[i] & d$timestamp <= ev$onset_ms[i] + dur_ms[i]] <- 1L
      }
      if ("message" %in% names(ev)) {
        msgs <- ev[!is.na(ev$message) & nzchar(ev$message), c("onset_ms", "message")]
      }
    }
    str_or_na <- function(v) if (is.null(v) || !length(v)) NA_character_ else as.character(v[[1]])
    list(data = d, msgs = msgs, eye = tolower(if (!is.null(meta$RecordedEye)) meta$RecordedEye else "cyclopean"),
         hz = meta$SamplingFrequency, manufacturer = str_or_na(meta$Manufacturer),
         pupil_units = str_or_na(meta$pupil_size$Units), blink_events = any(d$blink == 1))
  }

  out <- lapply(unique(run_id), function(rid) {
    recs <- lapply(files[run_id == rid], read_one)
    eyes <- vapply(recs, `[[`, "", "eye")
    hz <- recs[[1]]$hz
    msgs <- data.table::rbindlist(lapply(recs, `[[`, "msgs"))
    if (nrow(msgs)) msgs <- unique(msgs)
    vcols <- c("x", "y", "pupil", "blink")
    if (all(c("left", "right") %in% eyes)) {
      L <- recs[[which(eyes == "left")[1]]]$data
      R <- recs[[which(eyes == "right")[1]]]$data
      keepL <- intersect(c("timestamp", vcols), names(L))
      keepR <- intersect(c("timestamp", vcols), names(R))
      L <- L[, keepL, with = FALSE]; R <- R[, keepR, with = FALSE]
      data.table::setnames(L, setdiff(keepL, "timestamp"), paste0(setdiff(keepL, "timestamp"), "_left"))
      data.table::setnames(R, setdiff(keepR, "timestamp"), paste0(setdiff(keepR, "timestamp"), "_right"))
      d <- merge(L, R, by = "timestamp", all = TRUE)
      avg <- compute_monocular_mean
      if (eye %in% c("average", "both")) {
        d$pupil <- avg(d$pupil_left, d$pupil_right)
        d$x <- avg(d$x_left, d$x_right); d$y <- avg(d$y_left, d$y_right)
        d$blink <- as.integer((!is.na(d$blink_left) & d$blink_left == 1) | (!is.na(d$blink_right) & d$blink_right == 1))
      } else {
        sfx <- paste0("_", eye)
        for (v in vcols) d[[v]] <- d[[paste0(v, sfx)]]
      }
      if (eye != "both") d[, (grep("_(left|right)$", names(d), value = TRUE)) := NULL]
      recorded <- "both"
    } else {
      d <- recs[[1]]$data
      recorded <- eyes[1]
    }
    d$blink[is.na(d$blink)] <- 0L
    data.table::setorder(d, timestamp)

    # Messages onto the first sample at or after them
    d$message <- NA_character_
    if (!is.null(msgs) && nrow(msgs)) {
      idx <- findInterval(msgs$onset_ms, d$timestamp, left.open = TRUE) + 1
      ok <- idx <= nrow(d)
      for (i in which(ok)) {
        d$message[idx[i]] <- if (is.na(d$message[idx[i]])) msgs$message[i] else paste(d$message[idx[i]], msgs$message[i], sep = "; ")
      }
    }

    # Trials
    if (length(trial_start)) {
      # Several messages on one sample are joined with "; ", so match each one
      pieces <- strsplit(ifelse(is.na(d$message), "", d$message), "; ", fixed = TRUE)
      starts <- which(vapply(pieces, function(p) any(startsWith(rep(p, each = length(trial_start)), trial_start)), logical(1)))
      if (!length(starts)) stop("No messages starting with ", paste(trial_start, collapse = " / "), " in ", rid, call. = FALSE)
      d$trial <- findInterval(seq_len(nrow(d)), starts)
      d <- d[d$trial > 0]
      t0 <- d[, list(t0 = timestamp[1]), by = "trial"]
      d$time <- d$timestamp - t0$t0[match(d$trial, t0$trial)]
    } else {
      d$trial <- 1L
      d$time <- d$timestamp - d$timestamp[1]
    }
    data.table::setnames(d, "timestamp", "tracker_time")
    d$subject <- rid
    d$sample_rate <- hz
    d$eyes_recorded <- recorded
    info <- data.frame(subject = rid, sample_rate = hz, eyes_recorded = recorded,
                       manufacturer = recs[[1]]$manufacturer, pupil_units = recs[[1]]$pupil_units,
                       blink_events = any(vapply(recs, `[[`, logical(1), "blink_events")))
    list(d = d, info = info)
  })
  info <- do.call(rbind, lapply(out, `[[`, "info"))
  out <- data.table::rbindlist(lapply(out, `[[`, "d"), fill = TRUE)
  front <- intersect(c("subject", "trial", "time", "pupil", "x", "y", "blink", "message", "tracker_time"), names(out))
  data.table::setcolorder(out, c(front, setdiff(names(out), front)))
  out <- tibble::as_tibble(out)
  attr(out, "bids_info") <- info
  out
}

# Multiplier from a BIDS time unit to ms
unit_scale <- function(units) {
  switch(tolower(units),
         s = , sec = , second = , seconds = 1000,
         us = , "\u00b5s" = , microsecond = , microseconds = 1e-3,
         ns = , nanosecond = , nanoseconds = 1e-6,
         1)
}

utils::globalVariables("timestamp")
