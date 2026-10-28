#' Read Tobii Pro Lab data exports into gazeR format
#'
#' Reads the tab-separated data export from Tobii Pro Lab (Analyze > Data
#' Export, `.tsv`) and returns one row per eye-tracker sample, in the same
#' layout as [read_asc()].
#'
#' * **Participants:** `subject` is the `Participant name` column (or the
#'   file name if there is none). An export can hold several participants
#'   and recordings; each recording is kept apart (`recording` column).
#' * **Time:** from `Recording timestamp`. Its unit is read from the column
#'   name (`[μs]`, `[ms]` or `[s]`); without one, Pro Lab's default of
#'   microseconds is assumed. Converted to ms.
#' * **Pupil and gaze:** `Pupil diameter left/right [mm]` and the per-eye
#'   `Gaze point left/right X/Y` columns, falling back to the combined
#'   `Gaze point X/Y`. Samples whose `Validity left/right` is not "Valid"
#'   are `NA`. `eye` works as in [read_asc()]: `"average"` (mean of both
#'   eyes where both are valid), `"left"`, `"right"` or `"both"`. Pupil is in
#'   mm.
#' * **Events:** rows with an `Event` (and `Event value`) become messages,
#'   attached to the first sample at or after them. Rows from other sensors
#'   (mouse, keyboard) are dropped.
#' * **Trials:** by default a trial is each continuous stretch of samples
#'   showing one stimulus (`Presented Stimulus name`), and samples with no
#'   stimulus (or a calibration stimulus) are dropped. With `trial_start`, a new trial starts at each
#'   message beginning with one of those strings instead. `time` is ms from
#'   the start of the trial.
#' * **Blinks:** `blink` is 1 where Pro Lab classified the sample as a blink
#'   (`Eye movement type` "Blink"). Trackers that measure eye openness (e.g.
#'   Tobii Pro Spectrum and Fusion) give Pro Lab a direct blink signal: the
#'   lids closing and reopening, including samples where the pupil is still
#'   valid. Without it `blink` is 0; detect blinks from the signal, e.g. with
#'   [detect_blinks_noise()]. Eye openness (mm) is kept in
#'   `eye_openness_left`, `eye_openness_right` and `eye_openness` (filtered)
#'   when exported.
#'
#' Decimal commas (from some system locales) are handled.
#'
#' @param file path to a Pro Lab `.tsv` export (gzipped or not).
#' @param eye which eye to use: `"average"`, `"left"`, `"right"` or `"both"`.
#' @param trial_start optional character vector of messages that start a
#'   trial (matched against the start of each message). If `NULL`, trials
#'   follow `Presented Stimulus name`.
#' @param drop_calibration when trials follow the stimulus, drop stimuli
#'   whose name contains "calibration" (e.g. Pro Lab's "Eyetracker
#'   Calibration"). Default `TRUE`.
#' @return A tibble with `subject`, `recording`, `trial`, `time`, `pupil`,
#'   `x`, `y`, `blink`, `message`, `stimulus`, `tracker_time`,
#'   `sample_rate` (estimated from the mean time step within trials) and
#'   `eyes_recorded`.
#' @export
#' @examples
#' \dontrun{
#' d <- read_tobii_prolab("ProLab_export.tsv", eye = "average")
#' d <- read_tobii_prolab("ProLab_export.tsv", trial_start = "TrialStart")
#' }
read_tobii_prolab <- function(file, eye = c("average", "left", "right", "both"),
                              trial_start = NULL, drop_calibration = TRUE) {
  eye <- match.arg(eye)
  if (grepl("\\.gz$", file, ignore.case = TRUE)) {
    tmp <- tempfile(fileext = ".tsv")
    con_in <- gzfile(file, "rb"); writeLines(readLines(con_in, warn = FALSE), tmp); close(con_in)
    on.exit(unlink(tmp), add = TRUE)
    src <- tmp
  } else src <- file
  hdr <- names(data.table::fread(src, sep = "\t", nrows = 0, header = TRUE))
  # Find a column by its name without the unit, e.g. "Pupil diameter left [mm]"
  base <- trimws(sub("\\s*\\[.*\\]\\s*$", "", hdr))
  col <- function(name) { i <- which(tolower(base) == tolower(name)); if (length(i)) hdr[i[1]] else NA_character_ }
  ts_col <- col("Recording timestamp")
  if (is.na(ts_col)) stop("No 'Recording timestamp' column: is this a Tobii Pro Lab data export?", call. = FALSE)
  wanted <- c(ts = ts_col, participant = col("Participant name"), recording = col("Recording name"),
              sensor = col("Sensor"), event = col("Event"), event_value = col("Event value"),
              stimulus = col("Presented Stimulus name"),
              pupil_left = col("Pupil diameter left"), pupil_right = col("Pupil diameter right"),
              valid_left = col("Validity left"), valid_right = col("Validity right"),
              x_left = col("Gaze point left X"), y_left = col("Gaze point left Y"),
              x_right = col("Gaze point right X"), y_right = col("Gaze point right Y"),
              x = col("Gaze point X"), y = col("Gaze point Y"),
              movement = col("Eye movement type"),
              openness_left = col("Eye openness left"), openness_right = col("Eye openness right"),
              openness = col("Eye openness filtered"))
  wanted <- wanted[!is.na(wanted)]
  if (!any(c("pupil_left", "pupil_right") %in% names(wanted))) {
    stop("No 'Pupil diameter left/right' columns in the export. Include pupil diameter when exporting from Pro Lab.",
         call. = FALSE)
  }
  d <- data.table::fread(src, sep = "\t", select = unname(wanted), colClasses = "character",
                         na.strings = c("", "NA"), header = TRUE)
  data.table::setnames(d, unname(wanted), names(wanted))
  num <- function(v) suppressWarnings(as.numeric(sub(",", ".", v, fixed = TRUE)))
  for (v in intersect(c("ts", "pupil_left", "pupil_right", "x_left", "y_left", "x_right", "y_right", "x", "y",
                        "openness_left", "openness_right", "openness"), names(d))) {
    data.table::set(d, j = v, value = num(d[[v]]))
  }
  # Time in ms; Pro Lab's default unit is microseconds
  unit <- tolower(regmatches(ts_col, regexpr("\\[[^]]+\\]", ts_col)))
  scale <- if (!length(unit)) 1e-3 else switch(gsub("[][[:space:]]", "", unit),
                                               ms = 1, s = 1000, 1e-3)  # us / μs
  d$ts <- d$ts * scale
  if (!"participant" %in% names(d)) d$participant <- sub("\\.tsv(\\.gz)?$", "", basename(file), ignore.case = TRUE)
  if (!"recording" %in% names(d)) d$recording <- NA_character_

  # Messages from event rows
  is_event <- if ("event" %in% names(d)) !is.na(d$event) else rep(FALSE, nrow(d))
  msgs <- d[is_event]
  msgs$message <- if ("event_value" %in% names(msgs))
    ifelse(is.na(msgs$event_value), msgs$event, paste0(msgs$event, ": ", msgs$event_value)) else msgs$event
  # Eye-tracker samples
  samp <- d[!is_event]
  if ("sensor" %in% names(samp)) samp <- samp[is.na(samp$sensor) | samp$sensor == "Eye Tracker"]
  samp <- samp[!is.na(samp$ts)]

  valid <- function(side) {
    vcol <- paste0("valid_", side)
    if (vcol %in% names(samp)) !is.na(samp[[vcol]]) & samp[[vcol]] == "Valid" else rep(TRUE, nrow(samp))
  }
  side <- function(s) {
    ok <- valid(s)
    get <- function(v) if (v %in% names(samp)) ifelse(ok, samp[[v]], NA_real_) else rep(NA_real_, nrow(samp))
    gx <- if (paste0("x_", s) %in% names(samp)) get(paste0("x_", s)) else get("x")
    gy <- if (paste0("y_", s) %in% names(samp)) get(paste0("y_", s)) else get("y")
    p <- get(paste0("pupil_", s)); p[!is.na(p) & p <= 0] <- NA
    list(pupil = p, x = gx, y = gy)
  }
  L <- side("left"); R <- side("right")
  has_l <- any(!is.na(L$pupil)); has_r <- any(!is.na(R$pupil))
  out <- data.table::data.table(subject = samp$participant, recording = samp$recording, tracker_time = samp$ts,
                                stimulus = if ("stimulus" %in% names(samp)) samp$stimulus else NA_character_)
  both_ok <- !is.na(L$pupil) & !is.na(R$pupil)
  avg <- function(a, b) ifelse(both_ok, (a + b) / 2, NA_real_)
  pick <- if (eye %in% c("left", "right")) eye else if (has_l && has_r) "average" else if (has_l) "left" else "right"
  src_eye <- switch(pick, left = L, right = R, average = list(pupil = avg(L$pupil, R$pupil), x = avg(L$x, R$x), y = avg(L$y, R$y)))
  out$pupil <- src_eye$pupil; out$x <- src_eye$x; out$y <- src_eye$y
  if (eye == "both") {
    out$pupil_left <- L$pupil; out$pupil_right <- R$pupil
    out$x_left <- L$x; out$y_left <- L$y; out$x_right <- R$x; out$y_right <- R$y
  }
  # Pro Lab's own blink classification (Eye movement type "Blink", from eye
  # openness on trackers that measure it)
  out$blink <- if ("movement" %in% names(samp)) as.integer(!is.na(samp$movement) & samp$movement == "Blink") else 0L
  for (v in intersect(c("openness_left", "openness_right", "openness"), names(samp))) {
    out[[sub("^openness", "eye_openness", v)]] <- samp[[v]]
  }
  out$eyes_recorded <- if (has_l && has_r) "both" else if (has_l) "left" else "right"

  # Process each recording
  key <- paste(out$subject, out$recording, sep = "\r")
  mkey <- paste(msgs$participant, msgs$recording, sep = "\r")
  parts <- lapply(unique(key), function(k) {
    r <- out[key == k]
    data.table::setorder(r, tracker_time)
    r$message <- NA_character_
    m <- msgs[mkey == k]
    if (nrow(m)) {
      idx <- findInterval(m$ts, r$tracker_time, left.open = TRUE) + 1
      for (i in which(idx <= nrow(r))) {
        r$message[idx[i]] <- if (is.na(r$message[idx[i]])) m$message[i] else paste(r$message[idx[i]], m$message[i], sep = "; ")
      }
    }
    if (length(trial_start)) {
      pieces <- strsplit(ifelse(is.na(r$message), "", r$message), "; ", fixed = TRUE)
      starts <- which(vapply(pieces, function(p) any(startsWith(rep(p, each = length(trial_start)), trial_start)), logical(1)))
      if (!length(starts)) stop("No messages starting with ", paste(trial_start, collapse = " / "),
                                " for ", r$subject[1], call. = FALSE)
      r$trial <- findInterval(seq_len(nrow(r)), starts)
    } else {
      if (all(is.na(r$stimulus))) stop("No 'Presented Stimulus name' values for ", r$subject[1],
                                       ": give `trial_start` messages instead.", call. = FALSE)
      st <- ifelse(is.na(r$stimulus), "", r$stimulus)
      run <- cumsum(c(TRUE, st[-1] != st[-length(st)]))
      run[!nzchar(st) | (drop_calibration & grepl("calibration", st, ignore.case = TRUE))] <- 0L
      r$trial <- match(run, unique(run[run > 0]))
      r$trial[run == 0] <- 0L
    }
    r <- r[r$trial > 0]
    r$time <- r$tracker_time - stats::ave(r$tracker_time, r$trial, FUN = min)
    # Mean step within trials, ignoring gaps: Pro Lab can round timestamps to
    # whole ms, so the median step is wrong at e.g. 600 Hz (steps of 1 and 2 ms)
    dts <- unlist(lapply(split(r$tracker_time, r$trial), diff), use.names = FALSE)
    dts <- dts[is.finite(dts) & dts > 0]
    dts <- dts[dts < 5 * stats::median(dts)]
    r$sample_rate <- if (length(dts)) round(1000 / mean(dts)) else NA_real_
    r
  })
  res <- data.table::rbindlist(parts, fill = TRUE)
  front <- intersect(c("subject", "recording", "trial", "time", "pupil", "x", "y", "blink", "message",
                       "stimulus", "tracker_time"), names(res))
  data.table::setcolorder(res, c(front, setdiff(names(res), front)))
  tibble::as_tibble(res)
}

utils::globalVariables("tracker_time")
