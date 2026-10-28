#' Read an EyeLink .asc file into gazeR format
#'
#' A fast, single-file reader for EyeLink `.asc` files (convert `.edf` with
#' SR Research's `edf2asc` first). The file is parsed in a few vectorised
#' passes, so even large recordings take seconds. Unlike [parse_asc()] and
#' [find_messages_asc()], it needs no folder-per-subject layout and writes
#' nothing to disk: it returns one long data frame, one row per sample, ready
#' for the pupil functions (`extend_blinks()`, `smooth_interpolate_pupil()`,
#' `baseline_correction_pupil_msg()`, ...), or the shiny gazeR app
#' (<https://github.com/jgeller112/shiny-gazeR>).
#'
#' What it does:
#' * **Trials** run from each `TRIALID` message to the matching
#'   `TRIAL_RESULT` message (both configurable). Samples outside a trial are
#'   dropped. `time` is in ms relative to the start of recording in that trial
#'   (the `!MODE RECORD` message, or `TRIALID` if there is none), matching
#'   [parse_asc()]. The raw tracker clock is kept in `tracker_time`.
#' * **Messages** are attached to the first sample at or after the message
#'   time, in a `message` column. Experiment Builder offsets
#'   (`MSG <time> <offset> <text>`) are applied (time − offset). Tracker
#'   housekeeping messages (`RECCFG`, `GAZE_COORDS`, `!V DRAW_LIST`, ...) are
#'   left out of the sample data but kept in the full message table.
#' * **Trial variables** written by Experiment Builder (`!V TRIAL_VAR name
#'   value`) become one column per variable, with types guessed by
#'   [utils::type.convert()]. These are usually your condition columns.
#' * **Blinks** come from the tracker's `SBLINK`/`EBLINK` events (`blink` is
#'   1 during a blink). Pupil values of 0 and missing gaze (`.`) become `NA`.
#' * **Binocular data** are handled with `eye`: `"average"`, `"left"`,
#'   `"right"`, or `"both"` (per-eye columns plus the average). With
#'   `"average"` and `"both"`, `pupil`, `x` and `y` are the **monocular
#'   mean** ([compute_monocular_mean()]), as EyeLink reports the average eye:
#'   the mean of both eyes, or the value of the one valid eye (`NA` only if
#'   neither is valid), and `blink` is 1 if **either** eye is in a blink.
#'
#' @param file path to an `.asc` file (may be gzipped).
#' @param eye which eye to use for binocular recordings: `"average"`,
#'   `"left"`, `"right"` or `"both"`. Ignored for monocular recordings.
#' @param subject subject label. Defaults to the file name without extension.
#' @param trial_start,trial_end message text (first word) marking the start
#'   and end of a trial.
#' @param keep_messages if `TRUE`, the full message table is attached as
#'   attribute `"messages"` (columns `trial`, `time`, `tracker_time`, `text`).
#'
#' @return A tibble with one row per sample:
#'   * `subject`: participant label (file name without extension, or `subject`).
#'   * `trial`: trial number (one per `TRIALID`).
#'   * `time`: ms from the start of recording in the trial (`!MODE RECORD`).
#'   * `pupil`: pupil size in tracker units (area or diameter, see
#'     `asc_info`); `NA` when missing. For `eye = "average"`/`"both"`, the
#'     monocular mean of the two eyes.
#'   * `x`, `y`: gaze position in screen pixels; `NA` when missing. Averaged
#'     like `pupil`.
#'   * `blink`: 1 during a blink, 0 otherwise. For `eye = "average"`/`"both"`,
#'     1 if either eye is in a blink.
#'   * `message`: event message(s) attached to this sample, otherwise `NA`.
#'   * `tracker_time`: the tracker's own clock (ms).
#'   * With `eye = "both"`: `pupil_left`, `pupil_right`, `x_left`, `y_left`,
#'     `x_right`, `y_right`, `blink_left`, `blink_right`, each for one eye.
#'   * One column per Experiment Builder trial variable.
#'
#'   Attribute `"asc_info"` holds the sampling rate (`hz`), recorded eyes,
#'   pupil type (area or diameter) and the eye used.
#' @importFrom data.table := .I .N .SD
#' @export
#' @examples
#' \dontrun{
#' d <- read_asc("data/PN001.asc", eye = "left")
#' attr(d, "asc_info")$hz
#' }
read_asc <- function(file,
                     eye = c("average", "left", "right", "both"),
                     subject = NULL,
                     trial_start = "TRIALID",
                     trial_end = "TRIAL_RESULT",
                     keep_messages = FALSE) {
  eye <- match.arg(eye)
  if (!file.exists(file)) stop("File not found: ", file, call. = FALSE)
  if (is.null(subject)) subject <- sub("\\.asc(\\.gz)?$", "", basename(file), ignore.case = TRUE)

  lines <- readLines(file, warn = FALSE)

  # ---- Recording info ------------------------------------------------------
  samples_line <- grep("^SAMPLES\\s", lines, value = TRUE)
  if (!length(samples_line)) stop("No SAMPLES lines found. Was the file exported with samples?", call. = FALSE)
  samples_line <- samples_line[1]
  has_left <- grepl("\\bLEFT\\b", samples_line)
  has_right <- grepl("\\bRIGHT\\b", samples_line)
  binocular <- has_left && has_right
  hz <- as.numeric(sub(".*\\bRATE\\s+([0-9.]+).*", "\\1", samples_line))
  pupil_line <- grep("^PUPIL\\s", lines, value = TRUE)
  pupil_type <- if (length(pupil_line)) tolower(trimws(sub("^PUPIL\\s+", "", pupil_line[1]))) else NA_character_
  recorded <- if (binocular) "both" else if (has_left) "left" else "right"
  if (!binocular) eye <- recorded

  # ---- Samples -------------------------------------------------------------
  is_sample <- grepl("^[0-9]", lines)
  n_eye_cols <- if (binocular) 7 else 4  # time, then x, y, pupil per eye
  samp <- data.table::fread(text = lines[is_sample], sep = "\t", header = FALSE,
                            na.strings = c(".", ""), fill = TRUE,
                            select = seq_len(n_eye_cols), colClasses = "character")
  samp <- samp[, lapply(.SD, function(v) suppressWarnings(as.numeric(v)))]
  data.table::setnames(samp, if (binocular) {
    c("tracker_time", "x_left", "y_left", "pupil_left", "x_right", "y_right", "pupil_right")
  } else {
    c("tracker_time", "x", "y", "pupil")
  })
  for (p in intersect(c("pupil", "pupil_left", "pupil_right"), names(samp))) {
    data.table::set(samp, which(samp[[p]] <= 0), p, NA_real_)
  }

  # ---- Blinks --------------------------------------------------------------
  blink_lines <- grep("^EBLINK", lines, value = TRUE)
  blink_in <- function(side) {
    b <- blink_lines[grepl(paste0("^EBLINK\\s+", side, "\\s"), blink_lines)]
    flag <- integer(nrow(samp))
    if (!length(b)) return(flag)
    parts <- strsplit(trimws(sub("^EBLINK\\s+[LR]\\s+", "", b)), "\\s+")
    starts <- as.numeric(vapply(parts, `[`, "", 1))
    ends <- as.numeric(vapply(parts, `[`, "", 2))
    # Sample is in a blink if the latest blink starting before it hasn't ended
    idx <- findInterval(samp$tracker_time, starts)
    inside <- idx > 0
    inside[inside] <- samp$tracker_time[inside] <= ends[idx[inside]]
    flag[inside] <- 1L
    flag
  }
  if (binocular) {
    samp$blink_left <- blink_in("L")
    samp$blink_right <- blink_in("R")
  } else {
    samp$blink <- blink_in(if (has_left) "L" else "R")
  }

  # Pick or combine eyes
  if (binocular) {
    # Monocular mean, as EyeLink reports the average eye: both eyes' mean, or
    # the one valid eye
    avg <- compute_monocular_mean
    if (eye %in% c("average", "both")) {
      samp$pupil <- avg(samp$pupil_left, samp$pupil_right)
      samp$x <- avg(samp$x_left, samp$x_right)
      samp$y <- avg(samp$y_left, samp$y_right)
      samp$blink <- as.integer(samp$blink_left | samp$blink_right)
    } else {
      side <- if (eye == "left") "_left" else "_right"
      samp$pupil <- samp[[paste0("pupil", side)]]
      samp$x <- samp[[paste0("x", side)]]
      samp$y <- samp[[paste0("y", side)]]
      samp$blink <- samp[[paste0("blink", side)]]
    }
    if (eye != "both") {
      samp[, c("x_left", "y_left", "pupil_left", "x_right", "y_right",
               "pupil_right", "blink_left", "blink_right") := NULL]
    }
  }

  # ---- Messages ------------------------------------------------------------
  msg_lines <- grep("^MSG\\s", lines, value = TRUE)
  msg_time <- as.numeric(sub("^MSG\\s+([0-9]+).*", "\\1", msg_lines))
  msg_text <- trimws(sub("^MSG\\s+[0-9]+\\s?", "", msg_lines))
  # Experiment Builder offset: "MSG <time> <offset> <text>" -> event at time - offset
  has_offset <- grepl("^-?[0-9]+\\s+\\S", msg_text)
  offset <- rep(0, length(msg_text))
  offset[has_offset] <- as.numeric(sub("^(-?[0-9]+)\\s.*", "\\1", msg_text[has_offset]))
  msg_text[has_offset] <- trimws(sub("^-?[0-9]+\\s+", "", msg_text[has_offset]))
  msg_time <- msg_time - offset
  msg_word <- sub("\\s.*", "", msg_text)

  # ---- Trials --------------------------------------------------------------
  t_start <- msg_time[msg_word == trial_start]
  t_end <- msg_time[msg_word == trial_end]
  if (!length(t_start)) {
    stop("No '", trial_start, "' messages found. Set `trial_start` to the message that starts each trial.",
         call. = FALSE)
  }
  # Pair each start with the first end after it (before the next start)
  next_start <- c(t_start[-1], Inf)
  t_end <- vapply(seq_along(t_start), function(i) {
    e <- t_end[t_end >= t_start[i] & t_end < next_start[i]]
    if (length(e)) e[1] else next_start[i] - 1
  }, numeric(1))
  n_trial <- length(t_start)

  trial_of <- function(time) {
    i <- findInterval(time, t_start)
    i[i == 0] <- NA
    ok <- !is.na(i)
    i[ok][time[ok] > t_end[i[ok]]] <- NA
    i
  }

  # Time zero: first !MODE RECORD inside the trial, else the trial start
  rec <- msg_time[grepl("^!MODE RECORD", msg_text)]
  rec_trial <- trial_of(rec)
  zero <- t_start
  first_rec <- tapply(rec[!is.na(rec_trial)], rec_trial[!is.na(rec_trial)], min)
  zero[as.integer(names(first_rec))] <- first_rec

  samp$trial <- trial_of(samp$tracker_time)
  samp <- samp[!is.na(samp$trial)]
  samp$time <- samp$tracker_time - zero[samp$trial]

  msg_trial <- trial_of(msg_time)
  messages <- tibble::tibble(trial = msg_trial, time = msg_time - zero[msg_trial],
                             tracker_time = msg_time, text = msg_text)

  # Trial variables (!V TRIAL_VAR name value) -> one column each
  is_var <- grepl("^!V\\s+TRIAL_VAR\\s", msg_text) & !is.na(msg_trial)
  trial_vars <- NULL
  if (any(is_var)) {
    rest <- sub("^!V\\s+TRIAL_VAR\\s+", "", msg_text[is_var])
    tv <- data.table::data.table(trial = msg_trial[is_var],
                                 name = sub("\\s.*", "", rest),
                                 value = trimws(sub("^\\S+\\s*", "", rest)))
    tv <- tv[, list(value = value[.N]), by = c("trial", "name")]  # last value wins
    trial_vars <- data.table::dcast(tv, trial ~ name, value.var = "value")
    trial_vars[] <- lapply(trial_vars, function(v) utils::type.convert(v, as.is = TRUE))
    # Syntactic names: LONG_JITTER[1] -> LONG_JITTER_1
    clean <- gsub("_+$", "", gsub("[^A-Za-z0-9_.]+", "_", names(trial_vars)))
    data.table::setnames(trial_vars, make.unique(clean, sep = "_"))
    # Avoid clobbering the canonical columns
    clash <- setdiff(intersect(names(trial_vars), c("subject", "time", "pupil", "x", "y", "blink", "message")), "trial")
    if (length(clash)) data.table::setnames(trial_vars, clash, paste0(clash, "_var"))
  }

  # Attach event messages to the first sample at/after them
  housekeeping <- c("RECCFG", "ELCLCFG", "GAZE_COORDS", "THRESHOLDS", "ELCL_WINDOW_SIZES",
                    "CAMERA_LENS_FOCAL_LENGTH", "PUPIL_DATA_TYPE", "ELCL_PROC", "ELCL_PCR_PARAM",
                    "!MODE", "!V", "!CAL", "!CMD", "VALIDATE", "TRACKER_TIME", "DISPLAY_COORDS",
                    trial_start, trial_end)
  ev <- !is.na(msg_trial) & !(msg_word %in% housekeeping)
  samp$message <- NA_character_
  if (any(ev)) {
    ev_trial <- msg_trial[ev]
    ev_time <- msg_time[ev]
    ev_text <- msg_text[ev]
    # Row of the first sample at or after each message, within its trial
    key <- data.table::data.table(trial = ev_trial, tracker_time = ev_time, text = ev_text)
    samp[, row_id := .I]
    hit <- samp[key, on = c("trial", "tracker_time"), roll = -Inf, list(row_id, text)]
    hit <- hit[!is.na(hit$row_id)]
    if (nrow(hit)) {
      # If two messages land on one sample, keep both
      hit <- hit[, list(text = paste(text, collapse = "; ")), by = "row_id"]
      samp$message[hit$row_id] <- hit$text
    }
    samp[, row_id := NULL]
  }

  # ---- Assemble ------------------------------------------------------------
  samp$subject <- subject
  front <- c("subject", "trial", "time", "pupil", "x", "y", "blink", "message", "tracker_time")
  data.table::setcolorder(samp, c(front, setdiff(names(samp), front)))
  if (!is.null(trial_vars)) samp <- merge(samp, trial_vars, by = "trial", all.x = TRUE, sort = FALSE)
  data.table::setcolorder(samp, c(front, setdiff(names(samp), front)))
  data.table::setorder(samp, trial, time)

  out <- tibble::as_tibble(samp)
  attr(out, "asc_info") <- list(file = file, hz = hz, recorded_eyes = recorded, eye_used = eye,
                                pupil_type = pupil_type, n_trials = n_trial)
  if (keep_messages) attr(out, "messages") <- messages
  out
}

# Column names used in data.table / dplyr expressions above
utils::globalVariables(c("value", "row_id", "text", "trial", "time"))
