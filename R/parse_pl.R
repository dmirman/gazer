#' Read and align a single Pupil Labs subject, define trials, and reset time
#'
#' @description
#' Reads `gaze.csv`, `3d_eye_states.csv`, and `events.csv` from a subject folder,
#' attaches the nearest (non-recording) event to gaze rows (within a lag), defines
#' trial boundaries, removes blank trials, and resets `time` to 0 within each trial.
#'
#' @param subject_dir Path to a single subject directory containing Pupil Labs CSVs.
#' @param subject_id  Character ID for the subject. Defaults to folder name.
#' @param max_event_lag_ms Maximum absolute difference (ms) allowed when snapping an
#'   event to the nearest gaze row. Defaults to 2000.
#' @param start_mode Trial start rule: `"any"` = any non-empty, non-recording message;
#'   `"exact"` = only messages in `start_messages` (normalized) start a trial.
#' @param start_messages Character vector of trial-start messages when
#'   `start_mode = "exact"`. Case, stray whitespace, and hyphens/underscores are ignored.
#'
#' @return A tibble with columns: `subject`, `trial`, `time`, `x`, `y`, `pupil`, `blink`, `message`.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- read_pl_subject("path/to/subj001",
#'                       start_mode = "exact",
#'                       start_messages = c("trial-started-light", "start-dark"))
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#' @import readr
#' @import janitor

parse_pl <- function(subject_dir,
                            subject_id = basename(normalizePath(subject_dir)),
                            max_event_lag_ms = 50,
                            start_mode = c("any", "exact"),
                            start_messages = NULL) {
  stopifnot(dir.exists(subject_dir))
  start_mode <- match.arg(start_mode)
  if (start_mode == "exact" && (is.null(start_messages) || length(start_messages) == 0)) {
    stop("start_mode = 'exact' requires non-empty start_messages.")
  }

  ns_to_ms <- function(x) x / 1e6

  # --- read files -------------------------------------------------------------
  gaze   <- readr::read_csv(file.path(subject_dir, "gaze.csv"), show_col_types = FALSE) |> janitor::clean_names()
  eye3d  <- readr::read_csv(file.path(subject_dir, "3d_eye_states.csv"), show_col_types = FALSE) |> janitor::clean_names()
  events <- readr::read_csv(file.path(subject_dir, "events.csv"), show_col_types = FALSE) |> janitor::clean_names()

  # --- standardize gaze -------------------------------------------------------
  gaze <- gaze |>
    dplyr::transmute(
      t_ns = timestamp_ns,
      t_ms = ns_to_ms(timestamp_ns),
      x = gaze_x_px, y = gaze_y_px,
      fixation_id = fixation_id,
      blink_id = blink_id,
      blink = !is.na(blink_id) & blink_id != 0
    )

  eye3d <- eye3d |>
    dplyr::transmute(
      t_ns = timestamp_ns,
      t_ms = ns_to_ms(timestamp_ns),
      pupil_left_mm  = pupil_diameter_left_mm,
      pupil_right_mm = pupil_diameter_right_mm
    )

  gaze <- dplyr::left_join(gaze, eye3d, by = c("t_ns","t_ms"))

  # --- events: drop Pupil Labs recording markers ------------------------------
  rec_rx <- stringr::regex("^(recording\\.begin|recording\\.end)$", ignore_case = TRUE)

  events2 <- events |>
    dplyr::transmute(
      t_ns = timestamp_ns,
      t_ms = ns_to_ms(timestamp_ns),
      event_id = as.character(name)
    ) |>
    dplyr::filter(!stringr::str_detect(event_id, rec_rx)) |>
    dplyr::arrange(t_ms)

  # --- align ALL remaining events to nearest gaze (no de-dup by text) ---------
  ev_times   <- events2$t_ms
  gaze_times <- gaze$t_ms

  nearest_idx <- if (length(ev_times)) vapply(ev_times, function(et) which.min(abs(gaze_times - et)), integer(1)) else integer(0)
  diffs_ms    <- if (length(ev_times)) abs(gaze_times[nearest_idx] - ev_times) else numeric(0)
  keep        <- if (length(ev_times)) which(diffs_ms <= max_event_lag_ms) else integer(0)

  gaze$message <- NA_character_
  if (length(keep) > 0) {
    df_matches <- tibble::tibble(
      gaze_row = nearest_idx[keep],
      event_id = events2$event_id[keep],
      diff_ms  = diffs_ms[keep]
    ) |>
      dplyr::arrange(gaze_row, diff_ms) |>
      dplyr::distinct(gaze_row, .keep_all = TRUE)

    gaze$message[df_matches$gaze_row] <- df_matches$event_id
  }

  # --- base df ----------------------------------------------------------------
  df <- gaze |>
    dplyr::mutate(
      time    = t_ms - min(t_ms, na.rm = TRUE),
      pupil   = compute_monocular_mean(pupil_left_mm, pupil_right_mm),
      subject = subject_id
    ) |>
    dplyr::select(subject, time, x, y, pupil, blink, message) |>
    dplyr::arrange(subject, time)

  # helpers
  normalize_msg <- function(x) {
    x |>
      as.character() |>
      stringr::str_to_lower() |>
      stringr::str_replace_all("[^[:alnum:]]+", " ") |>
      stringr::str_squish()
  }
  is_valid_msg <- function(msg) {
    txt <- dplyr::coalesce(as.character(msg), "")
    nzchar(trimws(txt)) & !stringr::str_detect(txt, rec_rx)
  }

  # --- trial detection --------------------------------------------------------
  # normalize user triggers (for 'exact' mode) once
  start_set_norm <- if (start_mode == "exact") normalize_msg(start_messages) else character()

  df <- df |>
    dplyr::group_by(subject) |>
    dplyr::mutate(
      .msg_txt   = dplyr::coalesce(as.character(message), ""),
      .msg_norm  = normalize_msg(.msg_txt),
      .valid_msg = is_valid_msg(.msg_txt),
      .is_start  = dplyr::case_when(
        start_mode == "any"   ~ .valid_msg,
        start_mode == "exact" ~ .msg_norm %in% start_set_norm,
        TRUE ~ FALSE
      ),
      .new_trial = as.integer(.is_start & !dplyr::lag(.is_start, default = FALSE)),
      trial      = cumsum(.new_trial),
      trial      = dplyr::na_if(trial, 0L)
    ) |>
    tidyr::fill(trial, .direction = "down") |>
    dplyr::ungroup()

  # --- remove blank trials and pre-trial rows ---------------------------------
  df <- df |>
    dplyr::group_by(subject, trial) |>
    dplyr::mutate(.trial_has_valid = any(.valid_msg, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(trial) & .trial_has_valid)

  # --- reset time within each trial -------------------------------------------
  df <- df |>
    dplyr::group_by(subject, trial) |>
    dplyr::mutate(time = time - dplyr::first(time)) |>
    dplyr::ungroup() |>
    dplyr::select(subject, trial, time, x, y, pupil, blink, message) |>
    dplyr::arrange(subject, trial, time)

  df
}
