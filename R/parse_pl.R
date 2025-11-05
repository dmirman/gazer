#' Read and align a single Pupil Labs subject, define trials, and reset time
#'
#' @description
#' Reads `gaze.csv`, `3d_eye_states.csv`, `events.csv`, and `gaze_surface.csv` from a subject folder,
#' attaches the nearest (non-recording) event to gaze rows (within a lag), defines
#' trial boundaries, removes blank trials, and resets `time` to 0 within each trial.
#' @param subject_dir Path to a single subject directory containing Pupil Labs CSVs.
#' @param subject_id  Character ID for the subject. Defaults to folder name.
#' @param aoi Logical; if TRUE, use surface (AOI) stream as well.
#' @param max_event_lag_ms Maximum absolute difference (ms) allowed when snapping an
#'   event to the nearest gaze row. Defaults to 100.
#' @param start_mode Trial start rule: `"any"` = any non-empty, non-recording message;
#'   `"exact"` = only messages in `start_messages` (normalized) start a trial.
#' @param start_messages Character vector of trial-start messages when
#'   `start_mode = "exact"`. Case/whitespace/punctuation ignored after normalization.
#'
#' @return Tibble: `subject, trial, time, x, y, pupil, blink, message, start_message_first`.
#' @export
parse_pl <- function(subject_dir,
                     subject_id = basename(normalizePath(subject_dir)),
                     aoi = FALSE,
                     max_event_lag_ms = 100,
                     start_mode = c("any", "exact"),
                     start_messages = NULL) {
  stopifnot(dir.exists(subject_dir))
  start_mode <- match.arg(start_mode)
  if (start_mode == "exact" && (is.null(start_messages) || length(start_messages) == 0)) {
    stop("start_mode = 'exact' requires non-empty start_messages.")
  }

  ns_to_ms <- function(x) x / 1e6

  # --- read files -------------------------------------------------------------
  gaze <- readr::read_csv(file.path(subject_dir, "gaze.csv"), show_col_types = FALSE) |>
    janitor::clean_names()
  eye3d <- readr::read_csv(file.path(subject_dir, "3d_eye_states.csv"), show_col_types = FALSE) |>
    janitor::clean_names()
  events <- readr::read_csv(file.path(subject_dir, "events.csv"), show_col_types = FALSE) |>
    janitor::clean_names()

  # helpers
  rec_rx <- stringr::regex("^(recording\\.begin|recording\\.end)$", ignore_case = TRUE)
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
  # --- standardize gaze & pupil ----------------------------------------------
  gaze <- gaze |>
    dplyr::mutate(
      t_ns = timestamp_ns,
      t_ms = ns_to_ms(timestamp_ns),
      x = gaze_x_px,
      y = gaze_y_px,
      blink = !is.na(blink_id) & blink_id != 0,
      .keep = "none"
    )

  eye3d <- eye3d |>
    dplyr::mutate(
      t_ns = timestamp_ns,
      t_ms = ns_to_ms(timestamp_ns),
      pupil_left_mm = pupil_diameter_left_mm,
      pupil_right_mm = pupil_diameter_right_mm,
      .keep = "none"
    )

  gaze <- dplyr::left_join(gaze, eye3d, by = c("t_ns", "t_ms")) |>
    dplyr::mutate(pupil = compute_monocular_mean(pupil_left_mm, pupil_right_mm))

  # --- events (drop recording markers) ----------------------------------------
  events2 <- events |>
    dplyr::mutate(
      t_ns = timestamp_ns,
      t_ms = ns_to_ms(timestamp_ns),
      event_id = as.character(name)
    ) |>
    dplyr::filter(!stringr::str_detect(event_id, rec_rx)) |>
    dplyr::arrange(t_ms)

  if (isTRUE(aoi)) {
    # =========================== AOI branch ===================================
    gaze_surface <- readr::read_csv(file.path(subject_dir, "gaze_surface.csv"), show_col_types = FALSE) |>
      janitor::clean_names()

    gaze_aoi <- gaze_surface |>
      dplyr::mutate(
        t_ns = timestamp_ns,
        t_ms = ns_to_ms(timestamp_ns),
        fixation_id = fixation_id
      )

    gaze_merge <- dplyr::inner_join(gaze, gaze_aoi, by = c("t_ns", "t_ms"))

    # --- align ALL remaining events to nearest gaze (no de-dup by text) ---------
    ev_times <- events2$t_ms
    gaze_times <- gaze_merge$t_ms

    nearest_idx <- if (length(ev_times)) vapply(ev_times, function(et) which.min(abs(gaze_times - et)), integer(1)) else integer(0)
    diffs_ms <- if (length(ev_times)) abs(gaze_times[nearest_idx] - ev_times) else numeric(0)
    keep <- if (length(ev_times)) which(diffs_ms <= max_event_lag_ms) else integer(0)

    gaze_merge$message <- NA_character_
    if (length(keep) > 0) {
      df_matches <- tibble::tibble(
        gaze_row = nearest_idx[keep],
        event_id = events2$event_id[keep],
        diff_ms  = diffs_ms[keep]
      ) |>
        dplyr::arrange(gaze_row, diff_ms) |>
        dplyr::distinct(gaze_row, .keep_all = TRUE)

      gaze_merge$message[df_matches$gaze_row] <- df_matches$event_id
    }

    # Back to tibble & build base df
    df <- gaze_merge |>
      dplyr::mutate(
        time = t_ms - first(t_ms),
        subject = subject_id
      ) |>
      dplyr::select(
        subject, time, x, y, pupil, blink, message,
        gaze_detected_on_surface,
        gaze_position_on_surface_x_normalized,
        gaze_position_on_surface_y_normalized,
        fixation_id
      ) |>
      dplyr::arrange(subject, time)

    start_set_norm <- if (start_mode == "exact") normalize_msg(start_messages) else character()

    df <- df |>
      dplyr::mutate(
        .msg_txt = dplyr::coalesce(as.character(message), ""),
        .msg_norm = normalize_msg(.msg_txt),
        .valid_msg = is_valid_msg(.msg_txt),
        .is_start = dplyr::case_when(
          start_mode == "any" ~ .valid_msg,
          start_mode == "exact" ~ .msg_norm %in% start_set_norm,
          TRUE ~ FALSE
        ),
        .new_trial = as.integer(.is_start & !dplyr::lag(.is_start, default = FALSE)),
        trial = as.integer(cumsum(.new_trial)),
        trial = dplyr::na_if(trial, 0L)
      ) |>
      tidyr::fill(trial, .direction = "down") |>
      dplyr::ungroup()

    # --- remove blank trials & pre-trial rows -----------------------------------
    # df <- df |>
    # dplyr::group_by(subject, trial) |>
    # dplyr::mutate(.trial_has_valid = any(.valid_msg, na.rm = TRUE)) |>
    # dplyr::ungroup() |>
    # dplyr::filter(!is.na(trial) & .trial_has_valid)

    # --- reset time within trial -------------------------------------------------
    df <- df |>
      dplyr::group_by(subject, trial) |>
      dplyr::mutate(time = time - dplyr::first(time)) |>
      dplyr::ungroup() |>
      select(
        subject, trial, time, gaze_detected_on_surface, gaze_position_on_surface_x_normalized,
        gaze_position_on_surface_y_normalized, pupil, blink, message, fixation_id
      )

    df
  } else {
    # ========================= non-AOI branch =================================
    # --- align ALL remaining events to nearest gaze (no de-dup by text) ---------
    ev_times <- events2$t_ms
    gaze_times <- gaze$t_ms

    nearest_idx <- if (length(ev_times)) vapply(ev_times, function(et) which.min(abs(gaze_times - et)), integer(1)) else integer(0)
    diffs_ms <- if (length(ev_times)) abs(gaze_times[nearest_idx] - ev_times) else numeric(0)
    keep <- if (length(ev_times)) which(diffs_ms <= max_event_lag_ms) else integer(0)

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
    # --- trial detection --------------------------------------------------------
    start_set_norm <- if (start_mode == "exact") normalize_msg(start_messages) else character()

    df <- gaze |>
      dplyr::mutate(
        subject=subject_id,
        time = t_ms - first(t_ms),
        .msg_txt = dplyr::coalesce(as.character(message), ""),
        .msg_norm = normalize_msg(.msg_txt),
        .valid_msg = is_valid_msg(.msg_txt),
        .is_start = dplyr::case_when(
          start_mode == "any" ~ .valid_msg,
          start_mode == "exact" ~ .msg_norm %in% start_set_norm,
          TRUE ~ FALSE
        ),
        .new_trial = as.integer(.is_start & !dplyr::lag(.is_start, default = FALSE)),
        trial = as.integer(cumsum(.new_trial)),
        trial = dplyr::na_if(trial, 0L)
      ) |>
      tidyr::fill(trial, .direction = "down") |>
      dplyr::ungroup()

    # --- reset time within trial -------------------------------------------------
    df <- df |>
      dplyr::group_by(subject, trial) |>
      dplyr::mutate(time = time - dplyr::first(time)) |>
      dplyr::ungroup() |>
      dplyr::select(subject, trial, time, x, y, pupil, blink, message) |>
      dplyr::arrange(subject, trial, time)

    df
  }
}
