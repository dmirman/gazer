#' Merge paired Tobii Titta CSVs into a gazeR-ready, per-trial dataframe
#'
#' Loads gaze samples and event messages from two parallel folders, attaches
#' each message to the nearest gaze sample by `system_time_stamp` (as-of
#' merge), assigns trial numbers from onset / offset markers, and rebases
#' time to be trial-relative. The result is one row per gaze sample, with
#' event-marker columns set sparsely on the samples that absorbed an event.
#'
#'`merge_tobii_titta()`
#' reads the split CSVs that the current Titta build emits.
#'
#' ## Inputs
#' Files are paired by basename. A subject's gaze and msg file must share
#' the same filename minus extension, e.g. `gaze/sub01.csv` <->
#' `msg/sub01.csv`. The basename (without extension) becomes `subject`.
#'
#' ## Trial assignment
#' Trials are identified by message prefixes. Three modes are supported,
#' picked automatically from the data:
#' - **Bracketed** (default): `onset_<label>` starts a trial, `offset_<label>`
#'   ends it. Samples between an offset and the next onset are dropped.
#' - **Onset-only**: pass `offset_prefix = NULL`, or simply have no rows
#'   matching the offset prefix. Each onset runs until the next onset.
#' - **Custom prefixes**: override `onset_prefix` / `offset_prefix` for
#'   any other convention (`start_<n>`, `startfix_<n>`, etc.).
#'
#' The text following the onset prefix is kept as `event_label` and
#' broadcast across every sample in that trial -- it can be a stim filename,
#' a trial number, a condition tag, whatever your protocol uses.
#'
#' ## Nearest-sample merge
#' Because gaze is sampled on a fixed cadence (30 / 120 / 300 Hz on Tobii
#' trackers) but messages fire asynchronously, a message timestamp almost
#' never exactly equals a sample timestamp. Each message is therefore
#' merged onto the gaze sample whose timestamp is closest, found
#' vectorized via `findInterval()`. `match_offset_us`
#' (`matched sample ts - msg ts`) is exposed for diagnostics; on a 30 Hz
#' tracker this is at most ~16 ms (half the inter-sample gap), and well
#' below 1 ms on a 300 Hz tracker.
#'
#' ## Pupil / openness
#' Raw `left_pupil_diameter`, `right_pupil_diameter`,
#' `left_eye_openness_diameter`, and `right_eye_openness_diameter` (if
#' present) pass through unchanged. Average them downstream if you want a
#' single signal -- `gazeR::compute_monocular_mean()` or `rowMeans(..., na.rm = TRUE)`.
#'
#' ## Output time
#' `time` is in milliseconds relative to each trial's first sample (the one
#' the onset message merged onto), matching `gazeR`'s per-trial convention.
#'
#' @param gaze A directory of gaze CSVs, or a character vector of gaze file
#'   paths.
#' @param msg  A directory of message CSVs, or a character vector of msg
#'   file paths. Paired with `gaze` by basename.
#' @param onset_prefix Message prefix marking trial onsets. Required.
#'   Default `"onset_"`.
#' @param offset_prefix Message prefix marking trial offsets. Pass `NULL`
#'   for onset-only recordings. Default `"offset_"`.
#'
#' @return A tibble with one row per gaze sample inside a trial window.
#'   Leading columns: `subject`, `system_time_stamp`, `time`, `msg`,
#'   `message`, `event`, `event_label`, `match_offset_us`, `trial`. The
#'   raw Tobii sample columns (gaze x/y, pupil diameter, eye openness,
#'   validity flags, etc.) follow.
#' @export
#' @examples
#' \dontrun{
#' out <- merge_tobii_titta(
#'   gaze = "data/raw/gaze",
#'   msg  = "data/raw/msg"
#' )
#'
#' # Onset-only recording with custom prefix
#' out <- merge_tobii_titta(
#'   gaze = "data/raw/gaze",
#'   msg  = "data/raw/msg",
#'   onset_prefix  = "startfix_",
#'   offset_prefix = NULL
#' )
#' }
merge_tobii_titta <- function(gaze, msg,
                              onset_prefix  = "onset_",
                              offset_prefix = "offset_") {

  pid_from_path <- function(p) tools::file_path_sans_ext(basename(p))

  expand_input <- function(x, label) {
    if (length(x) == 1 && dir.exists(x)) {
      files <- list.files(x, pattern = "\\.csv$",
                          full.names = TRUE, recursive = FALSE)
      if (length(files) == 0) {
        stop("No .csv files found in ", label, " folder: ", x)
      }
      files
    } else {
      x
    }
  }
  gaze_files <- expand_input(gaze, "gaze")
  msg_files  <- expand_input(msg,  "msg")

  if (!requireNamespace("dplyr",   quietly = TRUE) ||
      !requireNamespace("readr",   quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE) ||
      !requireNamespace("purrr",   quietly = TRUE) ||
      !requireNamespace("tibble",  quietly = TRUE)) {
    stop("merge_tobii_titta() needs dplyr, readr, stringr, purrr, tibble.")
  }

  gaze_idx <- tibble::tibble(
    gaze = gaze_files,
    pid  = vapply(gaze_files, pid_from_path, character(1))
  )
  msg_idx <- tibble::tibble(
    msg = msg_files,
    pid = vapply(msg_files, pid_from_path, character(1))
  )
  pairs <- dplyr::inner_join(gaze_idx, msg_idx, by = "pid")

  if (nrow(pairs) == 0) {
    stop("No matching gaze / msg pairs. Files are paired by basename ",
         "(filename minus extension) -- each subject's gaze and msg file ",
         "must share the same filename. Sample gaze IDs: ",
         paste(head(unique(gaze_idx$pid), 3), collapse = ", "),
         " | sample msg IDs: ",
         paste(head(unique(msg_idx$pid), 3), collapse = ", "))
  }

  if (is.null(onset_prefix) || !nzchar(onset_prefix)) {
    stop("`onset_prefix` is required -- trials are anchored on onset markers.")
  }
  pat_on  <- paste0("^", onset_prefix)
  has_off <- !(is.null(offset_prefix) || !nzchar(offset_prefix))
  pat_off <- if (has_off) paste0("^", offset_prefix) else "$.^"  # never matches

  process_one <- function(gaze_path, msg_path, pid) {
    message("now processing: ", pid)

    gaze <- readr::read_csv(gaze_path, show_col_types = FALSE) |>
      dplyr::mutate(subject = pid) |>
      dplyr::arrange(system_time_stamp)

    msg <- readr::read_csv(msg_path, show_col_types = FALSE) |>
      dplyr::mutate(
        subject = pid,
        event = dplyr::case_when(
          stringr::str_detect(msg, pat_on)  ~ "onset",
          stringr::str_detect(msg, pat_off) ~ "offset",
          TRUE                              ~ NA_character_
        ),
        # Whatever follows the onset/offset prefix -- caller's payload, not
        # necessarily a stim. Could be a trial number, a stim filename,
        # a condition tag, etc.
        event_label = dplyr::case_when(
          event == "onset"  ~ stringr::str_remove(msg, pat_on),
          event == "offset" ~ stringr::str_remove(msg, pat_off),
          TRUE              ~ NA_character_
        )
      ) |>
      dplyr::arrange(system_time_stamp)

    if (!any(msg$event == "onset", na.rm = TRUE)) {
      stop("No onset markers found in msg file for ", pid,
           " (looking for prefix '", onset_prefix, "').")
    }

    # Nearest-sample as-of merge: for every msg timestamp, find the gaze
    # sample whose timestamp is closest. Vectorized via findInterval.
    sample_ts <- gaze$system_time_stamp
    n_samp    <- length(sample_ts)
    idx_lo    <- findInterval(msg$system_time_stamp, sample_ts)
    idx_lo[idx_lo == 0L] <- 1L
    idx_hi    <- pmin(idx_lo + 1L, n_samp)
    d_lo      <- abs(sample_ts[idx_lo] - msg$system_time_stamp)
    d_hi      <- abs(sample_ts[idx_hi] - msg$system_time_stamp)
    nearest   <- ifelse(d_hi < d_lo, idx_hi, idx_lo)

    msg_matched <- msg |>
      dplyr::mutate(matched_ts = sample_ts[nearest],
                    match_offset_us = sample_ts[nearest] - system_time_stamp)

    # If multiple messages land on the same nearest sample (rare; happens
    # only when two events fire within one inter-sample gap), keep the one
    # with the smallest |match_offset_us| so we don't duplicate gaze rows.
    msg_matched <- msg_matched |>
      dplyr::group_by(matched_ts) |>
      dplyr::slice_min(abs(match_offset_us), n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(matched_ts, msg, event, event_label, match_offset_us)

    # Attach msg fields to gaze rows by exact-match on the sample ts.
    combined <- gaze |>
      dplyr::left_join(msg_matched,
                       by = c("system_time_stamp" = "matched_ts"))

    # Trial assignment runs on the annotated gaze frame -- same cumsum
    # trick as before, but now over sample rows only. The onset/offset
    # markers live on the sample rows they were merged onto.
    combined <- combined |>
      dplyr::mutate(
        is_onset      = !is.na(event) & event == "onset",
        is_offset     = !is.na(event) & event == "offset",
        onset_run     = cumsum(is_onset),
        offset_before = dplyr::lag(cumsum(is_offset), default = 0),
        in_trial      = onset_run > offset_before,
        trial         = dplyr::if_else(in_trial, onset_run, NA_integer_)
      ) |>
      dplyr::filter(!is.na(trial)) |>
      dplyr::group_by(trial) |>
      dplyr::mutate(
        time        = (system_time_stamp - min(system_time_stamp)) / 1000,
        event_label = dplyr::coalesce(
          event_label, event_label[!is.na(event_label)][1]
        )
      ) |>
      dplyr::ungroup()

    combined
  }

  out <- purrr::pmap_dfr(
    list(pairs$gaze, pairs$msg, pairs$pid),
    process_one
  )

  # --- Drop the algorithm's internal trial-bookkeeping columns. These
  # are scratch fields the cumsum trick uses to assign trials; callers
  # never need them downstream.
  out$is_onset      <- NULL
  out$is_offset     <- NULL
  out$onset_run     <- NULL
  out$offset_before <- NULL
  out$in_trial      <- NULL

  # Reorder so the gazeR essentials lead, everything else trails. Same
  # pattern as `gazeR::make_gazer()` -- no project-specific column
  # filtering; whatever raw fields the tracker provided ride through.
  # The raw msg-file column was named `msg`; rename to `message` so it
  # matches the gazeR convention.
  dplyr::select(out, subject, trial, time,
                message = msg, dplyr::everything())
}
