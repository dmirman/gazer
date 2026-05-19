#' Noise-based blink detection (Hershman, Henik & Cohen, 2018)
#'
#' Detects blinks in a per-sample pupil time series. Starts from the
#' tracker's missing-data sentinel and refines each missing-sample run's
#' onset and offset by walking outward on the smoothed first-difference
#' signal -- picking up the eyelid descent before the missing samples and
#' the recovery rise after. Adjacent blinks separated by less than
#' `gap_ms` are merged.
#'
#' Tracker-agnostic missing handling: by default treats both `NA` and `0`
#' as missing (Tobii uses `0`; SR Research / EyeLink exports often use
#' `NA`). Override via `missing`.
#'
#' Composes with dplyr exactly like `gazeR::extend_blinks()`:
#'
#' ```r
#' df |>
#'   group_by(subject, trial) |>
#'   mutate(blink = detect_blinks_noise(pupil, hz = 250))
#' ```
#'
#' If you want pupil set to `NA` inside blinks (gazeR-style), chain one
#' more line:
#'
#' ```r
#' ... |>
#'   mutate(pupil = ifelse(blink > 0, NA_real_, pupil))
#' ```
#'
#' Adapted to R with Dr William Paul Boyce (paul.boyce@ntu.edu.sg);
#' indexing bug in the original port -- matching on index instead of
#' time -- was diagnosed by Dr Jason Geller.
#'
#' @param pupil Numeric vector of pupil samples (one trial, one eye).
#'   Must be in time order at the rate given by `hz`.
#' @param hz Sampling rate in Hz.
#' @param smooth_ms Centered moving-average window applied before
#'   onset / offset refinement. Default 10 ms.
#' @param gap_ms Adjacent blinks closer than this (in ms) are merged.
#'   Default 100 ms.
#' @param missing Values in `pupil` that should be treated as missing
#'   samples. Default `c(0, NA)` -- works for Tobii (0) and EyeLink-style
#'   (NA) exports without configuration.
#'
#' @return An integer vector the same length as `pupil`: `0` outside
#'   detected blinks, `1, 2, ...` inside each blink. When used inside
#'   `dplyr::mutate()` under a `group_by()`, blink ids restart at 1 in
#'   each group.
#' @export
detect_blinks_noise <- function(pupil, hz,
                                smooth_ms = 10, gap_ms = 100,
                                missing = c(0, NA)) {

  stopifnot(is.numeric(pupil),
            length(hz) == 1L, is.finite(hz), hz > 0,
            smooth_ms > 0, gap_ms >= 0)

  pupil_in <- as.numeric(pupil)
  n <- length(pupil_in)
  sampling_ms <- 1000 / hz

  # Tracker-agnostic missing mask.
  sentinels_num <- missing[!is.na(missing)]
  is_miss <- if (any(is.na(missing))) is.na(pupil_in) else logical(n)
  if (length(sentinels_num)) {
    safe_in <- pupil_in
    safe_in[is.na(safe_in)] <- -Inf
    is_miss <- is_miss | (safe_in %in% sentinels_num)
  }

  make_blink_vec <- function(refined_onset, refined_offset) {
    bvec <- integer(n)
    for (k in seq_along(refined_onset)) {
      rng <- max(1L, refined_onset[k]):min(n, refined_offset[k])
      bvec[rng] <- k
    }
    bvec
  }

  if (n < 4L || !any(is_miss)) {
    return(make_blink_vec(integer(0), integer(0)))
  }

  # Algorithm runs on a 0-encoded copy regardless of input sentinel.
  pupil_alg <- pupil_in
  pupil_alg[is_miss] <- 0

  d <- diff(is_miss)
  onsets  <- -which(d == 1L)          # nonzero -> missing
  offsets <-  which(d == -1L) + 1L    # missing -> nonzero
  blinks <- c(onsets, offsets)
  if (length(blinks) == 0L) return(make_blink_vec(integer(0), integer(0)))
  blinks <- blinks[order(abs(blinks))]

  # Edge cases: recording starts and/or ends inside a missing run.
  if (blinks[1] > 0 && is_miss[1])              blinks <- c(-1L, blinks)
  if (utils::tail(blinks, 1) < 0 && is_miss[n]) blinks <- c(blinks, n)
  if (length(blinks) %% 2L != 0L) blinks <- blinks[-length(blinks)]
  if (length(blinks) == 0L) return(make_blink_vec(integer(0), integer(0)))

  # Centered moving average. Restore the first two samples from the raw
  # signal so diff() has no leading NAs.
  smooth_n <- max(1L, ceiling(smooth_ms / sampling_ms))
  smooth <- if (smooth_n >= 2L) {
    as.numeric(stats::filter(pupil_alg, rep(1 / smooth_n, smooth_n),
                             sides = 2L))
  } else {
    pupil_alg
  }
  smooth[1L] <- pupil_alg[1L]
  if (n >= 2L) smooth[2L] <- pupil_alg[2L]
  smooth[smooth == 0] <- NaN
  diff_smooth <- diff(smooth)

  n_pairs <- length(blinks) %/% 2L
  refined_onset  <- integer(n_pairs)
  refined_offset <- integer(n_pairs)
  prev_offset    <- -1L

  for (k in seq_len(n_pairs)) {
    onset_cand  <- blinks[2L * k - 1L]
    offset_cand <- blinks[2L * k]

    # --- onset refinement: last index < onset_cand with diff_smooth > 0
    # (last calm sample before the eyelid descent).
    upper <- max(2L, abs(onset_cand))
    before <- diff_smooth[2L:upper]
    pos_before <- which(before > 0)
    onset_idx <- if (length(pos_before) == 0L) {
      if (onset_cand == blinks[1L]) 0L else -abs(onset_cand)
    } else {
      pos_before[length(pos_before)]
    }
    if (onset_cand > 0L ||
        (abs(onset_cand) + 2L <= n && pupil_alg[abs(onset_cand) + 2L] > 0)) {
      onset_idx <- onset_idx + 2L
    }

    # --- offset refinement: first index > offset_cand with diff_smooth < 0
    # (end of the post-blink recovery rise).
    lo <- abs(offset_cand)
    after <- diff_smooth[lo:length(diff_smooth)]
    neg_after <- which(after < 0)
    offset_idx <- if (length(neg_after) == 0L) {
      n + 1L
    } else {
      lo + neg_after[1L]
    }

    if (sampling_ms * onset_idx > gap_ms &&
        sampling_ms * onset_idx - sampling_ms * prev_offset <= gap_ms) {
      onset_idx <- prev_offset
    }
    prev_offset <- offset_idx - 1L

    refined_onset[k]  <- onset_idx
    refined_offset[k] <- offset_idx - 1L
  }

  # Merge adjacent pairs created by gap-merge (offset[k] == onset[k+1]) into a
  # single window by extending blink k+1's onset back to blink k's onset.
  if (length(refined_onset) >= 2L) {
    keep <- rep(TRUE, length(refined_onset))
    for (k in seq_len(length(refined_onset) - 1L)) {
      if (keep[k] && refined_offset[k] == refined_onset[k + 1L]) {
        refined_onset[k + 1L] <- refined_onset[k]
        keep[k] <- FALSE
      }
    }
    refined_onset  <- refined_onset[keep]
    refined_offset <- refined_offset[keep]
  }

  make_blink_vec(refined_onset, refined_offset)
}
