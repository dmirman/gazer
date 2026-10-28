#' Resample gaze and pupil data to a target sampling rate
#'
#' Brings each participant's data to `target_hz`, working within each
#' subject and trial:
#'
#' * **Downsampling** (recorded rate above `target_hz`): consecutive samples
#'   are averaged into bins of `1000 / target_hz` ms, which avoids aliasing.
#'   Pupil and gaze values are the bin mean, and are `NA` if any sample in the
#'   bin is missing (pupil values `<= 0` count as missing). Blink columns are
#'   1 if any sample in the bin was in a blink. Event columns keep every
#'   message in the bin (joined with `"; "`). Other columns take the bin's
#'   first value. `time` becomes the start of the bin.
#' * **Upsampling** (recorded rate below `target_hz`): new samples are added
#'   with [upsample_gaze()]. With `interpolate = TRUE`, their pupil and gaze
#'   values are linearly interpolated from the recorded samples either side,
#'   and left `NA` if either neighbour is missing (e.g. during a blink).
#' * **Same rate**: each trial is put on an evenly spaced grid, inserting any
#'   samples the tracker dropped as `NA` (never interpolated), ready for the
#'   usual blink and interpolation steps.
#'
#' Inserted samples are flagged in a `resampled` column. Any target rate
#' works. With a whole-millisecond step (e.g. 1000, 500, 250 Hz) samples are
#' placed on an integer-ms grid; otherwise (e.g. 600 Hz, 1.667 ms) each gap
#' between recorded samples gets `round(gap / step) - 1` evenly spaced new
#' samples, which stays aligned with the tracker's clock.
#'
#' @param x data frame with `subject`, `trial` and `time` (ms) columns.
#' @param target_hz target sampling rate in Hz.
#' @param hz recorded sampling rate: a single number, a named vector (names =
#'   subjects), or `NULL` (default) to estimate each subject's rate from the
#'   median time step.
#' @param pupil_cols,gaze_cols pupil and gaze columns. Columns not in `x` are
#'   ignored.
#' @param blink_cols blink-flag columns (1 = in a blink). Columns not in `x`
#'   are ignored.
#' @param event_cols event/message columns. Columns not in `x` are ignored.
#' @param interpolate when upsampling, interpolate the new samples' pupil and
#'   gaze values (default `TRUE`); if `FALSE` they are left `NA`.
#' @return `x` at `target_hz`, sorted by subject, trial and time, with a
#'   logical `resampled` column (`TRUE` for samples added by upsampling or
#'   gap filling).
#' @seealso [upsample_gaze()], [downsample_pupil()] and [downsample_gaze()]
#'   (which average into analysis time bins rather than resample).
#' @export
#' @examples
#' x <- data.frame(subject = "s1", trial = 1, time = 0:9,
#'                 pupil = c(1000:1003, NA, 1005:1009), message = NA)
#' x$message[3] <- "stim"
#' resample_gaze(x, target_hz = 500)   # 1000 -> 500 Hz
resample_gaze <- function(x, target_hz, hz = NULL,
                          pupil_cols = c("pupil", "pupil_left", "pupil_right"),
                          gaze_cols = c("x", "y", "x_left", "y_left", "x_right", "y_right"),
                          blink_cols = c("blink", "blink_left", "blink_right"),
                          event_cols = "message",
                          interpolate = TRUE) {
  if (!all(c("subject", "trial", "time") %in% names(x))) {
    stop("`x` needs subject, trial and time columns.", call. = FALSE)
  }
  if (!is.numeric(target_hz) || length(target_hz) != 1 || !is.finite(target_hz) || target_hz <= 0) {
    stop("`target_hz` must be a single positive number.", call. = FALSE)
  }
  pupil_cols <- intersect(pupil_cols, names(x))
  gaze_cols <- intersect(gaze_cols, names(x))
  blink_cols <- intersect(blink_cols, names(x))
  event_cols <- intersect(event_cols, names(x))
  value_cols <- c(pupil_cols, gaze_cols)

  subjects <- unique(as.character(x$subject))
  rate_of <- function(s, sub) {
    if (is.null(hz)) {
      dt <- unlist(lapply(split(sub$time, sub$trial), function(t) diff(sort(t))), use.names = FALSE)
      dt <- stats::median(dt[dt > 0], na.rm = TRUE)
      return(if (is.finite(dt)) round(1000 / dt) else NA_real_)
    }
    if (length(hz) == 1 && is.null(names(hz))) return(hz)
    unname(hz[s])
  }

  out <- lapply(subjects, function(s) {
    sub <- x[as.character(x$subject) == s, , drop = FALSE]
    rate <- rate_of(s, sub)
    if (!is.finite(rate)) stop("Could not determine the sampling rate for ", s, call. = FALSE)
    if (rate > target_hz) {
      res <- resample_down(sub, target_hz, pupil_cols, value_cols, blink_cols, event_cols)
      res$resampled <- FALSE
      return(res)
    }
    if ((1000 / target_hz) %% 1 != 0) {
      # Steps that aren't whole ms (e.g. 600 Hz = 1.667 ms): insert samples
      # between each pair of recorded ones
      up <- resample_up_between(sub, 1000 / target_hz, value_cols, event_cols)
      if (rate < target_hz && interpolate) {
        for (tr in unique(up$trial)) {
          idx <- which(up$trial == tr)
          for (v in value_cols) up[[v]][idx] <- fill_between(up[[v]][idx], up$time[idx], up$resampled[idx])
        }
      }
      return(up)
    }
    sub$time <- as.integer(round(sub$time))
    up <- upsample_gaze(sub, pupil_cols = pupil_cols, gaze_cols = gaze_cols,
                        target_hz = target_hz, upsample_pupil = TRUE, event_cols = event_cols)
    up$resampled <- up$up_sampled
    up$up_sampled <- NULL
    if (rate < target_hz && interpolate) {
      for (tr in unique(up$trial)) {
        idx <- which(up$trial == tr)
        for (v in value_cols) up[[v]][idx] <- fill_between(up[[v]][idx], up$time[idx], up$resampled[idx])
      }
    }
    up
  })
  out <- dplyr::bind_rows(out)
  out[order(out$subject, out$trial, out$time), , drop = FALSE]
}

# Average consecutive samples into 1000 / target_hz ms bins.
resample_down <- function(sub, target_hz, pupil_cols, value_cols, blink_cols, event_cols) {
  step <- 1000 / target_hz
  dt <- data.table::as.data.table(sub)
  for (v in pupil_cols) data.table::set(dt, which(dt[[v]] <= 0), v, NA_real_)
  dt[, .bin := floor(time / step) * step]
  keys <- c("subject", "trial", ".bin")
  other <- setdiff(names(dt), c(keys, "time", value_cols, blink_cols, event_cols))
  res <- if (length(value_cols)) dt[, lapply(.SD, mean), by = keys, .SDcols = value_cols]
         else unique(dt[, keys, with = FALSE])
  if (length(blink_cols)) res <- dt[, lapply(.SD, max, na.rm = TRUE), by = keys, .SDcols = blink_cols][res, on = keys]
  if (length(other)) res <- dt[, lapply(.SD, data.table::first), by = keys, .SDcols = other][res, on = keys]
  for (e in event_cols) {
    msg <- dt[!is.na(dt[[e]]), list(.msg = paste(get(e), collapse = "; ")), by = keys]
    res <- msg[res, on = keys]
    data.table::setnames(res, ".msg", e)
  }
  res[, time := .bin][, .bin := NULL]
  data.table::setcolorder(res, intersect(names(sub), names(res)))
  as.data.frame(res)
}

# Upsampling or gap filling to a step that isn't a whole number of ms: each
# gap between consecutive recorded samples gets round(gap / step) - 1 evenly
# spaced new samples (so there is no drift against the tracker's clock).
# New samples copy the other columns from the previous recorded sample;
# value and event columns are left NA.
resample_up_between <- function(sub, step, value_cols, event_cols) {
  sub <- sub[order(sub$trial, sub$time), , drop = FALSE]
  parts <- lapply(split(seq_len(nrow(sub)), sub$trial), function(i) {
    t <- sub$time[i]
    n_new <- if (length(t) > 1) pmax(0L, as.integer(round(diff(t) / step)) - 1L) else integer()
    src <- rep(i, c(n_new, 0L) + 1L)        # each recorded row, then its inserted copies
    k <- sequence(c(n_new, 0L) + 1L) - 1L   # 0 = recorded, 1.. = inserted after it
    gap <- c(diff(t), 0)[match(src, i)]
    out <- sub[src, , drop = FALSE]
    ins <- k > 0
    out$time[ins] <- out$time[ins] + k[ins] * gap[ins] / (n_new[match(src[ins], i)] + 1)
    for (v in c(value_cols, event_cols)) out[[v]][ins] <- NA
    out$resampled <- ins
    out
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

# Fill inserted samples by linear interpolation between the recorded samples
# either side; NA if either neighbour is missing.
fill_between <- function(v, time, inserted) {
  ins <- which(inserted)
  orig <- which(!inserted)
  ok <- !inserted & !is.na(v)
  if (!length(ins) || sum(ok) < 2) return(v)
  k <- findInterval(ins, orig)
  prev <- ifelse(k >= 1, orig[pmax(k, 1)], NA_integer_)
  nxt <- ifelse(k < length(orig), orig[pmin(k + 1, length(orig))], NA_integer_)
  est <- stats::approx(time[ok], v[ok], xout = time[ins])$y
  est[is.na(prev) | is.na(nxt) | is.na(v[prev]) | is.na(v[nxt])] <- NA
  v[ins] <- est
  v
}

utils::globalVariables(c(".bin", ".msg"))
