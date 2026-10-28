#' Z-score pupil size
#'
#' Standardises pupil size within each participant (default) or each trial:
#' the group's mean is subtracted and the result divided by its standard
#' deviation, both computed over the group's non-missing samples. Missing
#' samples stay missing. Z-scoring puts participants with different pupil
#' sizes or tracker units on a common scale.
#'
#' Use it as the last step, to re-scale the baseline-corrected pupil (e.g.
#' `pupil_col = "baselinecorrectedp"` from [baseline_correction_pupil()] or
#' [baseline_correction_pupil_msg()]): read data, de-blink and extend blinks,
#' smooth and interpolate, baseline-correct, then re-scale. Results are then
#' in standard deviations of each participant's (or trial's) signal.
#'
#' @param x data frame of samples.
#' @param pupil_col pupil column to standardise.
#' @param by grouping: `"subject"` (each participant's samples across all
#'   their trials, the default) or `"trial"` (each trial of each
#'   participant); or a vector of column names.
#' @param new_col name of the output column (default `"pupil_z"`; set it to
#'   `pupil_col` to overwrite).
#' @return `x` with `new_col` added. Groups with fewer than two non-missing
#'   samples, or no variation, get `NA`.
#' @export
#' @examples
#' x <- data.frame(subject = rep(c("s1", "s2"), each = 4), trial = 1,
#'                 pupil = c(1000, 1100, NA, 1200, 3000, 3300, 3600, 3900))
#' zscore_pupil(x)
#' zscore_pupil(x, by = "trial")
zscore_pupil <- function(x, pupil_col = "pupil", by = "subject", new_col = "pupil_z") {
  if (!pupil_col %in% names(x)) stop("No column `", pupil_col, "` in `x`.", call. = FALSE)
  by <- if (identical(by, "trial")) c("subject", "trial") else by
  missing_by <- setdiff(by, names(x))
  if (length(missing_by)) stop("`x` has no column(s): ", paste(missing_by, collapse = ", "), call. = FALSE)
  v <- x[[pupil_col]]
  g <- interaction(x[by], drop = TRUE, lex.order = TRUE)
  m <- stats::ave(v, g, FUN = function(z) mean(z, na.rm = TRUE))
  s <- stats::ave(v, g, FUN = function(z) if (sum(!is.na(z)) < 2) NA_real_ else stats::sd(z, na.rm = TRUE))
  s[!is.na(s) & s == 0] <- NA_real_
  x[[new_col]] <- (v - m) / s
  x
}
