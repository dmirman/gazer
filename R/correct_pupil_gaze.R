#' Correct pupil size for gaze position
#'
#' Removes the pupil foreshortening error: a video eye tracker sees the pupil
#' at an angle when the eye looks away from the camera, so recorded pupil size
#' changes with gaze position even when the true pupil does not (Hayes &
#' Petrov, 2016). Within each participant (default), pupil size is regressed
#' on gaze position, and the change that the fitted surface predicts between
#' each sample's gaze position and a reference position is subtracted (Gagl,
#' Hawelka & Hutzler, 2011; Brisson et al., 2013):
#'
#' `corrected = pupil - (f(x, y) - f(ref_x, ref_y))`
#'
#' so pupil size keeps its units and is unchanged at the reference position.
#'
#' The fit uses samples with valid pupil and gaze whose gaze lies inside the
#' central `1 - 2 * trim` of the participant's x and y range, so tracker-loss
#' codes (e.g. `1e8`) and stray samples do not drive it. Gaze is clamped to
#' that range when predicting, so the surface is never extrapolated. Samples
#' with missing gaze keep their pupil value.
#'
#' Use it after blinks and artifacts are removed and before interpolation.
#' Fitting on task data assumes that gaze position is unrelated to the effect
#' of interest; if it is not (e.g. stimuli on one side of the screen in one
#' condition), the correction can remove part of the effect. Fitting on a
#' separate calibration task (fixating points across the screen) avoids this:
#' fit there, and apply the coefficients to the task data with the same model.
#'
#' @param x data frame of samples.
#' @param pupil_col pupil column to correct.
#' @param x_col,y_col gaze position columns (any units, e.g. pixels or 0-1).
#' @param by columns that define one fit (default `"subject"`); `NULL` for a
#'   single fit across all samples.
#' @param model `"quadratic"` (default: x, y, x^2, y^2 and x*y) or `"linear"`
#'   (x and y).
#' @param reference gaze position where pupil size is left unchanged:
#'   `NULL` (default) for each group's median gaze position (the fixation
#'   point in most pupillometry tasks), or `c(x, y)`, e.g. the screen centre.
#' @param trim proportion trimmed from each end of the x and y range for the
#'   fit (default 0.01).
#' @param new_col name of the output column (default: overwrite `pupil_col`).
#' @return `x` with `new_col` added or replaced. The attribute `"gaze_fit"`
#'   holds one row per group: samples used (`n`), variance in pupil size
#'   explained by gaze position (`r_squared`), the reference position and
#'   the largest correction applied (`max_correction`).
#' @references
#' Brisson, J., Mainville, M., Mailloux, D., Beaulieu, C., Serres, J., &
#' Sirois, S. (2013). Pupil diameter measurement errors as a function of gaze
#' direction in corneal reflection eyetrackers. *Behavior Research Methods,
#' 45*, 1322-1331.
#'
#' Gagl, B., Hawelka, S., & Hutzler, F. (2011). Systematic influence of gaze
#' position on pupil size measurement: Analysis and correction. *Behavior
#' Research Methods, 43*, 1171-1181.
#'
#' Hayes, T. R., & Petrov, A. A. (2016). Mapping and correcting the influence
#' of gaze position on pupil size measurements. *Behavior Research Methods,
#' 48*, 510-527.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(subject = "s1", x = runif(500, 0, 1920), y = runif(500, 0, 1080))
#' d$pupil <- 3000 - 0.2 * (d$x - 960) + rnorm(500, 0, 20)
#' out <- correct_pupil_gaze(d, model = "linear")
#' attr(out, "gaze_fit")
correct_pupil_gaze <- function(x, pupil_col = "pupil", x_col = "x", y_col = "y", by = "subject",
                               model = c("quadratic", "linear"), reference = NULL, trim = 0.01,
                               new_col = pupil_col) {
  model <- match.arg(model)
  need <- c(pupil_col, x_col, y_col, by)
  missing_cols <- setdiff(need, names(x))
  if (length(missing_cols)) stop("`x` has no column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  if (!is.null(reference) && (length(reference) != 2 || !is.numeric(reference))) {
    stop("`reference` must be NULL or c(x, y).", call. = FALSE)
  }
  if (!is.numeric(trim) || length(trim) != 1 || trim < 0 || trim >= 0.5) {
    stop("`trim` must be a number from 0 to 0.5.", call. = FALSE)
  }
  terms <- function(gx, gy) {
    if (model == "linear") cbind(1, gx, gy) else cbind(1, gx, gy, gx^2, gy^2, gx * gy)
  }
  pupil <- as.numeric(x[[pupil_col]])
  gx <- as.numeric(x[[x_col]])
  gy <- as.numeric(x[[y_col]])
  out <- pupil
  g <- if (length(by)) interaction(x[by], drop = TRUE, lex.order = TRUE) else factor(rep("all", nrow(x)))
  fits <- lapply(split(seq_len(nrow(x)), g), function(i) {
    ok <- i[is.finite(pupil[i]) & is.finite(gx[i]) & is.finite(gy[i])]
    row <- data.frame(n = 0L, r_squared = NA_real_, ref_x = NA_real_, ref_y = NA_real_,
                      max_correction = NA_real_)
    if (length(by)) row <- cbind(x[i[1], by, drop = FALSE], row)
    if (length(ok) < 10) return(row)
    xr <- stats::quantile(gx[ok], c(trim, 1 - trim), names = FALSE)
    yr <- stats::quantile(gy[ok], c(trim, 1 - trim), names = FALSE)
    fit_i <- ok[gx[ok] >= xr[1] & gx[ok] <= xr[2] & gy[ok] >= yr[1] & gy[ok] <= yr[2]]
    X <- terms(gx[fit_i], gy[fit_i])
    if (length(fit_i) <= ncol(X)) return(row)
    beta <- qr.coef(qr(X), pupil[fit_i])
    beta[is.na(beta)] <- 0
    ref <- if (is.null(reference)) c(stats::median(gx[fit_i]), stats::median(gy[fit_i])) else reference
    clamp <- function(v, r) pmin(pmax(v, r[1]), r[2])
    has_gaze <- i[is.finite(gx[i]) & is.finite(gy[i])]
    delta <- drop(terms(clamp(gx[has_gaze], xr), clamp(gy[has_gaze], yr)) %*% beta) -
      drop(terms(clamp(ref[1], xr), clamp(ref[2], yr)) %*% beta)
    out[has_gaze] <<- pupil[has_gaze] - delta
    resid <- pupil[fit_i] - drop(X %*% beta)
    row$n <- length(fit_i)
    row$r_squared <- 1 - sum(resid^2) / sum((pupil[fit_i] - mean(pupil[fit_i]))^2)
    row$ref_x <- ref[1]
    row$ref_y <- ref[2]
    row$max_correction <- max(abs(delta[is.finite(pupil[has_gaze])]), 0)
    row
  })
  x[[new_col]] <- out
  fit <- do.call(rbind, fits)
  rownames(fit) <- NULL
  attr(x, "gaze_fit") <- fit
  x
}
