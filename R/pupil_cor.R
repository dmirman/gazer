#' Agreement between the left and right pupil
#'
#' A check on binocular data before the eyes are averaged. The two pupils
#' respond together, so their traces should agree closely (typically > .9).
#' A low value for a participant (or trial) means one eye was tracked
#' poorly, and averaging the eyes, or filling one eye's gaps with the other
#' ([compute_monocular_mean()]), would mix in that noise; consider using the
#' better eye instead.
#'
#' Both are computed over whole trials, over samples where both eyes are
#' valid, and returned side by side:
#'
#' * `pupil_icc`: intraclass correlation with the two eyes as raters and
#'   the samples as targets (McGraw & Wong, 1996). `icc_type = "agreement"`
#'   (ICC(A,1), default) is lowered by a systematic difference between the
#'   eyes (e.g. one pupil read 5% larger), which matters when one eye fills
#'   the other's gaps; `"consistency"` (ICC(C,1)) ignores it. The mean of
#'   both eyes is removed in each `within` unit (each trial; the same amount
#'   from each eye, so their difference is kept).
#' * `pupil_r`: repeated-measures correlation (rmcorr; Bakdash & Marusich,
#'   2017). Each eye's mean is removed in each `within` unit, and the eyes
#'   are correlated on what is left: do they rise and fall together within
#'   trials? A constant difference between the eyes does not lower it.
#'
#' Removing trial means keeps shifts in pupil size from one trial to the
#' next, which move both eyes, from inflating either value.
#'
#' `df` is that of rmcorr, samples minus units minus 1 (n - 2 for a plain
#' Pearson). Samples are autocorrelated, so they overstate the information
#' in the data; use the values as a descriptive check, not for a test.
#'
#' @param x data frame with per-eye pupil columns, e.g. from [read_asc()],
#'   [read_bids_eyetrack()] or [read_tobii_prolab()] with `eye = "both"`.
#' @param left,right left and right pupil columns.
#' @param by grouping, one value per group: `"subject"` (default), `"trial"`
#'   (each trial of each participant), a vector of column names, or `NULL`
#'   for one value for all of `x`.
#' @param within column(s) defining the unit whose mean is removed (within
#'   each `by` group): `"trial"` (default),
#'   or `NULL` for none (a plain ICC and Pearson correlation). Ignored when
#'   it is already part of `by`. With `by = NULL, within = "subject"`,
#'   `pupil_r` is the usual rmcorr across participants.
#' @param icc_type `"agreement"` (default) or `"consistency"`.
#' @param summary if `TRUE`, return one row per group instead of adding
#'   columns to `x`.
#' @return With `summary = FALSE` (default), `x` with `pupil_icc` and
#'   `pupil_r` columns (the group's values, repeated on each row). With
#'   `summary = TRUE`, a tibble with the grouping columns, `pupil_icc`,
#'   `pupil_r`, `n_both` (samples with both eyes valid), `n_units` (`within` units with
#'   such samples) and `df`. Groups with fewer than 3 such samples, or no
#'   variation, get `NA`.
#' @references Bakdash, J. Z., & Marusich, L. R. (2017). Repeated measures
#'   correlation. *Frontiers in Psychology, 8*, 456.
#'   \doi{10.3389/fpsyg.2017.00456}
#'
#'   McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#'   intraclass correlation coefficients. *Psychological Methods, 1*(1),
#'   30-46. \doi{10.1037/1082-989X.1.1.30}
#' @export
#' @examples
#' x <- data.frame(subject = rep(c("s1", "s2"), each = 50), trial = 1,
#'                 pupil_left = c(5 + sin(1:50 / 5), 5 + rnorm(50)))
#' x$pupil_right <- c(5 + sin(1:50 / 5) + rnorm(50, 0, 0.05), 5 + rnorm(50))
#' pupil_cor(x, summary = TRUE)   # s1 high, s2 near 0
#' pupil_cor(x, by = NULL, within = "subject", summary = TRUE)   # across participants
pupil_cor <- function(x, left = "pupil_left", right = "pupil_right", by = "subject",
                      within = "trial", icc_type = c("agreement", "consistency"),
                      summary = FALSE) {
  icc_type <- match.arg(icc_type)
  missing_cols <- setdiff(c(left, right), names(x))
  if (length(missing_cols)) {
    stop("No column(s) ", paste(missing_cols, collapse = ", "),
         ". Read binocular data with eye = \"both\" to keep each eye's pupil.", call. = FALSE)
  }
  by <- if (identical(by, "trial")) c("subject", "trial") else by
  within <- setdiff(within, by)
  missing_by <- setdiff(c(by, within), names(x))
  if (length(missing_by)) stop("`x` has no column(s): ", paste(missing_by, collapse = ", "), call. = FALSE)
  l <- x[[left]]; r <- x[[right]]
  l[!is.na(l) & l <= 0] <- NA
  r[!is.na(r) & r <= 0] <- NA
  ok <- !is.na(l) & !is.na(r)
  g <- if (length(by)) interaction(x[by], drop = TRUE, lex.order = TRUE) else factor(rep("all", nrow(x)))
  u <- if (length(within)) interaction(x[c(by, within)], drop = TRUE, lex.order = TRUE) else g

  idx <- split(seq_len(nrow(x)), g)
  stats_g <- lapply(idx, function(i) {
    i <- i[ok[i]]
    k <- if (length(within)) length(unique(u[i])) else 1L
    a <- l[i]; b <- r[i]
    if (length(i) < 3 || stats::sd(a) == 0 || stats::sd(b) == 0) {
      icc <- rr <- NA_real_
    } else {
      # ICC: remove the two-eye mean per unit (keeps the eyes' difference)
      m <- if (length(within)) stats::ave((a + b) / 2, u[i]) else 0
      icc <- icc_two(a - m, b - m, icc_type)
      # rmcorr: remove each eye's own mean per unit
      ac <- a - stats::ave(a, u[i]); bc <- b - stats::ave(b, u[i])
      den <- sqrt(sum(ac^2) * sum(bc^2))
      rr <- if (den == 0) NA_real_ else sum(ac * bc) / den
    }
    c(pupil_icc = icc, pupil_r = rr, n_both = length(i), n_units = k, df = length(i) - k - 1)
  })
  vals <- function(v) unname(vapply(stats_g, `[[`, numeric(1), v))
  if (summary) {
    out <- if (length(by)) tibble::as_tibble(x[vapply(idx, `[`, integer(1), 1), by, drop = FALSE])
           else tibble::tibble(.rows = 1)
    out$pupil_icc <- vals("pupil_icc")
    out$pupil_r <- vals("pupil_r")
    for (v in c("n_both", "n_units", "df")) out[[v]] <- as.integer(vals(v))
    return(out)
  }
  x$pupil_icc <- vals("pupil_icc")[as.integer(g)]
  x$pupil_r <- vals("pupil_r")[as.integer(g)]
  x
}

# Single-measure ICC for two raters (columns a, b) over n targets
# (McGraw & Wong, 1996): ICC(A,1) or ICC(C,1)
icc_two <- function(a, b, type = "agreement") {
  n <- length(a)
  m <- (a + b) / 2
  gm <- mean(m)
  msr <- 2 * sum((m - gm)^2) / (n - 1)
  msc <- n * ((mean(a) - gm)^2 + (mean(b) - gm)^2)
  mse <- sum((a - m - mean(a) + gm)^2 + (b - m - mean(b) + gm)^2) / (n - 1)
  if (type == "agreement") (msr - mse) / (msr + mse + 2 * (msc - mse) / n)
  else (msr - mse) / (msr + mse)
}
