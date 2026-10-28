#' Correlation between the left and right pupil
#'
#' A check on binocular data before the eyes are averaged. The two pupils
#' respond together, so their traces should be highly correlated (typically
#' r > .9). A low correlation for a participant (or trial) means one eye was
#' tracked poorly, and averaging the eyes, or filling one eye's gaps with the
#' other ([compute_monocular_mean()]), would mix in that noise; consider
#' using the better eye instead.
#'
#' The Pearson correlation is computed over samples where both eyes are
#' valid, within each group.
#'
#' @param x data frame with per-eye pupil columns, e.g. from [read_asc()],
#'   [read_bids_eyetrack()] or [read_tobii_prolab()] with `eye = "both"`.
#' @param left,right left and right pupil columns.
#' @param by grouping: `"subject"` (default), `"trial"` (each trial of each
#'   participant), or a vector of column names.
#' @param summary if `TRUE`, return one row per group instead of adding a
#'   column to `x`.
#' @return With `summary = FALSE` (default), `x` with a `pupil_r` column (the
#'   group's correlation, repeated on each row). With `summary = TRUE`, a
#'   tibble with the grouping columns, `pupil_r` and `n_both` (samples with
#'   both eyes valid). Groups with fewer than 3 such samples, or no
#'   variation, get `NA`.
#' @export
#' @examples
#' x <- data.frame(subject = rep(c("s1", "s2"), each = 50), trial = 1,
#'                 pupil_left = c(5 + sin(1:50 / 5), 5 + rnorm(50)))
#' x$pupil_right <- c(5 + sin(1:50 / 5) + rnorm(50, 0, 0.05), 5 + rnorm(50))
#' pupil_cor(x, summary = TRUE)   # s1 high, s2 near 0
pupil_cor <- function(x, left = "pupil_left", right = "pupil_right", by = "subject",
                      summary = FALSE) {
  missing_cols <- setdiff(c(left, right), names(x))
  if (length(missing_cols)) {
    stop("No column(s) ", paste(missing_cols, collapse = ", "),
         ". Read binocular data with eye = \"both\" to keep each eye's pupil.", call. = FALSE)
  }
  by <- if (identical(by, "trial")) c("subject", "trial") else by
  missing_by <- setdiff(by, names(x))
  if (length(missing_by)) stop("`x` has no column(s): ", paste(missing_by, collapse = ", "), call. = FALSE)
  l <- x[[left]]; r <- x[[right]]
  l[!is.na(l) & l <= 0] <- NA
  r[!is.na(r) & r <= 0] <- NA
  g <- interaction(x[by], drop = TRUE, lex.order = TRUE)
  idx <- split(seq_len(nrow(x)), g)
  stats_g <- lapply(idx, function(i) {
    ok <- !is.na(l[i]) & !is.na(r[i])
    rr <- if (sum(ok) < 3 || stats::sd(l[i][ok]) == 0 || stats::sd(r[i][ok]) == 0) NA_real_
          else stats::cor(l[i][ok], r[i][ok])
    c(pupil_r = rr, n_both = sum(ok))
  })
  if (summary) {
    keys <- x[vapply(idx, `[`, integer(1), 1), by, drop = FALSE]
    out <- tibble::as_tibble(keys)
    out$pupil_r <- vapply(stats_g, `[[`, numeric(1), "pupil_r")
    out$n_both <- as.integer(vapply(stats_g, `[[`, numeric(1), "n_both"))
    return(out)
  }
  rr <- vapply(stats_g, `[[`, numeric(1), "pupil_r")
  x$pupil_r <- unname(rr[as.integer(g)])
  x
}
