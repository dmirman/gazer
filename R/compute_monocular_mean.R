#' Compute a Monocular Mean Pupil Value
#'
#' A "monocular mean" averages both eyes together. If data is available in just
#' one eye, use the available value as the mean. If both eyes are missing, the
#' result is `NA`.
#'
#' @param x1 Numeric vector for the left pupil.
#' @param x2 Numeric vector for the right pupil.
#'
#' @return A numeric vector of monocular mean pupil values.
#' @export
compute_monocular_mean <- function(x1, x2) {
  xm <- rowMeans(cbind(x1, x2), na.rm = TRUE)
  # Replace NaN (caused when both are NA) with NA
  ifelse(is.nan(xm), NA, xm)
}
