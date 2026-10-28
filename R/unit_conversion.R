#' Convert pupil size to millimetres
#'
#' Eye trackers such as the EyeLink report pupil size in arbitrary units that
#' depend on the camera, lens and distance to the eye. To convert them to mm,
#' record an artificial pupil (a printed black disc) of known diameter at the
#' position of the participant's eye, with the same setup and pupil mode, and
#' note the value the tracker reports for it. Then:
#'
#' * **Diameter** data scale linearly: `mm = pupil * artificial_mm / artificial_value`.
#' * **Area** data grow with the square of the diameter, so the square root is
#'   taken first: `mm = artificial_mm * sqrt(pupil / artificial_value)`. The
#'   result is a diameter in mm.
#'
#' Instead of an artificial pupil you can give `scale`, a known factor with
#' `mm = scale * pupil` (diameter) or `mm = scale * sqrt(pupil)` (area).
#' The conversion is only valid for samples recorded at the same eye-to-camera
#' distance as the artificial pupil. Values of 0 or less (tracker loss) become
#' `NA`.
#'
#' @param pupil numeric vector of pupil sizes in tracker units.
#' @param pupil_type `"area"` or `"diameter"`: how the tracker measured the
#'   pupil. For EyeLink data read with [read_asc()], see
#'   `attr(x, "asc_info")$pupil_type`.
#' @param artificial_mm diameter of the artificial pupil in mm.
#' @param artificial_value value the tracker reported for the artificial
#'   pupil, in the same units and mode as `pupil`.
#' @param scale alternative to `artificial_mm` and `artificial_value`: the
#'   conversion factor (see above).
#' @return Pupil diameter in mm.
#' @export
#' @examples
#' # An 8 mm artificial pupil read as 5000 (area units)
#' pupil_to_mm(c(5000, 1250, 0), pupil_type = "area",
#'             artificial_mm = 8, artificial_value = 5000)
#' # Diameter mode: a 5 mm artificial pupil read as 100
#' pupil_to_mm(c(100, 80), pupil_type = "diameter", artificial_mm = 5, artificial_value = 100)
pupil_to_mm <- function(pupil, pupil_type = c("area", "diameter"),
                        artificial_mm = NULL, artificial_value = NULL, scale = NULL) {
  pupil_type <- match.arg(pupil_type)
  if (is.null(scale)) {
    if (is.null(artificial_mm) || is.null(artificial_value)) {
      stop("Give `artificial_mm` and `artificial_value` (an artificial pupil of known size), or `scale`.",
           call. = FALSE)
    }
    if (!is.numeric(artificial_mm) || !is.numeric(artificial_value) ||
        length(artificial_mm) != 1 || length(artificial_value) != 1 ||
        !(artificial_mm > 0) || !(artificial_value > 0)) {
      stop("`artificial_mm` and `artificial_value` must be single positive numbers.", call. = FALSE)
    }
    scale <- if (pupil_type == "diameter") artificial_mm / artificial_value else artificial_mm / sqrt(artificial_value)
  } else if (!is.numeric(scale) || length(scale) != 1 || !(scale > 0)) {
    stop("`scale` must be a single positive number.", call. = FALSE)
  }
  pupil[!is.na(pupil) & pupil <= 0] <- NA
  if (pupil_type == "diameter") scale * pupil else scale * sqrt(pupil)
}

#' Convert time stamps to milliseconds
#'
#' gazeR functions expect time in ms. Trackers record time in different
#' units: Tobii in microseconds, Pupil Labs in nanoseconds, some exports in
#' seconds or as sample numbers.
#'
#' @param time numeric vector of time stamps.
#' @param units units of `time`: `"s"`, `"ms"`, `"us"` (microseconds),
#'   `"ns"`, or `"samples"` (sample numbers, which needs `hz`).
#' @param hz sampling rate in Hz, for `units = "samples"`.
#' @return `time` in ms. Large clock values (e.g. ns since 1970) are kept as
#'   doubles, so subtract the first time stamp afterwards if you want time
#'   from the start of the recording.
#' @export
#' @examples
#' time_to_ms(c(1.5, 1.502), "s")
#' time_to_ms(c(1000000, 1004000), "us")
#' time_to_ms(0:4, "samples", hz = 500)
time_to_ms <- function(time, units = c("s", "ms", "us", "ns", "samples"), hz = NULL) {
  units <- match.arg(units)
  if (units == "samples") {
    if (is.null(hz) || !is.numeric(hz) || length(hz) != 1 || !(hz > 0)) {
      stop("`units = \"samples\"` needs `hz`, the sampling rate.", call. = FALSE)
    }
    return(time * 1000 / hz)
  }
  time * unit_scale(units)
}
