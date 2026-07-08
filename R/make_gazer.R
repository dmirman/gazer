#' Prepare data for pre-processing in Gazer
#'
#' This will make sure your data is in the right format for processing.
#' This package is designed to deal with data at it comes out of the eyetracker
#' in a long-form csv style format. Thus data input here would be a long
#' dataframe, wherein each row is a single frame collected by the eyetracker.
#'
#' `x`, `y`, and `pupil` may each name a single column or several. A single
#' column is renamed to gazeR's canonical name (`x`, `y`, or `pupil`); when you
#' pass more than one column (e.g. left and right eye) the original column names
#' are kept as-is. Either way the selected columns are moved to the front of the
#' data frame and every other column is kept, in its original order, after them.
#'
#' @param data a raw, long form dataframe organised by subject, trial, and time.
#' @param subject column name indicating subject ID
#' @param trial column name indicating trial ID. This should be unique for participants
#' @param time column name indicating time column (should be numeric)
#' @param x gaze x column(s). One name is renamed to `x`; several (e.g.
#'   `c("x_left", "x_right")`) keep their names. Use `NULL` if there is no gaze.
#' @param y gaze y column(s); see `x`.
#' @param pupil pupil column(s). One name is renamed to `pupil`; several (e.g.
#'   `c("pupil_left", "pupil_right")`) keep their names. `NULL` (default) if none.
#'
#' @export
#'
#' @return A dataframe ready to use in gazer

make_gazer <- function(data,
                       subject = "subject",
                       trial = "trial",
                       time = "time",
                       x = "x",
                       y = "y",
                       pupil = NULL) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  # canonical name -> source column(s). A single column is renamed to the
  # canonical name; multiple columns keep their own names. NULLs are skipped.
  spec <- list(subject = subject, trial = trial, time = time,
               x = x, y = y, pupil = pupil)

  mapping <- character(0)
  for (canon in names(spec)) {
    cols <- spec[[canon]]
    if (is.null(cols)) next
    new_names <- if (length(cols) == 1) canon else cols
    mapping <- c(mapping, stats::setNames(cols, new_names))
  }

  missing <- setdiff(mapping, names(data))
  if (length(missing)) {
    stop("Column(s) not found in `data`: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  data %>%
    dplyr::select(dplyr::all_of(mapping), dplyr::everything())
}
