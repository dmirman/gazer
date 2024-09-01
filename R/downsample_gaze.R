#' Downsample gaze data
#'
#' This function will combine gaze samples into time bins.
#' @param dataframe DataFrame containing gaze data.
#' @param bin.length Length of time bins.
#' @param timevar Column name representing time.
#' @param aggvars Vector of variable names to group by for aggregation.
#' @param type Type of data, either "pupil" for pupil data or "gaze" for gaze data.
#' @return Downsampled DataFrame.
#' @export
downsample_gaze <- function(dataframe, bin.length = 50, timevar = "time",
                            aggvars = c("subject", "condition", "target", "trial", "object", "timebins"),
                            type = "gaze") {

  dataframe <- dataframe %>%
    mutate(timebins = round(.data[[timevar]] / bin.length) * bin.length)

  if (type == "gaze") {
    if (length(aggvars) > 0) {
      downsample <- dataframe %>%
        group_by(across(all_of(aggvars))) %>%
        summarize(acc = unique(acc),
                  rt = unique(rt),
                  Fix = mean(Fix) > 0.5,
                  .groups = "drop")
    } else {
      downsample <- dataframe
    }
  } else if (type == "pupil") {
    if (length(aggvars) > 0) {
      downsample <- dataframe %>%
        group_by(across(all_of(aggvars))) %>%
        summarize(aggbaseline = mean(baselinecorrectedp, na.rm = TRUE),
                  .groups = "drop")
    } else {
      downsample <- dataframe
    }
  } else {
    stop("Invalid type specified. Choose either 'gaze' or 'pupil'.")
  }

  return(downsample)
}

