#' Prepare data for pre-processing in Gazer
#'
#' This will make sure your data is in the right format for processing.
#' This package is designed to deal with data at it comes out of the eyetracker
#' in a long-form csv style format. Thus data input here would be a long
#' dataframe, wherein each row is a single frame collected by the eyetracker.
#'
#' @param data a raw, long form dataframe organised by subject, trial, and time.
#' @param subject column name indicating subject ID
#' @param trial column name indicating trial ID. This should be unique for participants
#' @param time column name indicating time column (should be numeric)
#' @param x gaze x coordinates
#' @param y gaze y coordinates
#' @param pupil name of pupil column
#'
#' @export
#'
#' @return A dataframe ready to use in gazer

make_gazer <- function(data, subject="subject", trial="trial", time="time", x="x", y="y", pupil=NULL) {

if (is.null(pupil)) {

  newdata <- data %>%
    select(!!quo_name("subject"):= subject,
           !!quo_name("trial"):= trial,
           !!quo_name("time"):= time,
           !!quo_name("x"):= x,
           !!quo_name("y"):= y,
           everything())
}
  else {
  newdata <- data %>%
  select(!!quo_name("subject"):= subject,
  !!quo_name("trial"):= trial,
  !!quo_name("time"):= time,
  !!quo_name("x"):= x,
  !!quo_name("y"):= y,
  !!quo_name("pupil"):= pupil,
  everything())
}
  return(newdata)
}

