#' This function performs low pass filtering

#' @param pd pupil column.
#' @param order order butter filter
#' @param filter_to_use hz to low pass filter
#' @param samplerate sampling rate of the eye tracker in Hz
#' @export
#' @importFrom signal butter filtfilt
#' @return vector containing low pass filtered pupil data

lowpass_filter <- function(pd, order, filter_to_use, samplerate=NA){
  message("low pass filtering")
  num_NAs <- sum(is.na(pd))
    if (num_NAs < 1){
      myfilter = butter(order,filter_to_use/(samplerate/2), type = 'low', plane='z')
      filtered_values <- filtfilt(myfilter, pd)
    } else {
  warning("you have NAs in your data. \n Please check your data.")
      filtered_values <- NA
    }
    return(filtered_values)
  }
