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
      n <- length(pd)
      if (n < 2) return(pd)
      # filtfilt() pads with zeros, which pulls both ends of the signal towards
      # 0. Remove the mean and pad by odd reflection, then trim and add it back.
      pad <- min(n - 1, ceiling(3 * samplerate / filter_to_use))
      m <- mean(pd)
      y <- pd - m
      padded <- c(2 * y[1] - rev(y[2:(pad + 1)]), y, 2 * y[n] - rev(y[(n - pad):(n - 1)]))
      filtered_values <- filtfilt(myfilter, padded)[(pad + 1):(pad + n)] + m
    } else {
  warning("you have NAs in your data. \n Please check your data.")
      filtered_values <- NA
    }
    return(filtered_values)
  }
