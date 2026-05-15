#' This function takes a dataframe and applies a simple velocity algorithm to detect blinks in
#' the data based off velocity of pupil size changes. The algorithm has four
#' parameters that need to be adjusted based on the
#'particulars of the data: the amount of smoothing, the (negative) onset velocity threshold,
#'the (positive) reversal velocity threshold, and the temporal margin

#' @param df data frame.
#' @param neg_velocity_thresh threshold for onset of blinks neg number here we use default of -5
#' @param pos_velocity_thres threshold for offset of blinks neg number here we use default of 5
#' @param fillback extend blinks backward
#' @param fillforward extend blinks forward
#' @param hz recording frequency of ET
#' @export
#' @return data frame containing pupil values where blinks have been linear interpolated.
#'
blink_velocity=function(df, neg_velocity_thresh=-5, pos_velocity_thresh=5, fillback=10 , fillforward=10, hz=250){
  #First smooth the pupil signal. Here we use moving average.
  pupil_blink_algo <-  df %>%
    group_by(subject, trial) %>%
    mutate(smooth_pupil=moving_average_pupil(pupil, n=10)) %>%
  # Second, create velocity by taking diff from preceding sample
    mutate(velocity_pupil=c(diff(smooth_pupil)/diff(time), NA)) %>%
  # Thrid, set neg and pos threshold here we are using -3 and 2 (probably good idea to look at data)
    mutate(blinks_onset_offset=ifelse(velocity_pupil <= neg_velocity_thresh | velocity_pupil >= pos_velocity_thresh,
                                      1, 0)) %>%
  # Four turn pupil values 0 if blink detection coded as 1
    mutate(blinks_pupil=ifelse(blinks_onset_offset==1, pupil==NA, pupil))


  pup_extend<- pupil_blink_algo %>%
    group_by(subject, trial) %>%
    # five extend blinks
    mutate(extendpupil=extend_blinks(blinks_pupil, fillback=fillback, fillforward=fillforward, hz=250)) %>%
    #interpolate
    dplyr::mutate(interp = zoo::na.approx(extendpupil, na.rm = FALSE, rule=2))

  return(pup_extend)
}

