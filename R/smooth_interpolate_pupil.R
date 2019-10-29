#' Smooths the pupil trace before linear or cubic interpolation.
#' @param datafile data frame.
#' @param pupil name of pupil column
#' @param extendpupil name of pupil col that was extended
#' @param extendblinks blinks already extended  in data frame
#' @param method.first whether you want to smooth/interpolate or interpolate/smooth
#' @param type type of interpolation (linear or cubic)
#' @param maxgap max number of NAs to interpolate.
#' @param hz recording frequency of ET
#' @param n moving average window
#' @export
#' @import zoo
#'
#' @return df containing interpolated data
#'
smooth_interpolate_pupil<-function(datafile, pupil="pupil", extendpupil="extendpupil", extendblinks=FALSE, method.first="smooth", maxgap=Inf, type=NULL, hz=NA, n=NA) {
  #supports linear and cublic-spline interpolation
  if (maxgap!=Inf){
    maxgap <- round(maxgap/(1000/hz))
  }

  if (extendblinks==FALSE & type=="linear" & method.first=="smooth") {

    message("Turning pupil size with blinks to NA")

    blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation

    message("Smoothing the pupil trace with moving average")

    smooth_pupil <- as.data.frame(blinks_na) %>%
      mutate(movingavgpup =moving_average_pupil(pup, n = n))

    message("Performing linear interpolation")

    pupil_interp <- smooth_pupil %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(interp = zoo::na.approx(movingavgpup, rule=2)) %>%
      dplyr::ungroup()
    return(pupil_interp)
  }

  if (extendblinks==TRUE & type=="linear" & method.first=="smooth") {

    message("smoothing the pupil trace with moving average")

    smooth_pupil <- as.data.frame(datafile) %>%
      mutate(movingavgpup = moving_average_pupil(extendpupil, n = n))


    message("Performing linear interpolation")
    pupil_interp <- smooth_pupil %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(interp = zoo::na.approx(movingavgpup, na.rm = FALSE, rule=2)) %>%
      dplyr::ungroup()
    return(pupil_interp)

  }

  if (extendblinks==FALSE & type=="cubic" & method.first=="smooth") {
    message("Turning pupil size with blinks to NA")
    blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation

    message("Smoothing the pupil trace with moving average")

    smooth_pupil <- as.data.frame(blinks_na) %>%
      mutate(movingavgpup =moving_average_pupil(pup, n = n))

    message("Performing cubic interpolation")
    pupil_interp <- smooth_pupil %>% dplyr::group_by(subject, trial) %>%
      dplyr::mutate(index = ifelse(is.na(pup), NA, dplyr::row_number()),index=zoo::na.approx(index, na.rm=FALSE, rule=2),
                    interp = zoo::na.spline(movingavgpup, na.rm=FALSE, x=index, maxgap=maxgap)) %>%
      dplyr::ungroup()

    return(pupil_interp)
  }

  if (extendblinks==TRUE & type=="cubic" & method.first=="smooth") {
    
    message("Smoothing the pupil trace with moving average")
    
    smooth_pupil <- as.data.frame(pupil_interp) %>%
      mutate(movingavgpup = moving_average_pupil(extendpupil, n = n))
    
    message("Performing cubic interpolation")
    pupil_interp <- smooth_pupil  %>% dplyr::group_by(subject, trial) %>%
      dplyr::mutate(
        index = ifelse(is.na(extendpupil), NA, dplyr::row_number()),
        index= zoo::na.approx(index, na.rm=FALSE),
        interp = zoo::na.spline(movingavgpup, na.rm=FALSE, x=index, maxgap=maxgap)) %>%
      dplyr::ungroup()

    return(pupil_interp)
  }
  
  if (extendblinks==FALSE & type=="linear" & method.first=="interp") {
    
    message("Turning pupil size with blinks to NA")
    
    blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation
    
    message("Performing linear interpolation")
    
    pupil_interp <- blinks_na %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(interp = zoo::na.approx(movingavgpup, rule=2)) %>%
      dplyr::ungroup()
    
    message("Smoothing the pupil trace with moving average")
    
    smooth_pupil <- as.data.frame(pupil_interp) %>%
      mutate(movingavgpup = moving_average_pupil(interp, n = n))
    
    return(smooth_pupil)
  }

  
  if (extendblinks==TRUE & type=="linear" & method.first=="interp") {
    
    
    message("Performing linear interpolation")
    
    pupil_interp <- datafile %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(interp = zoo::na.approx(extendpupil, na.rm = FALSE, rule=2)) %>%
      dplyr::ungroup()
 
    message("Smoothing the pupil trace with moving average")
    
    smooth_pupil <- as.data.frame(pupil_interp) %>%
      mutate(movingavgpup = moving_average_pupil(interp, n = n))
    
    return(smooth_pupil)
  }
    
  
  if (extendblinks==FALSE & type=="cubic" & method.first=="interp") {
    message("Turning pupil size with blinks to NA")
    blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation
    
    message("Performing cubic interpolation")
    
    pupil_interp <- smooth_pupil %>% dplyr::group_by(subject, trial) %>%
      dplyr::mutate(index = ifelse(is.na(pup), NA, dplyr::row_number()),index=zoo::na.approx(index, na.rm=FALSE, rule=2),
                    interp = zoo::na.spline(pup, na.rm=FALSE, x=index, maxgap=maxgap)) %>%
      dplyr::ungroup()
    
    message("Smoothing the pupil trace with moving average")
    
    smooth_pupil <- as.data.frame(pupil_interp) %>%
      mutate(movingavgpup =moving_average_pupil(interp, n = n))
    
    return(smooth_pupil)
  }
  
  if (extendblinks==TRUE & type=="cubic" & method.first=="interp") {
    
    
    message("Performing cubic interpolation")
    pupil_interp <- datafile %>% dplyr::group_by(subject, trial) %>%
      dplyr::mutate(
        index = ifelse(is.na(extendpupil), NA, dplyr::row_number()),
        index= zoo::na.approx(index, na.rm=FALSE),
        interp = zoo::na.spline(extendpupil, na.rm=FALSE, x=index, maxgap=maxgap)) %>%
      dplyr::ungroup()
    
    message("Smoothing the pupil trace with moving average")
    
    smooth_pupil <- as.data.frame(pupil_interp) %>%
      mutate(movingavgpup = moving_average_pupil(interp, n = n))
  }
}