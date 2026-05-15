#' Smooths the pupil trace before linear or cubic interpolation.
#' @param datafile data frame.
#' @param pupil name of pupil column
#' @param extendpupil name of pupil col that was extended
#' @param extendblinks blinks already extended  in data frame
#' @param step.first whether you want to smooth/interpolate or interpolate/smooth
#' @param filter use either moving and hann filter
#' @param type type of interpolation (linear or cubic)
#' @param maxgap max number of NAs to interpolate.
#' @param hz recording frequency of ET
#' @param n moving average window
#' @export
#'
#' @return df containing interpolated data
#'
smooth_interpolate_pupil<-function(datafile, pupil="pupil", extendpupil="extendpupil", extendblinks=FALSE, step.first="smooth", filter="moving", maxgap=Inf, type=NULL, hz=NA, n=NA) {
  #supports linear and cublic-spline interpolation
  if (maxgap!=Inf){
    maxgap <- round(maxgap/(1000/hz))
  }

  if (extendblinks==FALSE & type=="linear" & step.first=="smooth" & filter=="moving") {

    message("Turning pupil size with blinks to NA")

    blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation

    message("Smoothing the pupil trace with moving average")

    smooth_pupil <- as.data.frame(blinks_na) %>%
      mutate(movingavgpup =moving_average_pupil(pup, n = n))

    message("Performing linear interpolation")

    pupil_interp <- smooth_pupil %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(pup_interp = zoo::na.approx(movingavgpup, rule=2)) %>%
      dplyr::ungroup()%>%
      dplyr::select(-movingavgpup)
    return(pupil_interp)
  }

  if (extendblinks==TRUE & type=="linear" & step.first=="smooth" & filter=="moving") {

    message("smoothing the pupil trace with moving average")

    smooth_pupil <- as.data.frame(datafile) %>%
      mutate(movingavgpup = moving_average_pupil(extendpupil, n = n))


    message("Performing linear interpolation")
    pupil_interp <- smooth_pupil %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(pup_interp = zoo::na.approx(movingavgpup, na.rm = FALSE, rule=2)) %>%
      dplyr::ungroup()%>%
      dplyr::select(-movingavgpup)
    return(pupil_interp)

  }

  if (extendblinks==FALSE & type=="cubic" & step.first=="smooth" & filter=="moving") {
    message("Turning pupil size with blinks to NA")
    blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation

    message("Smoothing the pupil trace with moving average")

    smooth_pupil <- as.data.frame(blinks_na) %>%
      mutate(movingavgpup =moving_average_pupil(pup, n = n))

    message("Performing cubic interpolation")
    warning("Due to extreme values from spline interpolation if NAs at onset and offset last valid pupil sizes are extrapolated")

    pupil_interp <- smooth_pupil %>% dplyr::group_by(subject, trial) %>%
      dplyr::mutate(index = ifelse(is.na(movingavgpup), NA, dplyr::row_number()),
                    index = zoo::na.approx(index, na.rm=FALSE, rule=2),
                    pup_interp = zoo::na.spline(movingavgpup, na.rm=FALSE, x=index, maxgap=maxgap),
                    pup_interp = zoo::na.locf(interp, na.rm = TRUE, fromLast=TRUE),
                    pup_interp = zoo::na.locf(interp, na.rm = TRUE)) %>%
      dplyr::ungroup()%>%
      dplyr::select(-movingavgpup)

    return(pupil_interp)
  }

  if (extendblinks==TRUE & type=="cubic" & step.first=="smooth" & filter=="moving") {

    message("Smoothing the pupil trace with moving average")

    smooth_pupil <- as.data.frame(datafile) %>%
      mutate(movingavgpup = moving_average_pupil(extendpupil, n = n))

    message("Performing cubic interpolation")
    warning("Due to extreme values from spline interpolation if NAs at onset and offset last valid pupil sizes are extrapolated")

    pupil_interp <- smooth_pupil  %>% dplyr::group_by(subject, trial) %>%
      dplyr::mutate(index = ifelse(is.na(movingavgpup), NA, dplyr::row_number()),
                    index =  zoo::na.approx(index, na.rm=FALSE, rule=2),
                    pup_interp = zoo::na.spline(movingavgpup, na.rm=FALSE, x=index, maxgap=maxgap),
                    pup_interp = zoo::na.locf(pup_interp, na.rm = TRUE, fromLast=TRUE),
                    pup_interp = zoo::na.locf(pup_interp, na.rm = TRUE)) %>%
      dplyr::ungroup()%>%
      dplyr::select(-movingavgpup)

    return(pupil_interp)
  }

  if (extendblinks==FALSE & type=="linear" & step.first=="interp" & filter=="moving") {

    message("Turning pupil size with blinks to NA")

    blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation

    message("Performing linear interpolation")

    pupil_interp <- blinks_na %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(interp = zoo::na.approx(extendpupil, rule=2)) %>%
      dplyr::ungroup()

    message("Smoothing the pupil trace with moving average")

    smooth_pupil <- as.data.frame(pupil_interp) %>%
      dplyr::mutate(pup_interp = moving_average_pupil(interp, n = n)) %>%

      return(smooth_pupil)
  }


  if (extendblinks==TRUE & type=="linear" & step.first=="interp" & filter=="moving") {


    message("Performing linear interpolation")

    pupil_interp <- datafile %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(interp = zoo::na.approx(extendpupil, na.rm = FALSE, rule=2)) %>%
      dplyr::ungroup()

    message("Smoothing the pupil trace with moving average")

    smooth_pupil <- as.data.frame(pupil_interp) %>%
      dplyr::mutate(pup_interp = moving_average_pupil(interp, n = n))

    return(smooth_pupil)
  }


  if (extendblinks==FALSE & type=="cubic" & step.first=="interp" & filter=="moving") {
    message("Turning pupil size with blinks to NA")
    blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation

    message("Performing cubic interpolation")
    warning("Due to extreme values from spline interpolation if NAs at onset and offset last valid pupil sizes are extrapolated")


    pupil_interp <- blinks_na %>% dplyr::group_by(subject, trial) %>%
      dplyr::mutate(index = ifelse(is.na(pup), NA, dplyr::row_number()),
                    index=zoo::na.approx(index, na.rm=FALSE, rule=2),
                    interp = zoo::na.spline(pup, na.rm=FALSE, x=index, maxgap=maxgap),
                    interp = zoo::na.locf(interp, na.rm = TRUE, fromLast=TRUE),
                    interp = zoo::na.locf(interp, na.rm = TRUE))%>%
      dplyr::ungroup()%>%
      dplyr::select(-movingavgpup)

    message("Smoothing the pupil trace with moving average")

    smooth_pupil <- as.data.frame(pupil_interp) %>%
      dplyr::mutate(pup_interp =moving_average_pupil(interp, n = n))

    return(smooth_pupil)
  }

  if (extendblinks==TRUE & type=="cubic" & step.first=="interp" & filter=="moving") {


    message("Performing cubic interpolation")
    warning("Due to extreme values from spline interpolation if NAs at onset and offset last valid pupil sizes are extrapolated")


    pupil_interp <- datafile %>% dplyr::group_by(subject, trial) %>%
      dplyr::mutate(index = ifelse(is.na(extendpupil), NA, dplyr::row_number()),
                    index =  zoo::na.approx(index, na.rm=FALSE, rule=2),
                    interp = zoo::na.spline(extendpupil, na.rm=FALSE, x=index, maxgap=maxgap),
                    interp = zoo::na.locf(interp, na.rm = TRUE, fromLast=TRUE),
                    interp = zoo::na.locf(interp, na.rm = TRUE)) %>%
      dplyr::ungroup()

    message("Smoothing the pupil trace with moving average")

    smooth_pupil <- as.data.frame(pupil_interp) %>%
      dplyr::mutate(pup_interp = moving_average_pupil(interp, n = n))

    return(smooth_pupil)
  }

  if (extendblinks==FALSE & type=="linear" & step.first=="smooth" & filter=="moving") {

    message("Turning pupil size with blinks to NA")

    blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation

    message("Smoothing the pupil trace with moving average")

    smooth_pupil <- as.data.frame(blinks_na) %>%
      mutate(movingavgpup =moving_average_pupil(pup, n = n))

    message("Performing linear interpolation")

    pupil_interp <- smooth_pupil %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(pup_interp = zoo::na.approx(movingavgpup, rule=2)) %>%
      dplyr::ungroup()%>%
      dplyr::select(-movingavgpup)
    return(pupil_interp)
  }

  if (extendblinks==TRUE & type=="linear" & step.first=="smooth" & filter=="Hann") {

    message("Smoothing the pupil trace with Hann window")

    smooth_pupil <- as.data.frame(datafile) %>%
      mutate(movingavgpup = hanning_filter(extendpupil, degree=11))


    message("Performing linear interpolation")
    pupil_interp <- smooth_pupil %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(pup_interp = zoo::na.approx(movingavgpup, na.rm = FALSE, rule=2)) %>%
      dplyr::ungroup()%>%
      dplyr::select(-movingavgpup)
    return(pupil_interp)

  }

  if (extendblinks==FALSE & type=="cubic" & step.first=="smooth" & filter=="Hann") {
    message("Turning pupil size with blinks to NA")
    blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation

    message("Smoothing the pupil trace with Hann window")

    smooth_pupil <- as.data.frame(blinks_na) %>%
      mutate(movingavgpup = hanning_filter(pup, degree=11))

    message("Performing cubic interpolation")
    warning("Due to extreme values from spline interpolation if NAs at onset and offset last valid pupil sizes are extrapolated")

    pupil_interp <- smooth_pupil %>% dplyr::group_by(subject, trial) %>%
      dplyr::mutate(index = ifelse(is.na(movingavgpup), NA, dplyr::row_number()),
                    index = zoo::na.approx(index, na.rm=FALSE, rule=2),
                    pup_interp = zoo::na.spline(movingavgpup, na.rm=FALSE, x=index, maxgap=maxgap),
                    pup_interp = zoo::na.locf(interp, na.rm = TRUE, fromLast=TRUE),
                    pup_interp = zoo::na.locf(interp, na.rm = TRUE)) %>%
      dplyr::ungroup()%>%
      dplyr::select(-movingavgpup)

    return(pupil_interp)
  }

  if (extendblinks==TRUE & type=="cubic" & step.first=="smooth" & filter=="Hann") {

    message("Smoothing the pupil trace with Hann window")

    smooth_pupil <- as.data.frame(datafile) %>%
      mutate(movingavgpup = hanning_filter(extendpupil, degree=11))

    message("Performing cubic interpolation")
    warning("Due to extreme values from spline interpolation if NAs at onset and offset last valid pupil sizes are extrapolated")
    pupil_interp <- smooth_pupil  %>% dplyr::group_by(subject, trial) %>%
      dplyr::mutate(index = ifelse(is.na(movingavgpup), NA, dplyr::row_number()),
                    index =  zoo::na.approx(index, na.rm=FALSE, rule=2),
                    pup_interp = zoo::na.spline(movingavgpup, na.rm=FALSE, x=index, maxgap=maxgap),
                    pup_interp = zoo::na.locf(pup_interp, na.rm = TRUE, fromLast=TRUE),
                    pup_interp = zoo::na.locf(pup_interp, na.rm = TRUE)) %>%
      dplyr::ungroup()%>%
      dplyr::select(-movingavgpup)

    return(pupil_interp)
  }

  if (extendblinks==FALSE & type=="linear" & step.first=="interp" & filter=="Hann") {

    message("Turning pupil size with blinks to NA")

    blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation

    message("Performing linear interpolation")

    pupil_interp <- blinks_na %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(interp = zoo::na.approx(extendpupil, rule=2)) %>%
      dplyr::ungroup()

    message("Smoothing the pupil trace with Hann window")

    smooth_pupil <- as.data.frame(pupil_interp) %>%
      dplyr::mutate(pup_interp = hanning_filter(interp, degree=11)) %>%

      return(smooth_pupil)
  }


  if (extendblinks==TRUE & type=="linear" & step.first=="interp" & filter=="Hann") {


    message("Performing linear interpolation")

    pupil_interp <- datafile %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(interp = zoo::na.approx(extendpupil, na.rm = FALSE, rule=2)) %>%
      dplyr::ungroup()

    message("Smoothing the pupil trace with Hann window")

    smooth_pupil <- as.data.frame(pupil_interp) %>%
      dplyr::mutate(pup_interp = hanning_filter(interp, degree=11))

    return(smooth_pupil)
  }


  if (extendblinks==FALSE & type=="cubic" & step.first=="interp" & filter=="Hann") {
    message("Turning pupil size with blinks to NA")
    blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation

    message("Performing cubic interpolation")

    pupil_interp <- blinks_na %>% dplyr::group_by(subject, trial) %>%
      dplyr::mutate(index = ifelse(is.na(pup), NA, dplyr::row_number()),
                    index=zoo::na.approx(index, na.rm=FALSE, rule=2),
                    interp = zoo::na.spline(pup, na.rm=FALSE, x=index, maxgap=maxgap),
                    interp = zoo::na.locf(interp, na.rm = TRUE, fromLast=TRUE),
                    interp = zoo::na.locf(interp, na.rm = TRUE))%>%
      dplyr::ungroup()%>%
      dplyr::select(-movingavgpup)

    message("Smoothing the pupil trace with Hann window")

    smooth_pupil <- as.data.frame(pupil_interp) %>%
      dplyr::mutate(pup_interp =hanning_filter(interp, degree=11))

    return(smooth_pupil)
  }

  if (extendblinks==TRUE & type=="cubic" & step.first=="interp" & filter=="Hann") {


    message("Performing cubic interpolation")
    warning("Due to extreme values from spline interpolation if NAs at onset and offset last valid pupil sizes are extrapolated")


    pupil_interp <- datafile %>% dplyr::group_by(subject, trial) %>%
      dplyr::mutate(index = ifelse(is.na(extendpupil), NA, dplyr::row_number()),
                    index =  zoo::na.approx(index, na.rm=FALSE, rule=2),
                    interp = zoo::na.spline(extendpupil, na.rm=FALSE, x=index, maxgap=maxgap),
                    interp = zoo::na.locf(interp, na.rm = TRUE, fromLast=TRUE),
                    interp = zoo::na.locf(interp, na.rm = TRUE)) %>%
      dplyr::ungroup()

    message("Smoothing the pupil trace with Hann window")

    smooth_pupil <- as.data.frame(pupil_interp) %>%
      dplyr::mutate(pup_interp = hanning_filter(extendpupil, degree=11))

    return(smooth_pupil)
  }

}
