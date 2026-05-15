#' Turns pupil sizes with blinks NA and linearly interpolates values
#' @param datafile data frame.
#' @param extendblinks blinks already extended  in data frame 
#' @param type type of interpolation (linear or cubic)
#' @param maxgap max number of NAs to interpolate. 
#' @param hz recording frequency of ET
#' @export
#'
#' @return data frame containing interpolated data 
#' 
interpolate_pupil<-function(datafile, pupil, subject, trial, extendblinks=FALSE, maxgap=Inf, type=c("linear", "cubic"), hz=NA) {
#supports linear and cublic-spline interpolation
  if (maxgap!=Inf){
    maxgap <- round(maxgap/(1000/hz))
  }
  
  if (extendblinks==FALSE & type=="linear") {
message("Turning pupil size with blinks to NA")
  blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation
  message("Performing linear interpolation")
  pupil_interp <- blinks_na %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::mutate(interp = zoo::na.approx(pup, rule=2)) %>% 
    dplyr::ungroup() 
    return(pupil_interp)
  }
  
  if (extendblinks==TRUE & type=="linear") {
message("Performing linear interpolation")
pupil_interp <- datafile %>%
  dplyr::group_by(subject, trial) %>%
  dplyr::mutate(interp = zoo::na.approx(extendpupil, na.rm = FALSE, rule=2)) %>% 
  dplyr::ungroup() 
return(pupil_interp)

  }
  
  if (extendblinks==FALSE & type=="cubic") { 
    message("Turning pupil size with blinks to NA")
    blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation
    message("Performing cubic interpolation")
    pupil_interp <- blinks_na %>% dplyr::group_by(subject, trial) %>% 
      dplyr::mutate(index = ifelse(is.na(pup), NA, dplyr::row_number()),index=zoo::na.approx(index, na.rm=FALSE, rule=2), 
                    interp = zoo::na.spline(pup, na.rm=FALSE, x=index, maxgap=maxgap)) %>% 
    dplyr::ungroup()
  
    return(pupil_interp)
  }
  
  if (extendblinks==TRUE & type=="cubic") { 
    message("Performing cubic interpolation")
    pupil_interp <- datafile %>% dplyr::group_by(subject, trial) %>% 
      dplyr::mutate( 
                    index = ifelse(is.na(extendpupil), NA, dplyr::row_number()),             
                    index= zoo::na.approx(index, na.rm=FALSE), 
                    interp = zoo::na.spline(extendpupil, na.rm=FALSE, x=index, maxgap=maxgap)) %>%
    dplyr::ungroup()
      
    return(pupil_interp)
  }
    
    }

  
