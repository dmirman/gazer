#' Calculate percent missing data by subject and by trial
#' @param datafile data frame with subject, trial and pupil columns
#' @param missingthresh user-specified threshold for missing data. set to .2 
#' @param pupil name of pupil column
#' @export
#' @return data frame containing missing data information
#' @export

count_missing_pupil<- function(datafile, pupil="pupil", missingthresh=.2) {
  
  countsbysubject <- datafile %>%
    dplyr::group_by(subject) %>%
    dplyr::summarise(missing = sum(is.na(!!sym(pupil))),
                     samples = sum(!is.na(!!sym(pupil))),
                     total = length(pupil)) %>%
    dplyr::mutate(averageMissingSub = missing / total)
  
  countsbytrial <- datafile %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::summarise(missing = sum(is.na(!!sym(pupil))),
                     samples = sum(!is.na(!!sym(pupil))),
                     total = length(pupil))%>%
    dplyr::mutate(averageMissingTrial = missing / total)
  
  greaterthan <- dplyr::filter(countsbytrial, averageMissingTrial > missingthresh)
  prop <- length(greaterthan$trial)/length(countsbytrial$trial)
  
  # % trials excluded
  message("% trials excluded:",  round(prop, digits=3))
  message("Participants taken out:", list(countsbysubject$subject[countsbysubject$averageMissingSub> missingthresh]))
  # print # subjects excluded
  
  # add missing data per subject to the data frame
  combineSub <- dplyr::full_join(datafile, countsbysubject[, c("subject", "averageMissingSub")], by="subject")
  # add missing data per trial to the data frame
  combinetrial <- dplyr::full_join(combineSub, countsbytrial[, c("subject", "trial", "averageMissingTrial")], by=c("subject", "trial"))
  # keep only the data that exceeds the threshold for retaining the trials & subjects
  combinetrial_above_threshold <- dplyr::filter(combinetrial, (averageMissingSub < missingthresh) & (averageMissingTrial < missingthresh))
  
  return(combinetrial_above_threshold)
}


