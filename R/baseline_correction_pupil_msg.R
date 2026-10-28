
#' Performs linear (sub) or divisive baseline correction relative to desired stim_onset and off_set with messages.
#' @param datafile raw pupil data
#' @param pupil_colname name of your pupil colname you want baseline corrected
#' @param baseline_dur duration of baseline period from stim offset
#' @param event message name for stim onset
#' @param baseline_method correction method. Default is sub but can also include divisive
#' @param baseline_stat statistic for the baseline value: "median" (default) or "mean".
#' @return data frame containing baseline corrected data from event of interest
#' @export
baseline_correction_pupil_msg<-function(datafile, pupil_colname=NULL, baseline_dur=500, event="event", baseline_method="sub", baseline_stat="median")
{
  baseline_stat <- match.arg(baseline_stat, c("median", "mean"))
  baseline_fun <- if (baseline_stat == "mean") mean else stats::median
  if (baseline_method=="sub") {
    message("Calculating ", baseline_stat, " baseline from",":", event)

    event_time <- datafile %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::filter(message==!!event) %>%
      dplyr::summarise(event_offset_time=time[!is.na(message)]) %>%
      dplyr::full_join(., datafile) %>%
      dplyr::ungroup()

    baseline <- event_time %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::filter(time >= event_offset_time - baseline_dur,
                    time <= event_offset_time) %>%
      dplyr::rename(pupil_avg = pupil_colname) %>%
      dplyr::summarise(baseline = baseline_fun(pupil_avg, na.rm=TRUE)) %>%
      dplyr::full_join(., datafile) %>%
      dplyr::ungroup()


    message("Merging baseline")

    message("Performing subtractive baseline correction")

    corrected_baseline <- baseline %>%
      dplyr::rename(pupil_avg = pupil_colname) %>%
      dplyr::mutate(baselinecorrectedp = pupil_avg - baseline) %>%
      dplyr::rename(pup_interp = pupil_avg) %>%
      dplyr::arrange(subject, trial, time)
  }
#use grand average baseline instead of individual trial baseline to minimize bias
  if (baseline_method=="div") {
    message("Calculating ", baseline_stat, " baseline from",":", event)

    event_time <- datafile %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::filter(message==!!event) %>%
      dplyr::summarise(event_offset_time=time[!is.na(message)]) %>%
      dplyr::full_join(., datafile) %>%
      dplyr::ungroup()

    baseline <- event_time %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::filter(time >= event_offset_time - baseline_dur,
                    time <= event_offset_time) %>%
      dplyr::rename(pupil_avg = pupil_colname) %>%
      dplyr::summarise(baseline = baseline_fun(pupil_avg, na.rm=TRUE)) %>%
      dplyr::full_join(., datafile)%>%
      dplyr::ungroup()

    message("Merging baseline")
    message("Performing median divisive baseline correction")
    corrected_baseline <- baseline %>%
      dplyr::rename(pupil_avg = pupil_colname) %>%
      dplyr::mutate(baselinecorrectedp = (pupil_avg - baseline)/baseline) %>%
      dplyr::rename(pup_interp = pupil_avg) %>%
      dplyr::arrange(subject, trial, time)

  }

  return(corrected_baseline)
}
